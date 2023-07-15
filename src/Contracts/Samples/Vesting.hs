module Contracts.Samples.Vesting where

import qualified Data.Map as Map
import Jambhala.Plutus
import Jambhala.Utils

-- Datum
data VestingDatum = VestingDatum
  { toBeneficiary :: PaymentPubKeyHash,
    afterMaturity :: POSIXTime
  }
  deriving (Generic, ToJSON, FromJSON)

unstableMakeIsData ''VestingDatum

vesting :: VestingDatum -> () -> ScriptContext -> Bool
vesting (VestingDatum ben maturity) _ sc =
  traceIfFalse "Beneficiary's signature missing" signedByBeneficiary
    && traceIfFalse "Maturity not reached" maturityReached
  where
    txInfo = scriptContextTxInfo sc
    signedByBeneficiary = txSignedBy txInfo $ unPaymentPubKeyHash ben
    maturityReached = contains (from maturity) $ txInfoValidRange txInfo
{-# INLINEABLE vesting #-}

validator :: Validator
validator = mkValidatorScript $$(compile [||wrapped||])
  where
    wrapped = mkUntypedValidator vesting

data Vesting = THIS

instance ValidatorTypes Vesting where
  type DatumType Vesting = VestingDatum

instance Emulatable Vesting where
  data GiveParam Vesting = Give
    { datum :: VestingDatum,
      lovelace :: Integer
    }
    deriving (Generic, ToJSON, FromJSON)
  data GrabParam Vesting = Grab
    deriving (Generic, ToJSON, FromJSON)

  give :: GiveParam Vesting -> ContractM Vesting ()
  give Give {..} = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor THIS validator,
          constraints = mustPayToScriptWithDatum validator datum lovelace
        }
    logStr $
      printf
        "Made a gift of %d lovelace to %s with deadline %s"
        lovelace
        (show $ toBeneficiary datum)
        (show $ afterMaturity datum)

  grab :: GrabParam Vesting -> ContractM Vesting ()
  grab _ = do
    pkh <- ownFirstPaymentPubKeyHash
    now <- getCurrentInterval
    utxos <- getUtxosAt validator
    let validUtxos = Map.mapMaybe (isEligible pkh now) utxos
    if validUtxos == mempty
      then logStr "No eligible gifts available"
      else do
        submitAndConfirm
          Tx
            { lookups = scriptLookupsFor THIS validator `andUtxos` validUtxos,
              constraints =
                mconcat
                  [ mustValidateInTimeRange (fromPlutusInterval now),
                    mustBeSignedBy pkh,
                    validUtxos `mustAllBeSpentWith` ()
                  ]
            }
        logStr "Collected eligible gifts"
    where
      isEligible :: PaymentPubKeyHash -> Interval POSIXTime -> DecoratedTxOut -> Maybe DecoratedTxOut
      isEligible pkh now dto = do
        (_, dfq) <- getDecoratedTxOutDatum dto
        Datum d <- getDatumInDatumFromQuery dfq
        VestingDatum ben maturity <- fromBuiltinData d
        guard (ben == pkh && from maturity `contains` now)
        Just dto

test :: EmulatorTest
test =
  initEmulator
    4
    [ Give
        { datum =
            VestingDatum
              { toBeneficiary = pkhForWallet 2,
                afterMaturity = m
              },
          lovelace = 30_000_000
        }
        `fromWallet` 1,
      Give
        { datum =
            VestingDatum
              { toBeneficiary = pkhForWallet 4,
                afterMaturity = m
              },
          lovelace = 30_000_000
        }
        `fromWallet` 1,
      Grab `toWallet` 2, -- deadline not reached
      waitUntil 20,
      Grab `toWallet` 3, -- wrong beneficiary
      Grab `toWallet` 4 -- collect gift
    ]
  where
    m :: POSIXTime
    m = defaultSlotBeginTime 20

exports :: JambContract -- Prepare exports for jamb CLI:
exports = exportContract ("vesting" `withScript` validator) {emulatorTest = test}