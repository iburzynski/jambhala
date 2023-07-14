module Contracts.Samples.Vesting where

import Data.Default (def)
import qualified Data.Map as Map
import Jambhala.Plutus
import Jambhala.Utils

-- Datum
data VestingDatum = VestingDatum
  { getBeneficiary :: PaymentPubKeyHash,
    getMaturity :: POSIXTime
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

  give :: GiveAction Vesting
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
        (show $ getBeneficiary datum)
        (show $ getMaturity datum)

  grab :: GrabAction Vesting
  grab _ = do
    pkh <- ownFirstPaymentPubKeyHash
    now <- getCurrentInterval
    utxos <- getUtxosAt validator
    let validUtxos = Map.mapMaybe (isEligible pkh now) utxos
    if Map.null validUtxos
      then logStr "No eligible gifts available"
      else do
        let lookups = scriptLookupsFor THIS validator `andUtxos` validUtxos
            constraints =
              mconcat $
                mustValidateInTimeRange (fromPlutusInterval now) :
                mustBeSignedBy pkh :
                map (`mustSpendScriptOutput` unitRedeemer) (Map.keys validUtxos)
        submitAndConfirm Tx {..}
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
              { getBeneficiary = pkhForWallet 2,
                getMaturity = slotToBeginPOSIXTime def 20
              },
          lovelace = 30_000_000
        }
        `fromWallet` 1,
      Give
        { datum =
            VestingDatum
              { getBeneficiary = pkhForWallet 4,
                getMaturity = slotToBeginPOSIXTime def 20
              },
          lovelace = 30_000_000
        }
        `fromWallet` 1,
      Grab `toWallet` 2, -- deadline not reached
      waitUntil 20,
      Grab `toWallet` 3, -- wrong beneficiary
      Grab `toWallet` 4 -- collect gift
    ]

exports :: JambContract -- Prepare exports for jamb CLI:
exports = exportContract ("vesting" `withScript` validator) {emulatorTest = test}