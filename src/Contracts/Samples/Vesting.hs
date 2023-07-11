module Contracts.Samples.Vesting where

import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import Jambhala.Plutus
import Jambhala.Utils

-- Datum
data VestingDatum = VestingDatum
  { getBeneficiary :: PaymentPubKeyHash,
    getDeadline :: POSIXTime
  }
  deriving (Generic, ToJSON, FromJSON)

unstableMakeIsData ''VestingDatum

vesting :: VestingDatum -> () -> ScriptContext -> Bool
vesting (VestingDatum ben dline) _ sc =
  traceIfFalse "Beneficiary's signature missing" signedByBeneficiary
    && traceIfFalse "Deadline not reached" dlineReached
  where
    txInfo = scriptContextTxInfo sc
    signedByBeneficiary = txSignedBy txInfo $ unPaymentPubKeyHash ben
    dlineReached = contains (from dline) $ txInfoValidRange txInfo
{-# INLINEABLE vesting #-}

validator :: Validator
validator = mkValidatorScript $$(compile [||wrapped||])
  where
    wrapped = mkUntypedValidator vesting

data Vesting

instance ValidatorTypes Vesting where
  type DatumType Vesting = VestingDatum

instance Emulatable Vesting where
  data GiveParam Vesting = Give
    { gpDatum :: VestingDatum,
      gpAmount :: Integer
    }
    deriving (Generic, ToJSON, FromJSON)
  data GrabParam Vesting = Grab
    deriving (Generic, ToJSON, FromJSON)

  give :: GiveAction Vesting
  give Give {..} = do
    submittedTxId <- getCardanoTxId <$> submitTxConstraintsWith @Vesting lookups constraints
    _ <- awaitTxConfirmed submittedTxId
    logString $
      printf
        "Made a gift of %d lovelace to %s with deadline %s"
        gpAmount
        (show $ getBeneficiary gpDatum)
        (show $ getDeadline gpDatum)
    where
      vHash = validatorHash validator
      lookups = plutusV2OtherScript validator
      constraints = mustPayToOtherScriptWithDatumInTx vHash (Datum . toBuiltinData $ gpDatum) $ lovelaceValueOf gpAmount

  grab :: GrabAction Vesting
  grab _ = do
    pkh <- ownFirstPaymentPubKeyHash
    now <- uncurry interval <$> currentNodeClientTimeRange
    addr <- getContractAddress validator
    validUtxos <- Map.mapMaybe (isEligible pkh now) <$> utxosAt addr
    if Map.null validUtxos
      then logString "No eligible gifts available"
      else do
        let lookups = unspentOutputs validUtxos <> plutusV2OtherScript validator
            constraints =
              mconcat $
                mustValidateInTimeRange (fromPlutusInterval now) :
                mustBeSignedBy pkh :
                map (`mustSpendScriptOutput` unitRedeemer) (Map.keys validUtxos)
        submittedTx <- getCardanoTxId <$> submitTxConstraintsWith @Vesting lookups constraints
        _ <- awaitTxConfirmed submittedTx
        logString "Collected eligible gifts"
    where
      isEligible :: PaymentPubKeyHash -> Interval POSIXTime -> DecoratedTxOut -> Maybe DecoratedTxOut
      isEligible pkh now dto = do
        (_, dfq) <- getDecoratedTxOutDatum dto
        Datum d <- getDatumInDatumFromQuery dfq
        VestingDatum ben dline <- fromBuiltinData d
        guard (ben == pkh && from dline `contains` now)
        Just dto

test :: EmulatorTest
test =
  initEmulator @Vesting
    4
    [ Give
        { gpDatum =
            VestingDatum
              { getBeneficiary = pkhForWallet 2,
                getDeadline = slotToBeginPOSIXTime def 20
              },
          gpAmount = 30_000_000
        }
        `fromWallet` 1,
      Give
        { gpDatum =
            VestingDatum
              { getBeneficiary = pkhForWallet 4,
                getDeadline = slotToBeginPOSIXTime def 20
              },
          gpAmount = 30_000_000
        }
        `fromWallet` 1,
      Grab `toWallet` 2, -- deadline not reached
      waitUntil 20,
      Grab `toWallet` 3, -- wrong beneficiary
      Grab `toWallet` 4 -- collect gift
    ]

exports :: JambContract -- Prepare exports for jamb CLI:
exports = exportValidatorWithTest "vesting" validator [] test