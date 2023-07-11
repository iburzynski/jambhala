-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Contracts.Samples.Parameterized where

import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import Jambhala.Plutus
import Jambhala.Utils

-- import Prelude hiding (Applicative (..), Eq (..), Semigroup (..), Traversable (..), mconcat)

data VestingParam = VestingParam
  { getBeneficiary :: PaymentPubKeyHash,
    getDeadline :: POSIXTime
  }

makeLift ''VestingParam

parameterized :: VestingParam -> () -> () -> ScriptContext -> Bool
parameterized (VestingParam ben dline) _ _ sc =
  traceIfFalse "Wrong pubkey hash" signedByBeneficiary
    && traceIfFalse "Deadline not reached" dlineReached
  where
    txInfo = scriptContextTxInfo sc
    signedByBeneficiary = txSignedBy txInfo $ unPaymentPubKeyHash ben
    dlineReached = contains (from dline) $ txInfoValidRange txInfo
{-# INLINEABLE parameterized #-}

validator :: VestingParam -> Validator
validator p = mkValidatorScript $ $$(compile [||wrapped||]) `applyCode` liftCode p
  where
    wrapped = mkUntypedValidator . parameterized

data ParamVesting deriving (ValidatorTypes) -- if Datum and Redeemer are both () we can derive this

instance Emulatable ParamVesting where
  data GiveParam ParamVesting = Give
    { lovelace :: Integer,
      forBeneficiary :: PaymentPubKeyHash,
      afterDeadline :: !POSIXTime
    }
    deriving (Generic, ToJSON, FromJSON)
  data GrabParam ParamVesting = Grab {withDeadline :: POSIXTime}
    deriving (Generic, ToJSON, FromJSON)

  give :: GiveAction ParamVesting
  give Give {..} = do
    submittedTxId <- getCardanoTxId <$> submitTxConstraintsWith @ParamVesting lookups constraints
    _ <- awaitTxConfirmed submittedTxId
    logInfo @String $
      printf
        "Made a gift of %d lovelace to %s with deadline %s"
        lovelace
        (show forBeneficiary)
        (show afterDeadline)
    where
      vParam = VestingParam {getBeneficiary = forBeneficiary, getDeadline = afterDeadline}
      v = validator vParam
      vHash = validatorHash v
      lookups = plutusV2OtherScript v
      constraints = mustPayToOtherScriptWithDatumInTx vHash unitDatum $ lovelaceValueOf lovelace

  grab :: GrabAction ParamVesting
  grab (Grab dline) = do
    pkh <- ownFirstPaymentPubKeyHash
    now <- uncurry interval <$> currentNodeClientTimeRange
    if from dline `contains` now
      then do
        let p = VestingParam {getBeneficiary = pkh, getDeadline = dline}
            v = validator p
        addr <- getContractAddress v
        validUtxos <- utxosAt addr
        if Map.null validUtxos
          then logInfo @String $ "No eligible gifts available"
          else do
            let lookups = unspentOutputs validUtxos <> plutusV2OtherScript v
                constraints =
                  mconcat $
                    mustValidateInTimeRange (fromPlutusInterval now) :
                    mustBeSignedBy pkh :
                    map (`mustSpendScriptOutput` unitRedeemer) (Map.keys validUtxos)
            submittedTx <- getCardanoTxId <$> submitTxConstraintsWith @ParamVesting lookups constraints
            _ <- awaitTxConfirmed submittedTx
            logInfo @String "Collected eligible gifts"
      else logInfo @String "Deadline not reached"

test :: EmulatorTest
test =
  initEmulator
    4
    [ Give
        { lovelace = 30_000_000,
          forBeneficiary = pkhForWallet 2,
          afterDeadline = dline
        }
        `fromWallet` 1,
      Give
        { lovelace = 30_000_000,
          forBeneficiary = pkhForWallet 4,
          afterDeadline = slotToBeginPOSIXTime def 20
        }
        `fromWallet` 1,
      Grab {withDeadline = dline} `toWallet` 2, -- deadline not reached
      waitUntil 20,
      Grab {withDeadline = dline} `toWallet` 3, -- wrong beneficiary
      Grab {withDeadline = dline} `toWallet` 4 -- collect gift
    ]
  where
    dline = slotToBeginPOSIXTime def 20

exports :: JambContract
exports = exportContract ("param-vesting" `withScript` validator p) {emulatorTest = test}
  where
    -- validator must be applied to some parameter to generate a hash or write script to file
    p =
      VestingParam
        { getBeneficiary = pkhForWallet 2,
          getDeadline = slotToBeginPOSIXTime def 20
        }