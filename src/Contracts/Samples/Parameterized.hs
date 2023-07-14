-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Contracts.Samples.Parameterized where

import Data.Default (def)
import qualified Data.Map.Strict as Map
import Jambhala.Plutus
import Jambhala.Utils

data VestingParam = VestingParam
  { getBeneficiary :: PaymentPubKeyHash,
    getMaturity :: POSIXTime
  }

makeLift ''VestingParam

parameterized :: VestingParam -> () -> () -> ScriptContext -> Bool
parameterized (VestingParam ben maturity) _ _ sc =
  traceIfFalse "Wrong pubkey hash" signedByBeneficiary
    && traceIfFalse "Maturity not reached" maturityReached
  where
    txInfo = scriptContextTxInfo sc
    signedByBeneficiary = txSignedBy txInfo $ unPaymentPubKeyHash ben
    maturityReached = contains (from maturity) $ txInfoValidRange txInfo
{-# INLINEABLE parameterized #-}

validator :: VestingParam -> Validator
validator p = mkValidatorScript $ $$(compile [||untyped||]) `applyCode` liftCode p
  where
    untyped = mkUntypedValidator . parameterized

data ParamVesting = THIS
  deriving (ValidatorTypes) -- if Datum and Redeemer are both () we can derive this

instance Emulatable ParamVesting where
  data GiveParam ParamVesting = Give
    { lovelace :: Integer,
      forBeneficiary :: PaymentPubKeyHash,
      afterMaturity :: !POSIXTime
    }
    deriving (Generic, ToJSON, FromJSON)
  data GrabParam ParamVesting = Grab {withMaturity :: POSIXTime}
    deriving (Generic, ToJSON, FromJSON)

  give :: GiveParam ParamVesting -> ContractM ParamVesting ()
  give Give {..} = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor THIS validator',
          constraints = mustPayToScriptWithDatum validator' () lovelace
        }
    logStr $
      printf
        "Made a gift of %d lovelace to %s with deadline %s"
        lovelace
        (show forBeneficiary)
        (show afterMaturity)
    where
      vParam = VestingParam {getBeneficiary = forBeneficiary, getMaturity = afterMaturity}
      validator' = validator vParam

  -- lookups = lookupScript v
  -- constraints = mustPayToScriptWithDatum v () lovelace

  grab :: GrabParam ParamVesting -> ContractM ParamVesting ()
  grab (Grab dline) = do
    pkh <- ownFirstPaymentPubKeyHash
    now <- getCurrentInterval
    if from dline `contains` now
      then do
        let p = VestingParam {getBeneficiary = pkh, getMaturity = dline}
            validator' = validator p
        validUtxos <- getUtxosAt validator'
        if Map.null validUtxos
          then logStr "No eligible gifts available"
          else do
            let lookups = scriptLookupsFor THIS validator' `andUtxos` validUtxos
                constraints =
                  mconcat $
                    mustValidateInTimeRange (fromPlutusInterval now) :
                    mustBeSignedBy pkh :
                    map (`mustSpendScriptOutput` unitRedeemer) (Map.keys validUtxos)
            submitAndConfirm Tx {..}
            logStr "Collected eligible gifts"
      else logStr "Maturity not reached"

test :: EmulatorTest
test =
  initEmulator
    4
    [ Give
        { lovelace = 30_000_000,
          forBeneficiary = pkhForWallet 2,
          afterMaturity = dline
        }
        `fromWallet` 1,
      Give
        { lovelace = 30_000_000,
          forBeneficiary = pkhForWallet 4,
          afterMaturity = slotToBeginPOSIXTime def 20
        }
        `fromWallet` 1,
      Grab {withMaturity = dline} `toWallet` 2, -- deadline not reached
      waitUntil 20,
      Grab {withMaturity = dline} `toWallet` 3, -- wrong beneficiary
      Grab {withMaturity = dline} `toWallet` 4 -- collect gift
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
          getMaturity = slotToBeginPOSIXTime def 20
        }