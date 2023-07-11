{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required to map validator and endpoint types
{-# LANGUAGE TypeFamilies #-}

module Contracts.Samples.CustomTyped where

import qualified Data.Map.Strict as Map
import Jambhala.Plutus
import Jambhala.Utils

newtype Redeem = Redeem {guess :: Integer}

-- `unstableMakeIsData` uses TemplateHaskell to generate ToData/FromData instances for a custom type
-- These classes have toBuiltInData/fromBuiltInData methods to convert between Haskell/Plutus data.
unstableMakeIsData ''Redeem

-- makeIsDataIndexed ''Redeemer [('Redeemer, 0)]
-- (requires importing `makeIsDataIndexed` from Plutus.Tx)

customTyped :: () -> Redeem -> ScriptContext -> Bool
customTyped _ (Redeem i) _ = traceIfFalse "Sorry, wrong guess!" (i #== 42)
{-# INLINEABLE customTyped #-}

validator :: Validator
validator = mkValidatorScript $$(compile [||wrapped||])
  where
    wrapped = mkUntypedValidator customTyped

-- PART II: OFF-CHAIN EMULATION

-- 1. Define a data type for the contract (no constructors needed)
data CustomTyped

-- 2. Map the validator types using associated type families
instance ValidatorTypes CustomTyped where
  type DatumType CustomTyped = ()
  type RedeemerType CustomTyped = Redeem

-- 3. Make the contract emulatable with Emulatable instance
instance Emulatable CustomTyped where
  -- Define associated data types for Give and Grab parameters
  data GiveParam CustomTyped = Give {lovelace :: Integer}
    deriving (Generic, FromJSON, ToJSON) -- parameter values must be convertible to/from JSON
  data GrabParam CustomTyped = Grab {withGuess :: Integer}
    deriving (Generic, FromJSON, ToJSON)

  -- 4. Define give endpoint action: send UTXOs to the script address
  give :: GiveAction CustomTyped
  give (Give q) = do
    logInfo @String $ printf "Start of the give action"
    submittedTxId <- getCardanoTxId <$> submitTxConstraintsWith @CustomTyped lookups constraints
    awaitTxConfirmed submittedTxId
    logInfo @String $ printf "Made transaction of %d ADA" q
    where
      vHash = validatorHash validator
      datum = Datum $ toBuiltinData ()
      lookups = plutusV2OtherScript validator
      constraints = mustPayToOtherScriptWithDatumInTx vHash datum $ lovelaceValueOf q

  -- 5. Define grab endpoint action: consume UTXOs at the script address
  grab :: GrabAction CustomTyped
  grab (Grab 42) = do
    addr <- getContractAddress validator
    utxos <- utxosAt addr
    let lookups = unspentOutputs utxos <> plutusV2OtherScript validator
        orefs = Map.keys utxos
        redeemer = Redeemer . toBuiltinData $ Redeem 42
        constraints = mconcat $ map (`mustSpendScriptOutput` redeemer) orefs
    submittedTxId <- getCardanoTxId <$> submitTxConstraintsWith @CustomTyped lookups constraints
    awaitTxConfirmed submittedTxId
    logString "collected gifts"
  grab _ = logString "Wrong guess"

-- 6. Define emulator test
test :: EmulatorTest
test =
  initEmulator
    3 -- specify the number of wallets involved in the test
    -- list the actions involved in the test
    [ Give {lovelace = 42_000_000} `fromWallet` 1,
      Grab {withGuess = 21} `toWallet` 2, -- wrong guess
      Grab {withGuess = 42} `toWallet` 3 -- right guess
    ]

-- Exports
redeemerSuccess :: DataExport
redeemerSuccess = DataExport "ctr42" $ Redeem 42

redeemerFail :: DataExport
redeemerFail = DataExport "ctr21" $ Redeem 21

exports :: JambContract -- Prepare exports for jamb CLI:
exports =
  exportContract
    ("custom-typed" `withScript` validator)
      { dataExports = [redeemerSuccess, redeemerFail],
        emulatorTest = test
      }