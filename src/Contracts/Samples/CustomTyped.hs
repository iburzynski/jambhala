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
data CTyped

-- 2. Map the validator types using associated type families
instance ValidatorTypes CTyped where
  type DatumType CTyped = ()
  type RedeemerType CTyped = Redeem

-- 3. Make the contract emulatable via another typeclass instance
instance Emulatable CTyped where
  -- Define associated data types for Give and Grab parameters
  data GiveParam CTyped = Give Integer
    deriving (Generic, FromJSON, ToJSON) -- parameter values must be convertible to/from JSON
  data GrabParam CTyped = Grab Integer
    deriving (Generic, FromJSON, ToJSON)

  -- 4. Define give endpoint action: send UTXOs to the script address
  give :: GiveAction CTyped
  give (Give q) = do
    logInfo @String $ printf "Start of the give action"
    submittedTxId <- getCardanoTxId <$> submitTxConstraintsWith @CTyped lookups constraints
    awaitTxConfirmed submittedTxId
    logInfo @String $ printf "Made transaction of %d ADA" q
    where
      vHash = validatorHash validator
      datum = Datum $ toBuiltinData ()
      lookups = plutusV2OtherScript validator
      constraints = mustPayToOtherScriptWithDatumInTx vHash datum $ lovelaceValueOf q

  -- 5. Define grab endpoint action: consume UTXOs at the script address
  grab :: GrabAction CTyped
  grab (Grab 42) = do
    addr <- getContractAddress validator
    utxos <- utxosAt addr
    let lookups = unspentOutputs utxos <> plutusV2OtherScript validator
        orefs = Map.keys utxos
        redeemer = Redeemer . toBuiltinData $ Redeem 42
        constraints = mconcat $ map (`mustSpendScriptOutput` redeemer) orefs
    submittedTxId <- getCardanoTxId <$> submitTxConstraintsWith @CTyped lookups constraints
    awaitTxConfirmed submittedTxId
    logInfo @String "collected gifts"
  grab _ = logInfo @String $ "Wrong guess"

-- 6. Define emulator test
test :: EmulatorTest
test =
  initEmulator @CTyped -- initialize the emulator for the contract type using TypeApplications
    3 -- specify the number of wallets involved in the test
    -- list the actions involved in the test
    [ Give 42_000_000 `fromWallet` 1,
      Grab 21 `toWallet` 2,
      Grab 42 `toWallet` 3
    ]

-- Exports
redeemerSuccess :: DataExport
redeemerSuccess = DataExport "ctr42" $ Redeem 42

redeemerFail :: DataExport
redeemerFail = DataExport "ctr21" $ Redeem 21

exports :: JambContract -- Prepare exports for jamb CLI:
exports = exportValidatorWithTest "custom-typed" validator [redeemerSuccess, redeemerFail] test