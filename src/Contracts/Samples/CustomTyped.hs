-- Required to map validator types and endpoint params
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
validator = mkValidatorScript $$(compile [||untyped||])
  where
    -- conversion to untyped must occur in a different scope when using custom data types
    -- otherwise Template Haskell will try to compile the code before `unstableMakeIsData` completes
    untyped = mkUntypedValidator customTyped

-- PART II: OFF-CHAIN EMULATION

-- 1. Define a data type for the contract with a reference value
data CustomTyped = THIS

-- 2. Map non-unit validator types using associated type families
instance ValidatorTypes CustomTyped where
  type RedeemerType CustomTyped = Redeem

-- 3. Make the contract emulatable with Emulatable instance
instance Emulatable CustomTyped where
  -- Define associated data types for Give and Grab parameters
  data GiveParam CustomTyped = Give {lovelace :: Integer}
    deriving (Generic, FromJSON, ToJSON) -- parameter values must be convertible to/from JSON
  data GrabParam CustomTyped = Grab {withGuess :: Integer}
    deriving (Generic, FromJSON, ToJSON)

  -- 4. Define give endpoint action: send UTXOs to the script address
  give :: GiveParam CustomTyped -> ContractM CustomTyped ()
  give (Give q) = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor THIS validator,
          constraints = mustPayToScriptWithDatum validator () q
        }
    logStr $ printf "Made transaction of %d ADA" q

  -- 5. Define grab endpoint action: consume UTXOs at the script address
  grab :: GrabParam CustomTyped -> ContractM CustomTyped ()
  grab (Grab 42) = do
    utxos <- getUtxosAt validator
    let lookups = scriptLookupsFor THIS validator `andUtxos` utxos
        orefs = Map.keys utxos
        redeemer = mkRedeemer $ Redeem 42
        constraints = mconcatMap (`mustSpendScriptOutput` redeemer) orefs
    submitAndConfirm Tx {..}
    logStr "collected gifts"
  grab _ = logStr "Wrong guess"

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

-- 7. Export contract for use with Jamb CLI
exports :: JambContract
exports =
  exportContract
    ("custom-typed" `withScript` validator)
      { dataExports =
          [ -- redeemer that succeeds
            DataExport "ctr42" $ Redeem 42,
            -- redeemer that fails
            DataExport "ctr21" $ Redeem 21
          ],
        emulatorTest = test
      }