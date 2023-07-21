-- Required to derive FromJSON/ToJSON instances for our off-chain types
{-# LANGUAGE DeriveAnyClass #-}
-- Required to derive Generic instances for our off-chain types (required for the former)
{-# LANGUAGE DeriveGeneric #-}
-- Required to map validator types and endpoint params
{-# LANGUAGE TypeFamilies #-}

module Contracts.Samples.CustomTyped where

import Jambhala.Plutus
import Jambhala.Utils

-- Define a custom data type for the redeemer
newtype CustomRedeemer = Guess Integer

-- `unstableMakeIsData` uses TemplateHaskell to generate ToData/FromData instances for a custom type
-- These classes have toBuiltInData/fromBuiltInData methods to convert between Haskell/Plutus data.
unstableMakeIsData ''CustomRedeemer

-- `makeIsDataIndexed` generates ToData/FromData instances with constructors defined in a specific order.
-- This should be used instead of `unstableMakeIsData` if your custom type is a sum type (i.e. has multiple constructors)
-- makeIsDataIndexed ''CustomRedeemer [('Guess, 0)]

customTyped :: () -> CustomRedeemer -> ScriptContext -> Bool
customTyped _ (Guess g) _ = traceIfFalse "Sorry, wrong guess!" (g #== 42)
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
  type RedeemerType CustomTyped = CustomRedeemer

-- 3. Make the contract emulatable with Emulatable instance
instance Emulatable CustomTyped where
  -- Define associated data types for Give and Grab parameters
  newtype GiveParam CustomTyped = Give {lovelace :: Integer}
    deriving (Generic, FromJSON, ToJSON) -- parameter values must be convertible to/from JSON
  newtype GrabParam CustomTyped = Grab {withGuess :: Integer}
    deriving (Generic, FromJSON, ToJSON)

  -- 4. Define give endpoint action: send UTXOs to the script address
  give :: GiveParam CustomTyped -> ContractM CustomTyped ()
  give (Give lovelace) = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor THIS validator,
          constraints = mustPayToScriptWithDatum validator () lovelace
        }
    logStr $ printf "Made transaction of %d lovelace." lovelace

  -- 5. Define grab endpoint action: consume UTXOs at the script address
  grab :: GrabParam CustomTyped -> ContractM CustomTyped ()
  grab (Grab 42) = do
    utxos <- getUtxosAt validator
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor THIS validator `andUtxos` utxos,
          constraints = utxos `mustAllBeSpentWith` Guess 42
        }
    logStr "Collected gifts!"
  grab _ = logStr "Wrong guess!"

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
            Guess 42 `toJSONfile` "42",
            -- redeemer that fails
            Guess 21 `toJSONfile` "21"
          ],
        emulatorTest = test
      }