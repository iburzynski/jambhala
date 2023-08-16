-- Required to derive FromJSON/ToJSON instances for our off-chain types
{-# LANGUAGE DeriveAnyClass #-}
-- Required to derive Generic instances for our off-chain types (required for the former)
{-# LANGUAGE DeriveGeneric #-}
-- Required to define typeclass instances for type synonyms
{-# LANGUAGE FlexibleInstances #-}
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

type CustomTyped = ValidatorContract "custom-typed"

contract :: CustomTyped
contract = mkValidatorContract $$(compile [||untyped||])
  where
    -- conversion to untyped must occur in a different scope when using custom data types
    -- otherwise Template Haskell will try to compile the code before `unstableMakeIsData` completes
    untyped :: UntypedValidator
    untyped = mkUntypedValidator customTyped

-- PART II: OFF-CHAIN EMULATION

-- 1. Define emulator endpoints with ValidatorEndpoints instance
instance ValidatorEndpoints CustomTyped where
  -- 1a. Define associated data type for `give` endpoint's input parameter:
  newtype GiveParam CustomTyped = Give {lovelace :: Integer}
    deriving (Generic, FromJSON, ToJSON) -- parameter values must be convertible to/from JSON
    -- 1b. Define associated data type for `grab` endpoint's input parameter:

  newtype GrabParam CustomTyped = Grab {withGuess :: Integer}
    deriving (Generic, FromJSON, ToJSON)

  -- 1c. Define give endpoint action: send UTXOs to the script address
  give :: GiveParam CustomTyped -> ContractM CustomTyped ()
  give (Give lovelace) = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor contract,
          constraints = mustPayToScriptWithDatum contract () lovelace
        }
    logStr $ printf "Made transaction of %d lovelace." lovelace

  -- 1d. Define grab endpoint action: consume UTXOs at the script address
  grab :: GrabParam CustomTyped -> ContractM CustomTyped ()
  grab (Grab guess) = do
    utxos <- getUtxosAt contract
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor contract `andUtxos` utxos,
          constraints = utxos `mustAllBeSpentWith` Guess guess
        }
    logStr "Collected gifts!"

-- 2. Define emulator test
test :: EmulatorTest
test =
  -- 2a. Call `initEmulator` with a type application to disambiguate the contract type:
  initEmulator @CustomTyped
    -- 2b. Specify the number of wallets required for the test:
    3
    -- 2c. List the actions involved in the test:
    [ Give {lovelace = 42_000_000} `fromWallet` 1,
      Grab {withGuess = 21} `toWallet` 2, -- wrong guess
      Grab {withGuess = 42} `toWallet` 3 -- right guess
    ]

-- Export contract for use with Jamb CLI
exports :: JambExports
exports =
  export
    (defExports contract)
      { dataExports =
          [ -- redeemer that succeeds
            Guess 42 `toJSONfile` "42",
            -- redeemer that fails
            Guess 21 `toJSONfile` "21"
          ],
        emulatorTest = test
      }