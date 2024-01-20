-- Required to derive FromJSON/ToJSON instances for our off-chain types
{-# LANGUAGE DeriveAnyClass #-}
-- Required to derive Generic instances for our off-chain types (required for the former)
{-# LANGUAGE DeriveGeneric #-}
-- Required to define typeclass instances for Jambhala contract type synonyms
{-# LANGUAGE FlexibleInstances #-}
-- Required to map validator types and endpoint params
{-# LANGUAGE TypeFamilies #-}

-- 0. Create Module & Declare Imports
module Contracts.Samples.CustomTyped where

import Jambhala.Plutus
import Jambhala.Utils

-- 1. Declare Types

-- Define a custom data type for the redeemer.
newtype CustomRedeemer = Guess Integer

-- `unstableMakeIsData` uses TemplateHaskell to generate ToData/FromData instances for a custom type
-- These classes have toBuiltInData/fromBuiltInData methods to convert between Haskell/Plutus data.
unstableMakeIsData ''CustomRedeemer

-- 2. Define Lambda

-- | Define typed version of the validator lambda.
customTypedLambda :: () -> CustomRedeemer -> ScriptContext -> Bool
customTypedLambda _ (Guess g) _ = traceIfFalse "Sorry, wrong guess!" (g #== 42)
{-# INLINEABLE customTypedLambda #-}

{- | Convert lambda into "untyped" form before pre-compilation (:: BuiltinData -> BuiltinData -> BuiltinData -> ())
  Conversion to untyped must occur in a different scope than the call to `compile` during pre-compilation.
  Otherwise Template Haskell will try to compile the code before `unstableMakeIsData` completes!
-}
untypedLambda :: UntypedValidator
untypedLambda = mkUntypedValidator customTypedLambda
{-# INLINEABLE untypedLambda #-}

-- 3. Pre-Compile Lambda

-- | Declare contract synonym with unique symbolic identifier.
type CustomTyped = ValidatorContract "custom-typed"

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
compiledScript :: CustomTyped
compiledScript = mkValidatorContract $$(compile [||untypedLambda||])

-- 4. Export Contract to Jambhala

-- | Define `exports` value for use with `j cli`.
exports :: JambExports
exports =
  export
    (defExports compiledScript)
      { dataExports =
          [ -- redeemer that succeeds
            Guess 42 `toJSONfile` "42"
          , -- redeemer that fails
            Guess 21 `toJSONfile` "21"
          ]
      , emulatorTest = test
      }

-- 5. Define Emulator Component

-- | Define `ValidatorEndpoints` instance for the contract synonym.
instance ValidatorEndpoints CustomTyped where
  newtype GiveParam CustomTyped = Give {lovelace :: Integer}
    deriving (Generic, FromJSON, ToJSON) -- parameter values must be convertible to/from JSON

  newtype GrabParam CustomTyped = Grab {withGuess :: Integer}
    deriving (Generic, FromJSON, ToJSON)

  give :: GiveParam CustomTyped -> ContractM CustomTyped ()
  give (Give lovelace) = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor compiledScript
        , constraints = mustPayScriptWithDatum compiledScript () (lovelaceValueOf lovelace)
        }
    logStr $ printf "Made transaction of %d lovelace." lovelace
  grab :: GrabParam CustomTyped -> ContractM CustomTyped ()
  grab (Grab guess) = do
    utxos <- getUtxosAt compiledScript
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor compiledScript `andUtxos` utxos
        , constraints = utxos `mustAllBeSpentWith` Guess guess
        }
    logStr "Collected gifts!"

-- | Define emulator test.
test :: EmulatorTest
test =
  -- Call `initEmulator` with a type application to disambiguate the contract type:
  initEmulator @CustomTyped
    -- Specify the number of wallets required for the test:
    3
    -- List the emulator actions involved in the test:
    [ Give {lovelace = 42_000_000} `fromWallet` 1
    , Grab {withGuess = 21} `toWallet` 2 -- wrong guess
    , Grab {withGuess = 42} `toWallet` 3 -- right guess
    ]
