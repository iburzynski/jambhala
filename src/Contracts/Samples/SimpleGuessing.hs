-- 0. Create Module & Declare Imports
module Contracts.Samples.SimpleGuessing where

import Jambhala.Plutus
import Jambhala.Utils

-- 1. Declare Types

-- | Custom Datum type for the guessing validator.
newtype Answer = Answer Integer

{- | Generate `FromData` and `ToData` instances for the custom type with Template Haskell.
  These typeclasses provide methods to convert to/from the `BuiltinData` type.
-}
unstableMakeIsData ''Answer

-- | Custom Redeemer type for the guessing validator.
newtype Guess = Guess Integer

unstableMakeIsData ''Guess

-- 2. Define Lambda

-- | Define typed guessing validator lambda.
guessLambda :: Answer -> Guess -> ScriptContext -> Bool
guessLambda (Answer a) (Guess g) _ = traceIfFalse "Sorry, wrong guess!" (g #== a)
{-# INLINEABLE guessLambda #-}

-- | Convert lambda into "untyped" form before pre-compilation (:: BuiltinData -> BuiltinData -> BuiltinData -> ())
untypedLambda :: UntypedValidator
untypedLambda = mkUntypedValidator guessLambda
{-# INLINEABLE untypedLambda #-}

-- 3. Pre-Compile Lambda

-- | Declare contract synonym with unique symbolic identifier.
type Guessing = ValidatorContract "guessing"

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
compiledScript :: Guessing
compiledScript = mkValidatorContract $$(compile [||untypedLambda||])

-- 4. Export Contract to Jambhala

-- | Define `exports` value for use with `j cli`.
exports :: JambExports
exports =
  export
    (defExports compiledScript)
      { emulatorTest = test
      }

-- 5. Define Emulator Component

-- | Define `ValidatorEndpoints` instance for contract synonym.
instance ValidatorEndpoints Guessing where
  data GiveParam Guessing = Give
    { lovelace :: !Integer
    , withAnswer :: !Integer
    }
    deriving (Generic, ToJSON, FromJSON)
  data GrabParam Guessing = Grab {withGuess :: Integer}
    deriving (Generic, ToJSON, FromJSON)

  give :: GiveParam Guessing -> ContractM Guessing ()
  give (Give lovelace answer) = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor compiledScript
        , constraints = mustPayScriptWithDatum compiledScript (Answer answer) (lovelaceValueOf lovelace)
        }
    logStr $ printf "Gave %d lovelace" lovelace

  grab :: GrabParam Guessing -> ContractM Guessing ()
  grab (Grab g) = do
    utxos <- getUtxosAt compiledScript
    let validUtxos = filterByDatum (\(Answer a) -> a == g) utxos
    if validUtxos == mempty
      then logStr "No matching UTxOs"
      else do
        submitAndConfirm
          Tx
            { lookups = scriptLookupsFor compiledScript `andUtxos` validUtxos
            , constraints = validUtxos `mustAllBeSpentWith` Guess g
            }
        logStr "Collected gifts"

-- | Define emulator test.
test :: EmulatorTest
test =
  initEmulator @Guessing
    6
    [ Give {lovelace = 10_000_000, withAnswer = 42} `fromWallet` 1
    , Give {lovelace = 10_000_000, withAnswer = 42} `fromWallet` 2
    , Give {lovelace = 10_000_000, withAnswer = 21} `fromWallet` 3
    , Grab {withGuess = 33} `toWallet` 4
    , Grab {withGuess = 21} `toWallet` 5
    , Grab {withGuess = 42} `toWallet` 6
    ]
