-- Required to write `traceError` BuiltinString messages as string literals:
{-# LANGUAGE OverloadedStrings #-}

-- 0. Create Module & Declare Imports
module Contracts.Samples.SimpleTyped where

import Jambhala.Plutus
import Jambhala.Utils

-- 1. Declare Types: N/A

-- 2. Define Lambda

-- | Define a lambda without types (all inputs `:: BuiltinData`).
typelessLambda :: BuiltinData -> BuiltinData -> BuiltinData -> ()
typelessLambda _ redeemer _
  | redeemer #== toBuiltinData @Integer 42 = ()
  | otherwise = traceError "expected 42"
{-# INLINEABLE typelessLambda #-}

-- 3. Pre-Compile Lambda

-- | Declare contract synonym with unique symbolic identifier.
type Untyped = ValidatorContract "simple-untyped"

compiledTypelessScript :: Untyped
compiledTypelessScript = mkValidatorContract $$(compile [||typelessLambda||])

-- 4. Export Contract to Jambhala

-- | Define exports value for the typeless version to use with `j cli`.
untypedExports :: JambExports
untypedExports = export (defExports compiledTypelessScript)

--------------------------------------------------------------------------------

-- 1. Declare Types: N/A

-- 2. Define Lambda

-- | Define a lambda using Haskell types.
simpleTypedLambda :: () -> Integer -> ScriptContext -> Bool
simpleTypedLambda _ redeemer _ = traceIfFalse "Sorry, wrong guess!" (redeemer #== 42)
{-# INLINEABLE simpleTypedLambda #-}

-- | Convert lambda into "untyped" form before pre-compilation (:: BuiltinData -> BuiltinData -> BuiltinData -> ())
untypedLambda :: UntypedValidator
untypedLambda = mkUntypedValidator simpleTypedLambda
{-# INLINEABLE untypedLambda #-}

-- 3. Pre-Compile Lambda

-- | Declare contract synonym with unique symbolic identifier.
type Typed = ValidatorContract "simple-typed"

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
compiledSimpleTyped :: Typed
compiledSimpleTyped = mkValidatorContract $$(compile [||untypedLambda||])

-- 4. Export Contract to Jambhala

-- | Define exports value for the typed version to use with `jamb` CLI.
typedExports :: JambExports
typedExports =
  export
    (defExports compiledSimpleTyped)
      { dataExports = [(42 :: Integer) `toJSONfile` "tr42", (21 :: Integer) `toJSONfile` "tr21"]
      }
