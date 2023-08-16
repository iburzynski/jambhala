-- Required to write `traceError` BuiltinString messages as string literals:
{-# LANGUAGE OverloadedStrings #-}

module Contracts.Samples.SimpleTyped where

import Jambhala.Plutus
import Jambhala.Utils

simpleUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
simpleUntyped _ redeemer _
  | redeemer #== toBuiltinData @Integer 42 = ()
  | otherwise = traceError "expected 42"
{-# INLINEABLE simpleUntyped #-}

type Untyped = ValidatorContract "simple-untyped"

untypedValidator :: Untyped
untypedValidator = mkValidatorContract $$(compile [||simpleUntyped||])

untypedExports :: JambExports
untypedExports = export (defExports untypedValidator)

--------------------------------------------------------------------------------

simpleTyped :: () -> Integer -> ScriptContext -> Bool
simpleTyped _ redeemer _ = traceIfFalse "Sorry, wrong guess!" (redeemer #== 42)
{-# INLINEABLE simpleTyped #-}

type Typed = ValidatorContract "simple-typed"

typedValidator :: Typed
typedValidator = mkValidatorContract $$(compile [||untyped||])
  where
    untyped :: UntypedValidator
    untyped = mkUntypedValidator simpleTyped

typedExports :: JambExports
typedExports =
  export
    (defExports typedValidator)
      { dataExports = [(42 :: Integer) `toJSONfile` "tr42", (21 :: Integer) `toJSONfile` "tr21"]
      }