-- Required to disambiguate Integral literals:
{-# LANGUAGE TypeApplications #-}

module Contracts.Samples.SimpleTyped where

import Jambhala.Plutus
import Jambhala.Utils

simpleUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
simpleUntyped _ redeemer _
  | redeemer #== toBuiltinData @Integer 42 = ()
  | otherwise = traceError "expected 42"
{-# INLINEABLE simpleUntyped #-}

untypedValidator :: Validator
untypedValidator = mkValidatorScript $$(compile [||simpleUntyped||])

untypedExports :: JambContract
untypedExports = exportContract ("simple-untyped" `withScript` untypedValidator)

--------------------------------------------------------------------------------

simpleTyped :: () -> Integer -> ScriptContext -> Bool
simpleTyped _ redeemer _ = traceIfFalse "Sorry, wrong guess!" (redeemer #== 42)
{-# INLINEABLE simpleTyped #-}

typedValidator :: Validator
typedValidator = mkValidatorScript $$(compile [||wrapped||])
  where
    wrapped = mkUntypedValidator simpleTyped

typedRedeemerSuccess :: DataExport
typedRedeemerSuccess = DataExport @Integer "tr42" 42

typedRedeemerFail :: DataExport
typedRedeemerFail = DataExport @Integer "tr21" 42

typedExports :: JambContract
typedExports =
  exportContract
    ("simple-typed" `withScript` typedValidator)
      { dataExports = [typedRedeemerSuccess, typedRedeemerFail]
      }