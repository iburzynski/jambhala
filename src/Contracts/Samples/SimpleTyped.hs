module Contracts.Samples.SimpleTyped where

import Jambhala.Plutus
import Jambhala.Utils ( exportValidator, ContractExports )

simpleUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
simpleUntyped _ redeemer _
  | redeemer == mkI 42 = ()
  | otherwise          = traceError "expected 42"
{-# INLINABLE simpleUntyped #-}

untypedValidator :: Validator
untypedValidator = mkValidatorScript $$(compile [|| simpleUntyped ||])

untypedExports :: ContractExports
untypedExports = exportValidator untypedValidator

--------------------------------------------------------------------------------

simpleTyped :: () -> Integer -> ScriptContext -> Bool
simpleTyped _ redeemer _ = traceIfFalse "Sorry, wrong guess!" (redeemer == 42)
{-# INLINABLE simpleTyped #-}

typedValidator :: Validator
typedValidator = mkValidatorScript $$(compile [|| wrapped ||])
  where wrapped = mkUntypedValidator simpleTyped

typedExports :: ContractExports
typedExports = exportValidator typedValidator