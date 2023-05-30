{-# LANGUAGE TypeApplications #-}

module Contracts.Samples.SimpleTyped where

import Jambhala.Plutus
import Jambhala.Utils (ContractExports, DataExport (..), exportValidator)

simpleUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
simpleUntyped _ redeemer _
  | redeemer == toBuiltinData @Integer 42 = ()
  | otherwise = traceError "expected 42"
{-# INLINEABLE simpleUntyped #-}

untypedValidator :: Validator
untypedValidator = mkValidatorScript $$(compile [||simpleUntyped||])

untypedExports :: ContractExports
untypedExports = exportValidator untypedValidator []

--------------------------------------------------------------------------------

simpleTyped :: () -> Integer -> ScriptContext -> Bool
simpleTyped _ redeemer _ = traceIfFalse "Sorry, wrong guess!" (redeemer == 42)
{-# INLINEABLE simpleTyped #-}

typedValidator :: Validator
typedValidator = mkValidatorScript $$(compile [||wrapped||])
  where
    wrapped = mkUntypedValidator simpleTyped

typedRedeemerSuccess :: DataExport
typedRedeemerSuccess = DataExport "tr42" (42 :: Integer)

typedRedeemerFail :: DataExport
typedRedeemerFail = DataExport "tr21" (21 :: Integer)

typedExports :: ContractExports
typedExports = exportValidator typedValidator [typedRedeemerSuccess, typedRedeemerFail]