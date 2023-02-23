module Contracts.Samples.SimpleTyped where

import Jambhala.Plutus
import Jambhala.Utils ( ContractExports(..), wrap )

simpleTyped :: () -> Integer -> ScriptContext -> Bool
simpleTyped _ redeemer _ = traceIfFalse "Sorry, wrong guess!" (redeemer == 42)
{-# INLINABLE simpleTyped #-}

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrapped ||])
  where wrapped = wrap simpleTyped

exports :: ContractExports -- Prepare exports for jamb CLI
exports = ContractExports { getValidator = validator, getTest = Nothing }
-- If no emulator test for the contract, pass `Nothing` as second argument to the constructor