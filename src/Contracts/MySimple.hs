{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Contracts.MySimple where

import Jambhala.Plutus
import Jambhala.Utils

-- Datum -> Redeemer -> ScriptContext -> Bool
gift :: BuiltinData -> BuiltinData -> BuiltinData -> ()
gift _ _ _ = () -- always succeeds
{-# INLINEABLE gift #-}

burn :: BuiltinData -> BuiltinData -> BuiltinData -> ()
burn _ _ _ = perror () -- always fails
{-# INLINEABLE burn #-}