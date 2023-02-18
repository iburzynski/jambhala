-- Sample Plutus contract ("always succeeds")

--1 Extensions
{- These two extensions are required for all contracts. They are added as `default-extensions` in
   this project's `.cabal` file, so it isn't necessary to include them explicitly in your contracts
   like below.

   Additional extensions required in more advanced contracts must be added explicitly.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Contracts.Simple where

--2 Imports
{- `PlutusTx.Prelude` has been set up as an alternate Prelude for this project, so it does not need
   to be imported, and the `NoImplicitPrelude` extension isn't required.
-}
import PlutusTx ( compile, CompiledCode )
import Plutus.V1.Ledger.Address ( Address, scriptHashAddress )
import Plutus.V2.Ledger.Api ( Validator, ValidatorHash, mkValidatorScript )
import Plutus.Script.Utils.V2.Scripts ( validatorHash )

--3 Onchain code

--Actual validator logic
{-# INLINABLE simpleV #-}
simpleV :: BuiltinData -> BuiltinData -> BuiltinData -> ()
simpleV _ _ _ = error ()

--Boilerplate
simpleVCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
simpleVCompiled = $$(compile [|| simpleV ||])

validator :: Validator
validator = mkValidatorScript simpleVCompiled

simpleValidatorHash :: ValidatorHash
simpleValidatorHash = validatorHash validator

address :: Address
address = scriptHashAddress simpleValidatorHash
