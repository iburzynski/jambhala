-- Sample Plutus contract ("always succeeds")

--1 Extensions
{- The following two extensions are required for all contracts. They're `default-extensions`
   in this project's `.cabal` file, so it isn't necessary to include them explicitly like this in
   your contracts.

   Additional extensions required in more advanced contracts must be added explicitly.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Contracts.Samples.Simple where

--2 Jambhala Imports
{- `PlutusTx.Prelude` has been set up as an alternate Prelude for this project, so it does not need
   to be imported, and the `NoImplicitPrelude` extension isn't required. -}
{- The `Jambhala.Plutus` module re-exports many common Plutus types and functions, so you don't need
   to keep track of all the messy imports. You can always import the Plutus modules explicitly if
   you prefer.

   Hover over any Plutus type or function to see which module it is imported from. -}
import Jambhala.Plutus
{- The `Jambhala.Utils` module contains boilerplate utility functions for performing various common
   operations on contracts.
-}
import Jambhala.Utils

-- 3. Validator Logic:
simpleV :: BuiltinData -> BuiltinData -> BuiltinData -> ()
simpleV _ _ _ = error ()
{-# INLINABLE simpleV #-}

-- 4. Boilerplate:
simpleVCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
simpleVCompiled = $$(compile [|| simpleV ||])

validator :: Validator
validator = mkValidatorScript simpleVCompiled

exports :: ContractExports -- Prepare exports for jamb CLI
exports = ContractExports { getValidator = validator, getTest = Nothing }
-- If no emulator test for the contract, pass `Nothing` as second argument to the constructor