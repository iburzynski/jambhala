-- Simple Plutus Contracts: always succeeds ("gift")/always fails ("burn")
--1 Extensions
{- The following two extensions are required for all contracts. They're `default-extensions`
   in this project's `.cabal` file, so it isn't necessary to include them explicitly like this in
   your contracts.

   Additional extensions required in more advanced contracts will be introduced as they become necessary.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Contracts.Samples.Simple where

--2 Jambhala Imports
{- `PlutusTx.Prelude` has been set up as an alternate Prelude for this project, so it does not need
   to be imported, and the `NoImplicitPrelude` extension isn't required. -}
{- The `Jambhala.Plutus` module re-exports many common Plutus types and functions, so you don't need
   to keep track of all the messy imports. You can always import the Plutus modules explicitly if
   you prefer.

   Hover over any Plutus type or function to see which module it is imported from. -}

{- The `Jambhala.Utils` module contains boilerplate utility functions for performing common
   operations on contracts.
-}

import Jambhala.Plutus
import Jambhala.Utils

-- 3. Validator Logic:
gift :: BuiltinData -> BuiltinData -> BuiltinData -> ()
gift _ _ _ = ()
{-# INLINEABLE gift #-}

burn :: BuiltinData -> BuiltinData -> BuiltinData -> ()
burn _ _ _ = perror ()
{-# INLINEABLE burn #-}

-- 4. Boilerplate:
giftValidator :: Validator
giftValidator = mkValidatorScript $$(compile [||gift||])

burnValidator :: Validator
burnValidator = mkValidatorScript $$(compile [||burn||])

-- Exports:
unitExport :: DataExport
unitExport = DataExport "unit" ()

giftExports :: JambContract -- Prepare exports for jamb CLI
giftExports = exportContract ("gift" `withScript` giftValidator) {dataExports = [unitExport]}

burnExports :: JambContract
burnExports = exportContract ("burn" `withScript` burnValidator)