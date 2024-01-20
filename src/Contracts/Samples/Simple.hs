-- Simple Plutus Contracts: always succeeds ("gift")/always fails ("burn")
-- 1. Extensions
{- The following two extensions are required for all contracts. They're `default-extensions`
   in this project's `.cabal` file, so it isn't necessary to include them explicitly like this in
   your contracts.

   Additional extensions required in more advanced contracts will be introduced as they become necessary.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Contracts.Samples.Simple where

-- 2. Jambhala Imports
{- `PlutusTx.Prelude` has been set up as an alternate Prelude for this project, so it does not need
   to be imported, and the `NoImplicitPrelude` extension isn't required. -}
{- The `Jambhala.Plutus` module re-exports many common Plutus types and functions, so you don't need
   to keep track of all the messy imports. You can always import the Plutus modules explicitly if
   you prefer.

   Hover over any Plutus type or function to see which module it is imported from. -}
import Jambhala.Plutus

{- The `Jambhala.Utils` module contains boilerplate utility functions for performing common
   operations on contracts.
-}
import Jambhala.Utils

-- 3. Define validator:
giftLambda :: BuiltinData -> BuiltinData -> BuiltinData -> ()
giftLambda _ _ _ = () -- always succeeds
{-# INLINEABLE giftLambda #-}

burnLambda :: BuiltinData -> BuiltinData -> BuiltinData -> ()
burnLambda _ _ _ = perror () -- always fails
{-# INLINEABLE burnLambda #-}

-- 4. Define custom type for contract:
type Gift = ValidatorContract "gift"

type Burn = ValidatorContract "burn"

-- 5. Compile to custom contract type:
giftCompiled :: Gift
giftCompiled = mkValidatorContract $$(compile [||giftLambda||])

burnCompiled :: Burn
burnCompiled = mkValidatorContract $$(compile [||burnLambda||])

-- 6. Prepare exports for use with `j cli`:
giftExports :: JambExports
giftExports =
  export
    (defExports giftCompiled)
      { dataExports =
          [ () `toJSONfile` "unit"
          ]
      }

burnExports :: JambExports
burnExports = export (defExports burnCompiled)

-- (Now import JambExports values and add to list in `src/Contracts.hs`)
