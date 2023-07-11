{-# LANGUAGE OverloadedLists #-}

module Contracts (contracts, samples) where

-- Sample contracts

import qualified Contracts.Samples.CustomTyped as CustomTyped
import qualified Contracts.Samples.FreeMinting as FreeMinting
import qualified Contracts.Samples.Guess as Guess
import qualified Contracts.Samples.Parameterized as Parameterized
import qualified Contracts.Samples.Simple as Simple
import qualified Contracts.Samples.SimpleTyped as SimpleTyped
import qualified Contracts.Samples.Vesting as Vesting
import Jambhala.Utils

-- Import your contracts here, i.e.:
-- import qualified Contracts.MyContract as MyContract

{- HLINT ignore "Use empty" -}
contracts :: JambContracts
contracts =
  []

-- Add contracts here, i.e.:
--  MyContract.exports

samples :: JambContracts
samples =
  [ Simple.giftExports,
    Simple.burnExports,
    SimpleTyped.typedExports,
    SimpleTyped.untypedExports,
    CustomTyped.exports,
    Guess.exports,
    Vesting.exports,
    Parameterized.exports,
    FreeMinting.exports
  ]