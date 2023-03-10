module Contracts ( contracts, samples ) where


import Jambhala.Utils
import qualified Data.Map.Strict as M

-- Sample contracts
import qualified Contracts.Samples.Simple        as Simple
import qualified Contracts.Samples.SimpleTyped   as SimpleTyped
import qualified Contracts.Samples.CustomTyped   as CustomTyped
import qualified Contracts.Samples.Guess         as Guess
import qualified Contracts.Samples.Vesting       as Vesting
import qualified Contracts.Samples.Parameterized as Parameterized
-- Import your contracts here, i.e.:
-- import qualified Contracts.MyContract as MyContract

{- HLINT ignore "Use empty" -}
contracts :: Contracts
contracts = M.fromList [
  -- Add contracts here, i.e.:
--  ("my-contract", MyContract.exports)
  ]

samples :: Contracts
samples = M.fromList [
    ("simple",        Simple.exports)
  , ("simple-typed",  SimpleTyped.exports)
  , ("custom-typed",  CustomTyped.exports)
  , ("guess",         Guess.exports)
  , ("vesting",       Vesting.exports)
  , ("parameterized", Parameterized.exports)
  ]