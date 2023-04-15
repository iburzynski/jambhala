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
import qualified Contracts.Samples.FreeMinting   as FreeMinting
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
    ("gift",           Simple.giftExports)
  , ("burn",           Simple.burnExports)
  , ("simple-typed",   SimpleTyped.typedExports)
  , ("simple-untyped", SimpleTyped.untypedExports)
  , ("custom-typed",   CustomTyped.exports)
  , ("guess",          Guess.exports)
  , ("vesting",        Vesting.exports)
  , ("parameterized",  Parameterized.exports)
  , ("free-minting",   FreeMinting.exports)
  ]