module Contracts ( contracts, samples ) where

import Data.Map.Strict ( Map )
import Data.String ( String )
import Plutus.V2.Ledger.Api ( Validator )
import qualified Data.Map.Strict as M

-- Sample contracts
import qualified Contracts.Samples.Simple as Simple
import qualified Contracts.Samples.SimpleTyped as SimpleTyped
-- Import your contracts here, i.e.:
-- import qualified Contracts.MyContract as MyContract


{- HLINT ignore "Use empty" -}
contracts :: Map String Validator
contracts = M.fromList [
  -- Add contracts here, i.e.:
--  ("my-contract", MyContract.validator)
  ]

samples :: Map String Validator
samples = M.fromList [
    ("simple", Simple.validator)
  , ("simple-typed", SimpleTyped.validator)
  ]