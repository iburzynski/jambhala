module Contracts.Contracts ( contracts ) where

import Data.Map.Strict ( Map )
import Data.String ( String )
import Plutus.V2.Ledger.Api ( Validator )
import qualified Data.Map.Strict as M

-- Import your contracts here
import qualified Contracts.Simple as Simple
import qualified Contracts.SimpleTyped as SimpleTyped

contracts :: Map String Validator
contracts = M.fromList [
  -- Add contracts here in format `, (<name>, <validator>)`
    ("simple", Simple.validator)
  , ("simple-typed", SimpleTyped.validator)
  ]