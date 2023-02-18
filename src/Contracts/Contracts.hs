module Contracts.Contracts ( contracts ) where

import Data.Map.Strict ( Map )
import Data.String ( String )
import Plutus.V2.Ledger.Api ( Validator )
import qualified Data.Map.Strict as M
import qualified Contracts.Simple as Simple
import qualified Contracts.SimpleTyped as SimpleTyped

contracts :: Map String Validator
contracts = M.fromList [
    ("simple", Simple.validator)
  , ("simple-typed", SimpleTyped.validator)
  ]

