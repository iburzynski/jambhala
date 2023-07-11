module Jambhala.CLI.Types where

import Data.Map.Strict (Map)
import Jambhala.CLI.Export (ContractExports)
import Jambhala.Plutus (Network)

type ContractName = String

type JambContracts = Map ContractName ContractExports

type FileName = String

data Command
  = List
  | Addr !ContractName !Network
  | Hash !ContractName
  | Test !ContractName
  | Write !ContractName !(Maybe FileName)
  | Update !(Maybe String)