{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jambhala.CLI.Types where

import Prelude hiding ( Eq(..), Ord(..) )
import Jambhala.Haskell
import Jambhala.Plutus

import Control.Monad.Freer ( Eff )
import Control.Monad.Freer.Reader (Reader)
import Data.IntMap.Strict ( IntMap )

type ContractName = String
type Contracts    = Map ContractName ContractExports
type FileName     = String

data ContractExports = ContractExports { validator :: !Validator
                                       , test      :: !(Maybe EmulatorExport) }

data Command = List
             | Hash   !ContractName
             | Test   !ContractName
             | Write  !ContractName !(Maybe FileName)
             | Update !(Maybe String)

type JambEmulatorEffects =  Reader WalletQuantity ': EmulatorEffects

type JambEmulatorTrace = Eff JambEmulatorEffects ()

data EmulatorExport = EmulatorExport { jTrace     :: !JambEmulatorTrace
                                     , numWallets :: !WalletQuantity}

type ContractHandles w s e = IntMap (ContractHandle w s e)

newtype WalletQuantity = WalletQuantity { walletQuantity :: Integer }
  deriving ( Eq, Ord, Num )