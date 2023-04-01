{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric  #-}

module Jambhala.CLI.Types where

import Prelude hiding ( Eq(..), Ord(..) )
import Jambhala.Haskell
import Jambhala.Plutus

import Control.Monad.Freer ( Eff )
import Control.Monad.Freer.Reader (Reader)
import Data.IntMap.Strict ( IntMap )
import Codec.Serialise (Serialise(..))
import Cardano.Ledger.BaseTypes (Network)

type ContractName = String
type Contracts    = Map ContractName ContractExports
type FileName     = String

data JambScript = JambValidator     !Validator
                | JambMintingPolicy !MintingPolicy
  deriving Generic

instance Serialise JambScript where
  encode (JambValidator v)     = encode v
  encode (JambMintingPolicy p) = encode p

data ContractExports = ContractExports { script :: !JambScript
                                       , test   :: !(Maybe EmulatorExport) }

data Command = List
             | Addr   !ContractName !Network
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