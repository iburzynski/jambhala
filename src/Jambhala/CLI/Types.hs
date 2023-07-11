{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Jambhala.CLI.Types where

import Codec.Serialise (Serialise (..))
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Reader (Reader)
import Data.IntMap.Strict (IntMap)
import Data.Map (Map)
import Data.Row (Row)
import GHC.Real (Integral, Real)
import Jambhala.Plutus
import Numeric.Natural (Natural)

type ContractName = String

type JambContracts = Map ContractName ContractExports

type FileName = String

data JambScript
  = JambValidator !Validator
  | JambMintingPolicy !MintingPolicy
  deriving (Generic)

instance Serialise JambScript where
  encode (JambValidator v) = encode v
  encode (JambMintingPolicy p) = encode p

data ContractExports = ContractExports
  { script :: !JambScript,
    dataExports :: ![DataExport],
    test :: !(Maybe EmulatorTest)
  }

type JambContract = (String, ContractExports)

data Command
  = List
  | Addr !ContractName !Network
  | Hash !ContractName
  | Test !ContractName
  | Write !ContractName !(Maybe FileName)
  | Update !(Maybe String)

type JambEmulatorEffects schema = Reader (ContractHandles schema) ': EmulatorEffects

data EmulatorTest = ETest {numWallets :: !WalletQuantity, jTrace :: !(Eff EmulatorEffects ())}

data EmulatorParams c where
  EmulatorParams ::
    { giveAction :: !(GiveAction c),
      grabAction :: !(GrabAction c),
      walletQuantity :: !WalletQuantity
    } ->
    EmulatorParams c

mkEmulatorParams ::
  Emulatable c =>
  GiveAction c ->
  GrabAction c ->
  WalletQuantity ->
  EmulatorParams c
mkEmulatorParams = EmulatorParams

class ValidatorTypes c => Emulatable c where
  data GiveParam c :: *
  data GrabParam c :: *

  -- type GrabParam c = ()
  type Schema c :: Row *
  type
    Schema c =
      Endpoint "give" (GiveParam c)
        .\/ Endpoint "grab" (GrabParam c)
  type GiveAction c :: *
  type GiveAction c = GiveParam c -> Contract () (Schema c) Text ()
  type GrabAction c :: *
  type GrabAction c = GrabParam c -> Contract () (Schema c) Text ()
  give :: GiveParam c -> Contract () (Schema c) Text ()
  grab :: GrabParam c -> Contract () (Schema c) Text ()

type ContractActions s = Contract () s Text ()

type ContractPromise s = Promise () s Text ()

type ContractHandles s = IntMap (ContractHandle () s Text)

newtype WalletQuantity = WalletQuantity {walletQ :: Natural}
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral)

data DataExport where
  DataExport :: (ToData a) => String -> a -> DataExport

instance ToData DataExport where
  toBuiltinData (DataExport _ x) = toBuiltinData x