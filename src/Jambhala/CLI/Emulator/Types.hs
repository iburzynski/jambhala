{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Jambhala.CLI.Emulator.Types where

import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Reader (Reader)
import Data.IntMap.Strict (IntMap, Key)
import Data.Row (Row)
import GHC.Real (Integral, Real)
import Jambhala.Plutus
import Numeric.Natural (Natural)

type JambEmulatorEffects schema = Reader (ContractHandles schema) ': EmulatorEffects

type WalletID = Key

type EmulatorAction s = Eff (JambEmulatorEffects s) ()

data EmulatorTest = ETest {numWallets :: !WalletQuantity, jTrace :: !(Eff EmulatorEffects ())}

class ValidatorTypes c => Emulatable c where
  data GiveParam c :: *
  data GrabParam c :: *
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
