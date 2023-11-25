module Jambhala.CLI.Emulator.Types where

import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Reader (Reader)
import Data.IntMap.Strict (IntMap, Key)
import Jambhala.CLI.Types (Schema)
import Jambhala.Plutus

type ContractHandles s = IntMap (ContractHandle () s Text)

type JambEmulatorEffects contract = Reader (ContractHandles (Schema contract)) ': EmulatorEffects

type WalletID = Key

type EmulatorAction contract = Eff (JambEmulatorEffects contract) ()
