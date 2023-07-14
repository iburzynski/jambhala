{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Jambhala.CLI.Emulator.Types where

import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Reader (Reader)
import Data.IntMap.Strict (IntMap, Key)
import Data.Row (Row)
import Jambhala.CLI.Types (IsScript (..), JambScript' (..))
import Jambhala.Plutus

type JambEmulatorEffects schema = Reader (ContractHandles schema) ': EmulatorEffects

type WalletID = Key

type EmulatorAction s = Eff (JambEmulatorEffects s) ()

class ValidatorTypes contract => Emulatable contract where
  data GiveParam contract :: *
  data GrabParam contract :: *
  type Schema contract :: Row *
  type
    Schema contract =
      Endpoint "give" (GiveParam contract)
        .\/ Endpoint "grab" (GrabParam contract)
  type GiveAction contract :: *
  type GiveAction contract = GiveParam contract -> ContractM contract ()
  type GrabAction contract :: *
  type GrabAction contract = GrabParam contract -> ContractM contract ()
  type ContractM contract :: * -> *
  type ContractM contract = Contract () (Schema contract) Text
  give :: GiveParam contract -> ContractM contract ()
  grab :: GrabParam contract -> ContractM contract ()
  scriptLookupsFor :: IsScript (JambScript' script) => contract -> script -> ScriptLookups contract
  scriptLookupsFor _ = scriptLookupFunc @_ @contract . JambScript

type ContractActions s = Contract () s Text ()

type ContractPromise s = Promise () s Text ()

type ContractHandles s = IntMap (ContractHandle () s Text)
