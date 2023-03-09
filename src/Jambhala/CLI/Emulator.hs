{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}

module Jambhala.CLI.Emulator ( EmulatorExport(..), JambEmulatorTrace, activateWallets, runJambEmulator, wait1 ) where

import Prelude hiding ( Monoid(..), Traversable(..) )
import Jambhala.CLI.Types
import Jambhala.Haskell hiding ( asks )
import Jambhala.Plutus

import Data.Aeson ( ToJSON, FromJSON )
import Plutus.Trace (EmulatorConfig (..))
import Wallet.Emulator.Types (knownWallets)
import Data.Default (def)
import Plutus.Contract.Trace (defaultDistFor)
import Plutus.Contract (IsContract)
import Plutus.Trace.Emulator (ContractConstraints)
import Plutus.Trace.Effects.RunContract (StartContract)
import Control.Monad.Freer ( Eff, Member )
import Data.Row ( Row )

import GHC.Real (fromIntegral)
import Control.Monad.Freer.Reader (Reader, asks, runReader)
import Plutus.Trace.Effects.Waiting (Waiting)

import qualified Data.IntMap.Strict as IntMap

runJambEmulator :: EmulatorExport -> IO ()
runJambEmulator EmulatorExport{..} =
  runEmulatorTraceIOWithConfig def (mkEmulatorConfig numWallets) $ runReader numWallets jTrace
  where
    mkEmulatorConfig (WalletQuantity n) = EmulatorConfig (Left $ defaultDistFor $ take n knownWallets) def

-- | Takes endpoints and activates all mock wallets in a `JambEmulatorTrace` test, returning their
-- `ContractHandle` values in an `IntMap` with keys corresponding to wallet numbers. Activated
-- wallet handles can be referenced in the test via the (`!`) operator.

-- To use, apply to endpoints listener (endpoints :: Contract w s e a)
-- Example:
-- ```
-- hs <- activateWallets endpoints
-- callEndpoint @"give" (hs ! 1) 33_000_000
-- ```

-- This function can only be used in the enhanced `JambEmulatorTrace` context, not the
-- standard `EmulatorTrace` context.
activateWallets :: forall (contract :: * -> Row * -> * -> * -> *) w (s :: Row *) e
       (effs :: [* -> *]). (IsContract contract, ContractConstraints s, Show e, ToJSON e,
 FromJSON e, ToJSON w, FromJSON w, Member StartContract effs, Member (Reader WalletQuantity) effs,
 Monoid w) => contract w s e () -> Eff effs (ContractHandles w s e)
activateWallets endpoints = do
  n  <- asks walletQuantity
  hs <- traverse ((`activateContractWallet` endpoints) . knownWallet) [1 .. n]
  return . IntMap.fromList $ zip [1 .. fromIntegral n] hs

-- | Waits 1 slot in an EmulatorTrace test and discards the slot value
wait1 :: Member Waiting effs => Eff effs ()
wait1 = void $ waitNSlots 1