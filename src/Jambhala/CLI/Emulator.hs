{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jambhala.CLI.Emulator
  ( EmulatorTest,
    activateWallets,
    fromWallet,
    initEmulator,
    mkEndpoints,
    notImplemented,
    pkhForWallet,
    runJambEmulator,
    toWallet,
    wait,
    waitUntil,
    (IntMap.!),
  )
where

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Reader (asks, runReader)
import Data.Default (def)
import Data.IntMap.Strict (Key, (!))
import qualified Data.IntMap.Strict as IntMap
import Data.Row (Row)
import GHC.Real (fromIntegral)
import Jambhala.CLI.Emulator.Types
import Jambhala.Plutus
import Ledger (Slot)
import Plutus.Contract (HasEndpoint)
import Plutus.Contract.Trace (defaultDistFor)
import Plutus.Trace (EmulatorConfig (..), EmulatorRuntimeError (..))
import Plutus.Trace.Effects.RunContract (StartContract)
import Plutus.Trace.Effects.Waiting (Waiting)
import Plutus.Trace.Emulator (ContractConstraints, throwError)
import Wallet.Emulator.Types (knownWallets)

runJambEmulator :: EmulatorTest -> IO ()
runJambEmulator ETest {..} =
  runEmulatorTraceIOWithConfig def (mkEmulatorConfig numWallets) jTrace
  where
    mkEmulatorConfig (WalletQuantity n) = EmulatorConfig (Left $ defaultDistFor $ take (fromIntegral n) knownWallets) def

notImplemented :: EmulatorTest
notImplemented = ETest {numWallets = 1, jTrace = logNotImplemented}
  where
    logNotImplemented :: Eff EmulatorEffects ()
    logNotImplemented = do
      throwError $ GenericError "No emulator test implemented for contract"

initEmulator ::
  forall c.
  ( Emulatable c,
    FromJSON (GiveParam c),
    FromJSON (GrabParam c),
    ContractConstraints (Schema c),
    HasEndpoint "give" (GiveParam c) (Schema c),
    HasEndpoint "grab" (GrabParam c) (Schema c),
    (GiveAction c ~ (GiveParam c -> Contract () (Schema c) Text ())),
    (GrabAction c ~ (GrabParam c -> Contract () (Schema c) Text ()))
  ) =>
  WalletQuantity ->
  [EmulatorAction (Schema c)] ->
  EmulatorTest
initEmulator numWallets effs = ETest {..}
  where
    jTrace = do
      let endpoints = mkEndpoints @c give grab
      hs <- activateWallets numWallets endpoints
      runReader hs (sequence_ effs)

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
activateWallets ::
  forall
    (s :: Row *)
    (effs :: [* -> *]).
  ( ContractConstraints s,
    Member StartContract effs
  ) =>
  WalletQuantity ->
  Contract () s Text () ->
  Eff effs (ContractHandles s)
activateWallets (WalletQuantity wq) endpoints = do
  hs <- traverse ((`activateContractWallet` endpoints) . knownWallet) [1 .. fromIntegral wq]
  pure . IntMap.fromList $ zip [1 .. fromIntegral wq] hs

-- | Returns the PaymentPubKeyHash for a JambEmulatorTrace wallet
pkhForWallet :: Key -> PaymentPubKeyHash
pkhForWallet = mockWalletPaymentPubKeyHash . knownWallet . fromIntegral

mkEndpoints ::
  forall c.
  ( Emulatable c,
    FromJSON (GiveParam c),
    FromJSON (GrabParam c),
    ContractConstraints (Schema c),
    HasEndpoint "give" (GiveParam c) (Schema c),
    HasEndpoint "grab" (GrabParam c) (Schema c),
    (GiveAction c ~ (GiveParam c -> Contract () (Schema c) Text ())),
    (GrabAction c ~ (GrabParam c -> Contract () (Schema c) Text ()))
  ) =>
  GiveAction c ->
  GrabAction c ->
  ContractActions (Schema c)
mkEndpoints giveAction grabAction = endpoints
  where
    endpoints :: ContractActions (Schema c)
    endpoints = awaitPromise (give' `select` grab') >> endpoints
    give' :: Promise () (Schema c) Text ()
    give' = endpoint @"give" @(GiveParam c) @_ @(Schema c) $ giveAction
    grab' :: Promise () (Schema c) Text ()
    grab' = endpoint @"grab" @(GrabParam c) @_ @(Schema c) $ grabAction

fromWallet ::
  forall (schema :: Row *) giveParam.
  ( ToJSON giveParam,
    ContractConstraints schema,
    HasEndpoint "give" giveParam schema
  ) =>
  giveParam ->
  WalletID ->
  EmulatorAction schema
fromWallet p w = do
  h <- asks (! w)
  callEndpoint @"give" @giveParam @() @schema @Text h p
  wait

toWallet ::
  forall (schema :: Row *) grabParam.
  ( ToJSON grabParam,
    ContractConstraints schema,
    HasEndpoint "grab" grabParam schema
  ) =>
  grabParam ->
  WalletID ->
  EmulatorAction schema
toWallet p w = do
  h <- asks (! w)
  callEndpoint @"grab" @grabParam @() @schema @Text h p
  wait

-- | Wait 1 slot in an EmulatorTest
wait :: Member Waiting effs => Eff effs ()
wait = void $ waitNSlots 1

-- | Wait until the specified slot in an EmulatorTest
waitUntil :: Member Waiting effs => Slot -> Eff effs ()
waitUntil slot = void $ waitUntilSlot slot