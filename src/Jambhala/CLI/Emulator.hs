{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Jambhala.CLI.Emulator.Types
import Jambhala.CLI.Types
import Jambhala.Plutus
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
  forall contract.
  ( Emulatable contract,
    ToJSON (GiveParam contract),
    FromJSON (GiveParam contract),
    FromJSON (GrabParam contract),
    ToJSON (GrabParam contract)
  ) =>
  WalletQuantity ->
  [EmulatorAction (Schema contract)] ->
  EmulatorTest
initEmulator numWallets effs = ETest {..}
  where
    jTrace = do
      let endpoints = mkEndpoints @contract give grab
      hs <- activateWallets numWallets endpoints
      runReader hs (sequence_ effs)

activateWallets ::
  ( ContractConstraints (Schema contract),
    Member StartContract effs
  ) =>
  WalletQuantity ->
  ContractM contract () ->
  Eff effs (ContractHandles (Schema contract))
activateWallets (WalletQuantity wq) endpoints = do
  hs <- traverse ((`activateContractWallet` endpoints) . knownWallet) [1 .. fromIntegral wq]
  pure . IntMap.fromList $ zip [1 .. fromIntegral wq] hs

-- | Returns the PaymentPubKeyHash for an EmulatorTest wallet
pkhForWallet :: Key -> PaymentPubKeyHash
pkhForWallet = mockWalletPaymentPubKeyHash . knownWallet . fromIntegral

mkEndpoints ::
  forall contract.
  ( FromJSON (GiveParam contract),
    FromJSON (GrabParam contract),
    ContractConstraints (Schema contract)
  ) =>
  GiveAction contract ->
  GrabAction contract ->
  ContractActions (Schema contract)
mkEndpoints giveAction grabAction = endpoints
  where
    endpoints :: ContractActions (Schema contract)
    endpoints = awaitPromise (give' `select` grab') >> endpoints
    give' :: Promise () (Schema contract) Text ()
    give' = endpoint @"give" @(GiveParam contract) @_ @(Schema contract) $ giveAction
    grab' :: Promise () (Schema contract) Text ()
    grab' = endpoint @"grab" @(GrabParam contract) @_ @(Schema contract) $ grabAction

fromWallet ::
  forall contract.
  ( ToJSON (GiveParam contract),
    ContractConstraints (Schema contract),
    HasEndpoint "give" (GiveParam contract) (Schema contract)
  ) =>
  GiveParam contract ->
  WalletID ->
  EmulatorAction (Schema contract)
fromWallet p w = do
  h <- asks (! w)
  callEndpoint @"give" @(GiveParam contract) @() @(Schema contract) @Text h p
  wait

toWallet ::
  forall contract.
  ( ToJSON (GrabParam contract),
    ContractConstraints (Schema contract),
    HasEndpoint "grab" (GrabParam contract) (Schema contract)
  ) =>
  GrabParam contract ->
  WalletID ->
  EmulatorAction (Schema contract)
toWallet p w = do
  h <- asks (! w)
  callEndpoint @"grab" @(GrabParam contract) @() @(Schema contract) @Text h p
  wait

-- | Wait 1 slot in an EmulatorTest
wait :: Member Waiting effs => Eff effs ()
wait = void $ waitNSlots 1

-- | Wait until the specified slot in an EmulatorTest
waitUntil :: Member Waiting effs => Slot -> Eff effs ()
waitUntil slot = void $ waitUntilSlot slot