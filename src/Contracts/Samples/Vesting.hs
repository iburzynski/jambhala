{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Contracts.Samples.Vesting where

import Control.Lens ((^?))
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (..))
import qualified Data.Map.Strict as Map
import Jambhala.Haskell
import Jambhala.Plutus
import Jambhala.Utils
import Text.Printf (printf)
import Prelude hiding (Applicative (..), Eq (..), Semigroup (..), Traversable (..), mconcat, (<$>))

-- Datum
data VestingDatum = VestingDatum
  { getBeneficiary :: PaymentPubKeyHash,
    getDeadline :: POSIXTime
  }

unstableMakeIsData ''VestingDatum

vesting :: VestingDatum -> () -> ScriptContext -> Bool
vesting (VestingDatum ben dline) _ sc =
  traceIfFalse "Beneficiary's signature missing" signedByBeneficiary
    && traceIfFalse "Deadline not reached" dlineReached
  where
    txInfo = scriptContextTxInfo sc
    signedByBeneficiary = txSignedBy txInfo $ unPaymentPubKeyHash ben
    dlineReached = contains (from dline) $ txInfoValidRange txInfo
{-# INLINEABLE vesting #-}

validator :: Validator
validator = mkValidatorScript $$(compile [||wrapped||])
  where
    wrapped = mkUntypedValidator vesting

data VTypes

instance ValidatorTypes VTypes where
  type DatumType VTypes = VestingDatum
  type RedeemerType VTypes = ()

data GiveParam = GiveParam
  { gpBeneficiary :: !PaymentPubKeyHash,
    gpDeadline :: !POSIXTime,
    gpAmount :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON)

type Schema =
  Endpoint "give" GiveParam
    .\/ Endpoint "grab" ()

give :: GiveParam -> Contract () Schema Text ()
give (GiveParam {..}) = do
  submittedTxId <- getCardanoTxId <$> submitTxConstraintsWith @VTypes lookups constraints
  _ <- awaitTxConfirmed submittedTxId
  logInfo @String $
    printf
      "Made a gift of %d lovelace to %s with deadline %s"
      gpAmount
      (show gpBeneficiary)
      (show gpDeadline)
  where
    vHash = validatorHash validator
    datum = Datum . toBuiltinData $ VestingDatum gpBeneficiary gpDeadline
    lookups = plutusV2OtherScript validator
    constraints = mustPayToOtherScriptWithDatumInTx vHash datum $ lovelaceValueOf gpAmount

grab :: () -> Contract () Schema Text ()
grab _ = do
  pkh <- ownFirstPaymentPubKeyHash
  now <- uncurry interval <$> currentNodeClientTimeRange
  addr <- getContractAddress validator
  validUtxos <- Map.mapMaybe (isEligible pkh now) <$> utxosAt addr
  if Map.null validUtxos
    then logInfo @String $ "No eligible gifts available"
    else do
      let lookups = unspentOutputs validUtxos <> plutusV2OtherScript validator
          constraints =
            mconcat $
              mustValidateInTimeRange (fromPlutusInterval now) :
              mustBeSignedBy pkh :
              map (`mustSpendScriptOutput` unitRedeemer) (Map.keys validUtxos)
      submittedTx <- getCardanoTxId <$> submitTxConstraintsWith @VTypes lookups constraints
      _ <- awaitTxConfirmed submittedTx
      logInfo @String "Collected eligible gifts"
  where
    isEligible :: PaymentPubKeyHash -> Interval POSIXTime -> DecoratedTxOut -> Maybe DecoratedTxOut
    isEligible pkh now dto = do
      (_, dfq) <- dto ^? decoratedTxOutDatum
      Datum d <- dfq ^? datumInDatumFromQuery
      VestingDatum ben dline <- fromBuiltinData d
      guard (ben == pkh && from dline `contains` now)
      Just dto

endpoints :: Contract () Schema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" grab

test :: JambEmulatorTrace
test = do
  hs <- activateWallets endpoints
  sequence_
    [ callEndpoint @"give" (hs ! 1) $
        GiveParam
          { gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2,
            gpDeadline = slotToBeginPOSIXTime def 20,
            gpAmount = 30_000_000
          },
      wait1,
      callEndpoint @"give" (hs ! 1) $
        GiveParam
          { gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 4,
            gpDeadline = slotToBeginPOSIXTime def 20,
            gpAmount = 30_000_000
          },
      wait1,
      callEndpoint @"grab" (hs ! 2) (), -- deadline not reached
      void $ waitUntilSlot 20,
      callEndpoint @"grab" (hs ! 3) (), -- wrong beneficiary
      wait1,
      callEndpoint @"grab" (hs ! 4) () -- collect gift
    ]

exports :: ContractExports -- Prepare exports for jamb CLI:
exports = exportValidatorWithTest validator [] test 4