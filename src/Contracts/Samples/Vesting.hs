{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Contracts.Samples.Vesting where

import Prelude hiding ( Applicative(..), Semigroup(..), Eq(..), Traversable(..), (<$>), mconcat )
import Jambhala.Plutus
import Jambhala.Haskell
import Jambhala.Utils

import Control.Lens ( (^?) )
import Data.Aeson ( ToJSON, FromJSON )
import Data.Default ( Default(..) )
import Text.Printf ( printf )
import qualified Data.Map.Strict as Map

-- Datum
data VestingDatum = VestingDatum {
    getBeneficiary :: PaymentPubKeyHash
  , getDeadline    :: POSIXTime }
unstableMakeIsData ''VestingDatum

vesting :: VestingDatum -> () -> ScriptContext -> Bool
vesting (VestingDatum ben dline) _ sc =  traceIfFalse "Wrong pubkey hash"    signedByBeneficiary
                                      && traceIfFalse "Deadline not reached" dlineReached
  where
    txInfo              = scriptContextTxInfo sc
    signedByBeneficiary = txSignedBy txInfo $ unPaymentPubKeyHash ben
    dlineReached        = contains (from dline) $ txInfoValidRange txInfo
{-# INLINABLE vesting #-}

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrapped ||])
  where wrapped = wrap vesting

data VTypes
instance ValidatorTypes VTypes where
    type instance DatumType VTypes    = VestingDatum
    type instance RedeemerType VTypes = ()

data GiveParams = GiveParams
     { gpBeneficiary :: !PaymentPubKeyHash
     , gpDeadline    :: !POSIXTime
     , gpAmount      :: !Integer
     } deriving (Generic, ToJSON, FromJSON)

type Schema =
      Endpoint "give" GiveParams
  .\/ Endpoint "grab" ()

give :: GiveParams -> Contract () Schema Text ()
give (GiveParams{..}) = do
  submittedTxId <- getCardanoTxId <$> submitTxConstraintsWith @VTypes lookups constraints
  _ <- awaitTxConfirmed submittedTxId
  logInfo @String $ printf "Made a gift of %d lovelace to %s with deadline %s"
    gpAmount (show gpBeneficiary) (show gpDeadline)
  where
    vHash = validatorHash validator
    datum = Datum . toBuiltinData $ VestingDatum gpBeneficiary gpDeadline
    lookups = plutusV2OtherScript validator
    constraints = mustPayToOtherScriptWithDatumInTx vHash datum $ lovelaceValueOf gpAmount

grab :: () -> Contract () Schema Text ()
grab _ = do
  now        <- uncurry interval <$> currentNodeClientTimeRange
  pkh        <- ownFirstPaymentPubKeyHash
  addr       <- getContractAddress validator
  validUtxos <- Map.mapMaybe (isEligible pkh now) <$> utxosAt addr
  if Map.null validUtxos then logInfo @String $ "No eligible gifts available"
    else do
      let lookups     = unspentOutputs validUtxos <> plutusV2OtherScript validator
          constraints = mconcat
                      $ mustValidateInTimeRange (fromPlutusInterval now)
                      : mustBeSignedBy pkh
                      : map (`mustSpendScriptOutput` unitRedeemer) (Map.keys validUtxos)
      submittedTx <- getCardanoTxId <$> submitTxConstraintsWith @VTypes lookups constraints
      _           <- awaitTxConfirmed submittedTx
      logInfo @String "Collected eligible gifts"
  where
    isEligible :: PaymentPubKeyHash -> Interval POSIXTime -> DecoratedTxOut -> Maybe DecoratedTxOut
    isEligible pkh now dto = do
      (_, dfq)               <- dto ^? decoratedTxOutDatum
      Datum d                <- dfq ^? datumInDatumFromQuery
      VestingDatum ben dline <- fromBuiltinData d
      guard (ben == pkh && from dline `contains` now)
      Just dto

endpoints :: Contract () Schema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" grab

test :: EmulatorTrace ()
test = do
  hs <- traverse ((`activateContractWallet` endpoints) . knownWallet) [1 .. 4]
  case hs of
    [h1, h2, h3, h4] ->
      sequence_ [
          callEndpoint @"give" h1 $ GiveParams {
            gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2
          , gpDeadline    = slotToBeginPOSIXTime def 20
          , gpAmount      = 30000000
          }
        , wait1
        , callEndpoint @"give" h1 $ GiveParams {
             gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 4
           , gpDeadline    = slotToBeginPOSIXTime def 20
           , gpAmount      = 30000000
           }
        , wait1
        , callEndpoint @"grab" h2 () -- deadline not reached
        , void $ waitUntilSlot 20
        , callEndpoint @"grab" h3 () -- wrong beneficiary
        , wait1
        , callEndpoint @"grab" h4 () -- collect gift
        ]
    _ -> pure ()

exports :: ContractExports -- Prepare exports for jamb CLI:
exports = ContractExports { getValidator = validator, getTest = Just test }