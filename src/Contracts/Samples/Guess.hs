-- Required for JSON conversion of give/grab params:
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Contracts.Samples.Guess where

-- Required for JSON conversion of give/grab params
import Control.Lens ((^?)) -- Used for retrieving datum from utxos
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map.Strict as Map
import Jambhala.Haskell hiding (Functor (..), (<$>), (==))
import Jambhala.Plutus
import Jambhala.Utils
import Text.Printf (printf)
import Prelude hiding (Applicative (..), Semigroup (..), Traversable (..), mconcat)

-- Datum
newtype Answer = Answer {getAnswer :: Integer}

unstableMakeIsData ''Answer

-- Redeemer
newtype Guess = Guess {getGuess :: Integer}

unstableMakeIsData ''Guess

guess :: Answer -> Guess -> ScriptContext -> Bool
guess (Answer a) (Guess g) _ = traceIfFalse "Sorry, wrong guess!" (g == a)
{-# INLINEABLE guess #-}

validator :: Validator
validator = mkValidatorScript $$(compile [||wrapped||])
  where
    wrapped = mkUntypedValidator guess

data VTypes

instance ValidatorTypes VTypes where
  type DatumType VTypes = Answer
  type RedeemerType VTypes = Guess

data GiveParam = GiveParam
  { giveAmount :: !Integer,
    giveAnswer :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON)

newtype GrabParam = GrabParam {grabGuess :: Integer}
  deriving (Generic, ToJSON, FromJSON)

type Schema =
  Endpoint "give" GiveParam
    .\/ Endpoint "grab" GrabParam

give :: GiveParam -> Contract () Schema Text ()
give (GiveParam q a) = do
  submittedTxId <- getCardanoTxId <$> submitTxConstraintsWith @VTypes lookups constraints
  _ <- awaitTxConfirmed submittedTxId
  logInfo @String $ printf "Gave %d lovelace" q
  where
    vHash = validatorHash validator
    datum = Datum . toBuiltinData $ Answer a
    lookups = plutusV2OtherScript validator
    constraints = mustPayToOtherScriptWithDatumInTx vHash datum $ lovelaceValueOf q

grab :: GrabParam -> Contract () Schema Text ()
grab (GrabParam g) = do
  addr <- getContractAddress validator
  validUtxos <- Map.mapMaybe hasMatchingDatum <$> utxosAt addr
  if Map.null validUtxos
    then logInfo @String $ "No matching UTXOs"
    else do
      let lookups = unspentOutputs validUtxos <> plutusV2OtherScript validator
          redeemer = Redeemer . toBuiltinData $ Guess g
          constraints = mconcat . map (`mustSpendScriptOutput` redeemer) $ Map.keys validUtxos
      submittedTxId <- getCardanoTxId <$> submitTxConstraintsWith @VTypes lookups constraints
      awaitTxConfirmed submittedTxId
      logInfo @String "Collected gifts"
  where
    -- ref: https://github.com/input-output-hk/plutus-apps/blob/main/plutus-ledger/src/Ledger/Tx.hs
    hasMatchingDatum :: DecoratedTxOut -> Maybe DecoratedTxOut
    hasMatchingDatum dto = do
      (_, dfq) <- dto ^? decoratedTxOutDatum
      Datum d <- dfq ^? datumInDatumFromQuery
      Answer a <- fromBuiltinData d
      guard (g == a) -- terminates with Nothing if guess doesn't match answer
      Just dto -- else return the matching utxo

endpoints :: Contract () Schema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" grab

test :: JambEmulatorTrace
test = do
  hs <- activateWallets endpoints
  sequence_
    [ callEndpoint @"give" (hs ! 1) $ GiveParam 10_000_000 42,
      wait1,
      callEndpoint @"give" (hs ! 2) $ GiveParam 10_000_000 42,
      wait1,
      callEndpoint @"give" (hs ! 3) $ GiveParam 10_000_000 21,
      wait1,
      callEndpoint @"grab" (hs ! 4) $ GrabParam 33,
      wait1,
      callEndpoint @"grab" (hs ! 5) $ GrabParam 21,
      wait1,
      callEndpoint @"grab" (hs ! 6) $ GrabParam 42
    ]

exports :: ContractExports -- Prepare exports for jamb CLI:
exports = exportValidatorWithTest validator [] test 6