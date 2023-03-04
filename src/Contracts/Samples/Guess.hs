{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}  -- Required for using `.\/` in schema type declaration
{-# LANGUAGE DeriveGeneric #-}  -- Required for JSON conversion of give/grab params
{-# LANGUAGE DeriveAnyClass #-} -- Required for JSON conversion of give/grab params

module Contracts.Samples.Guess where

import Prelude          hiding ( Semigroup(..), mconcat, Applicative(..), Traversable(..) )
import Jambhala.Haskell hiding ( (<$>), (==), Functor(..) )
import Jambhala.Plutus
import Jambhala.Utils

import Data.Aeson ( ToJSON, FromJSON ) -- Required for JSON conversion of give/grab params
import Control.Lens ( (^?) ) -- Used for retrieving datum from utxos
import Text.Printf ( printf )
import qualified Data.Map.Strict as Map

-- Datum
newtype Answer = Answer { getAnswer :: Integer }
unstableMakeIsData ''Answer
-- Redeemer
newtype Guess  = Guess { getGuess :: Integer }
unstableMakeIsData ''Guess

guess :: Answer -> Guess -> ScriptContext -> Bool
guess (Answer a) (Guess g) _ = traceIfFalse "Sorry, wrong guess!" (g == a)
{-# INLINABLE guess #-}

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrapped ||])
  where wrapped = wrap guess

data GiveParams = GiveParams {
      giveAmount :: !Integer
    , giveAnswer :: !Integer }
  deriving (Generic, ToJSON, FromJSON)

newtype GrabParams = GrabParams { grabGuess :: Integer }
  deriving (Generic, ToJSON, FromJSON)

type GiftSchema =
      Endpoint "give" GiveParams
  .\/ Endpoint "grab" GrabParams

give :: AsContractError e => GiveParams -> Contract w s e ()
give (GiveParams q a) = do
  submittedTxId <- getCardanoTxId <$> submitTxConstraintsWith @Void lookups constraints
  awaitTxConfirmed submittedTxId
  logInfo @String $ printf "Gave %d ADA" q
  where
    vHash       = validatorHash validator
    datum       = Datum . toBuiltinData $ Answer a
    lookups     = plutusV2OtherScript validator
    constraints = mustPayToOtherScriptWithDatumInTx vHash datum $ lovelaceValueOf q

grab :: AsContractError e => GrabParams -> Contract w s e ()
grab (GrabParams g) = do
    utxos <- utxosAt $ mkPreviewAddress validator
    let orefs = mapMaybe hasMatchingDatum $ Map.toList utxos
    if null orefs then logInfo @String $ "No matching UTXOs"
      else do
        let lookups     = unspentOutputs utxos <> plutusV2OtherScript validator
            redeemer    = Redeemer . toBuiltinData $ Guess g
            constraints = mconcat $ map (`mustSpendScriptOutput` redeemer) orefs
        submittedTxId <- getCardanoTxId <$> submitTxConstraintsWith @Void lookups constraints
        awaitTxConfirmed submittedTxId
        logInfo @String "Collected gifts"
  where
    -- ref: https://github.com/input-output-hk/plutus-apps/blob/main/plutus-ledger/src/Ledger/Tx.hs
    hasMatchingDatum :: (TxOutRef, DecoratedTxOut) -> Maybe TxOutRef
    hasMatchingDatum (oref, dto) = do
      (_, dfq) <- dto ^? decoratedTxOutDatum
      Datum  d <- dfq ^? datumInDatumFromQuery
      Answer a <- fromBuiltinData d
      guard (g == a) -- terminates with Nothing if guess doesn't match answer
      pure oref -- else return the matching utxo

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" grab

test :: EmulatorTrace ()
test = do
  hs <- traverse ((`activateContractWallet` endpoints) . knownWallet) [1 .. 6]
  case hs of
    [h1, h2, h3, h4, h5, h6] -> do
      callEndpoint @"give" h1 $ GiveParams 10000000 42
      _ <- waitNSlots 1
      callEndpoint @"give" h2 $ GiveParams 10000000 42
      _ <- waitNSlots 1
      callEndpoint @"give" h3 $ GiveParams 10000000 21
      _ <- waitNSlots 1
      callEndpoint @"grab" h4 $ GrabParams 33
      _ <- waitNSlots 1
      callEndpoint @"grab" h5 $ GrabParams 21
      _ <- waitNSlots 1
      callEndpoint @"grab" h6 $ GrabParams 42
    _ -> pure ()

exports :: ContractExports -- Prepare exports for jamb CLI:
exports = ContractExports { getValidator = validator, getTest = Just test }