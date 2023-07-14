module Contracts.Samples.Guess where

import qualified Data.Map as Map
import Jambhala.Plutus
import Jambhala.Utils

-- Datum
newtype Answer = Answer Integer

-- Redeemer
newtype Guess = Guess Integer

unstableMakeIsData ''Answer
unstableMakeIsData ''Guess

guess :: Answer -> Guess -> ScriptContext -> Bool
guess (Answer a) (Guess g) _ = traceIfFalse "Sorry, wrong guess!" (g #== a)
{-# INLINEABLE guess #-}

validator :: Validator
validator = mkValidatorScript $$(compile [||untyped||])
  where
    untyped = mkUntypedValidator guess

data Guessing = THIS

instance ValidatorTypes Guessing where
  type DatumType Guessing = Answer
  type RedeemerType Guessing = Guess

instance Emulatable Guessing where
  data GiveParam Guessing = Give
    { lovelace :: !Integer,
      withAnswer :: !Integer
    }
    deriving (Generic, ToJSON, FromJSON)
  data GrabParam Guessing = Grab {withGuess :: Integer}
    deriving (Generic, ToJSON, FromJSON)

  give :: GiveParam Guessing -> ContractM Guessing ()
  give (Give q a) = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor THIS validator,
          constraints = mustPayToScriptWithDatum validator (Answer a) q
        }
    logStr $ printf "Gave %d lovelace" q

  grab :: GrabParam Guessing -> ContractM Guessing ()
  grab (Grab g) = do
    utxos <- getUtxosAt validator
    let validUtxos = Map.mapMaybe hasMatchingDatum utxos
    if Map.null validUtxos
      then logStr "No matching UTXOs"
      else do
        let lookups = scriptLookupsFor THIS validator `andUtxos` validUtxos
            redeemer = mkRedeemer $ Guess g
            constraints = mconcatMap (`mustSpendScriptOutput` redeemer) $ Map.keys validUtxos
        submitAndConfirm Tx {..}
        logStr "Collected gifts"
    where
      hasMatchingDatum :: DecoratedTxOut -> Maybe DecoratedTxOut
      hasMatchingDatum dto = do
        (_, dfq) <- getDecoratedTxOutDatum dto
        Datum d <- getDatumInDatumFromQuery dfq
        Answer a <- fromBuiltinData d
        guard (g == a) -- terminate with Nothing if guess doesn't match answer
        Just dto -- else return the matching utxo

test :: EmulatorTest
test =
  initEmulator
    6
    [ Give {lovelace = 10_000_000, withAnswer = 42} `fromWallet` 1,
      Give {lovelace = 10_000_000, withAnswer = 42} `fromWallet` 2,
      Give {lovelace = 10_000_000, withAnswer = 21} `fromWallet` 3,
      Grab {withGuess = 33} `toWallet` 4,
      Grab {withGuess = 21} `toWallet` 5,
      Grab {withGuess = 42} `toWallet` 6
    ]

exports :: JambContract -- Prepare exports for jamb CLI:
exports = exportContract ("guessing" `withScript` validator) {emulatorTest = test}