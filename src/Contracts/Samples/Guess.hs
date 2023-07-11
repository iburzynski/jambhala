module Contracts.Samples.Guess where

import qualified Data.Map.Strict as Map
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
validator = mkValidatorScript $$(compile [||wrapped||])
  where
    wrapped = mkUntypedValidator guess

data Guessing

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

  give :: GiveAction Guessing
  give (Give q a) = do
    submittedTxId <- getCardanoTxId <$> submitTxConstraintsWith @Guessing lookups constraints
    _ <- awaitTxConfirmed submittedTxId
    logString $ printf "Gave %d lovelace" q
    where
      vHash = validatorHash validator
      datum = Datum . toBuiltinData $ Answer a
      lookups = plutusV2OtherScript validator
      constraints = mustPayToOtherScriptWithDatumInTx vHash datum $ lovelaceValueOf q

  grab :: GrabAction Guessing
  grab (Grab g) = do
    addr <- getContractAddress validator
    validUtxos <- Map.mapMaybe hasMatchingDatum <$> utxosAt addr
    if Map.null validUtxos
      then logString "No matching UTXOs"
      else do
        let lookups = unspentOutputs validUtxos <> plutusV2OtherScript validator
            redeemer = Redeemer . toBuiltinData $ Guess g
            constraints = mconcat . map (`mustSpendScriptOutput` redeemer) $ Map.keys validUtxos
        submittedTxId <- getCardanoTxId <$> submitTxConstraintsWith @Guessing lookups constraints
        awaitTxConfirmed submittedTxId
        logString "Collected gifts"
    where
      -- ref: https://github.com/input-output-hk/plutus-apps/blob/main/plutus-ledger/src/Ledger/Tx.hs
      hasMatchingDatum :: DecoratedTxOut -> Maybe DecoratedTxOut
      hasMatchingDatum dto = do
        (_, dfq) <- getDecoratedTxOutDatum dto
        Datum d <- getDatumInDatumFromQuery dfq
        Answer a <- fromBuiltinData d
        guard (g == a) -- terminates with Nothing if guess doesn't match answer
        Just dto -- else return the matching utxo

test :: EmulatorTest
test =
  initEmulator @Guessing
    6
    [ Give {lovelace = 10_000_000, withAnswer = 42} `fromWallet` 1,
      Give {lovelace = 10_000_000, withAnswer = 42} `fromWallet` 2,
      Give {lovelace = 10_000_000, withAnswer = 21} `fromWallet` 3,
      Grab {withGuess = 33} `toWallet` 4,
      Grab {withGuess = 21} `toWallet` 5,
      Grab {withGuess = 42} `toWallet` 6
    ]

exports :: JambContract -- Prepare exports for jamb CLI:
exports = exportValidatorWithTest "guess" validator [] test