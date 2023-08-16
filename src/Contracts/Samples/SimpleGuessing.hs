-- Used for post-fix style qualified imports:
{-# LANGUAGE ImportQualifiedPost #-}

module Contracts.Samples.SimpleGuessing where

import Data.Map qualified as Map
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

type Guessing = ValidatorContract "guessing"

contract :: Guessing
contract = mkValidatorContract $$(compile [||untyped||])
  where
    untyped :: UntypedValidator
    untyped = mkUntypedValidator guess

instance ValidatorEndpoints Guessing where
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
        { lookups = scriptLookupsFor contract,
          constraints = mustPayToScriptWithDatum contract (Answer a) q
        }
    logStr $ printf "Gave %d lovelace" q

  grab :: GrabParam Guessing -> ContractM Guessing ()
  grab (Grab g) = do
    utxos <- getUtxosAt contract
    let validUtxos = Map.mapMaybe hasMatchingDatum utxos
    if validUtxos == mempty
      then logStr "No matching UTXOs"
      else do
        submitAndConfirm
          Tx
            { lookups = scriptLookupsFor contract `andUtxos` validUtxos,
              constraints = validUtxos `mustAllBeSpentWith` Guess g
            }
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
  initEmulator @Guessing
    6
    [ Give {lovelace = 10_000_000, withAnswer = 42} `fromWallet` 1,
      Give {lovelace = 10_000_000, withAnswer = 42} `fromWallet` 2,
      Give {lovelace = 10_000_000, withAnswer = 21} `fromWallet` 3,
      Grab {withGuess = 33} `toWallet` 4,
      Grab {withGuess = 21} `toWallet` 5,
      Grab {withGuess = 42} `toWallet` 6
    ]

exports :: JambExports -- Prepare exports for jamb CLI:
exports =
  export
    (defExports contract)
      { emulatorTest = test
      }