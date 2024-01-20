module Contracts.Samples.StateCounter where

import Contracts.Samples.NFT qualified as NFT
import Control.Monad (replicateM_)
import Data.Map qualified as Map
import Jambhala.Plutus
import Jambhala.Utils

-- 1. Declare Types

-- | Define a synonym for the datum.
type Count = Integer

-- 2. Define Helper Functions & Lambda

-- | Define typed counter lambda.
countLambda :: AssetClass -> Count -> () -> ScriptContext -> Bool
countLambda proofToken curCount _ sc = spendsProof proofToken sc && locksProof proofToken curCount sc
{-# INLINEABLE countLambda #-}

-- | Helper function: check that the validating input contains proof token
spendsProof :: AssetClass -> ScriptContext -> Bool
spendsProof proofToken sc = case findOwnInput sc of
  Nothing -> trace "Input not found" False
  Just (TxInInfo _ txOut) ->
    traceIfFalse "Proof token not spent" $
      assetClassValueOf (txOutValue txOut) proofToken #== 1
{-# INLINEABLE spendsProof #-}

-- | Helper function: check that a single output is locked to the script with proof token and valid state
locksProof :: AssetClass -> Count -> ScriptContext -> Bool
locksProof proofToken curCount sc = case getContinuingOutputs sc of
  [o] -> case (txOutDatum o, assetClassValueOf (txOutValue o) proofToken) of
    (OutputDatum (Datum d), qty) ->
      traceIfFalse "Proof token not locked" (qty #== 1)
        && traceIfFalse "Counter not incremented" (unsafeFromBuiltinData d #== curCount #+ 1)
    _noDatum -> trace "No inline datum on output" False
  [] -> trace "No continuing output" False
  _multipleOutputs -> trace "Multiple continuing outputs" False
{-# INLINEABLE locksProof #-}

-- | Convert lambda into "untyped" form before pre-compilation (:: BuiltinData -> BuiltinData -> BuiltinData -> ())
untypedLambda :: AssetClass -> UntypedValidator
untypedLambda proofToken = mkUntypedValidator (countLambda proofToken)
{-# INLINEABLE untypedLambda #-}

-- 3. Pre-Compile Lambda

-- | Declare contract synonym with unique symbolic identifier.
type StateCounter = ValidatorContract "state-counter"

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
compileValidator :: AssetClass -> StateCounter
compileValidator proofToken =
  mkValidatorContract ($$(compile [||untypedLambda||]) `applyCode` liftCode proofToken)

-- 4. Export Contract to Jambhala

-- | Define `exports` value for use with `j cli`.
exports :: JambExports
exports =
  export
    (defExports compiled)
      { dataExports =
          [(0 :: Count) `toJSONfile` "zero"]
      , emulatorTest = NFT.test <> test
      }
  where
    compiled =
      compileValidator
        -- Copy the currency symbol (policy ID) for an NFT
        (assetClass "c95a9fd1118088f71fd1438fdadce9cc0856f5a70631e7ee9c5c10af" "jambtoken")

-- 5. Define Emulator Component

-- | Define `ValidatorEndpoints` instance for contract synonym.
instance ValidatorEndpoints StateCounter where
  newtype GiveParam StateCounter = SetCount AssetClass
    deriving (Generic, FromJSON, ToJSON)
  newtype GrabParam StateCounter = IncCount AssetClass
    deriving (Generic, FromJSON, ToJSON)

  give :: GiveParam StateCounter -> ContractM StateCounter ()
  give (SetCount proofToken@(AssetClass (cs, tn))) = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor compiled
        , constraints =
            mustPayScriptWithInlineDatum @Count
              compiled
              0
              (lovelaceValueOf 2_000_000 <> singleton cs tn 1)
        }
    logStr "Counter initialized with value 0"
    where
      compiled :: StateCounter
      compiled = compileValidator proofToken

  grab :: GrabParam StateCounter -> ContractM StateCounter ()
  grab (IncCount proofToken@(AssetClass (cs, tn))) = do
    utxos <- getUtxosAt compiled
    let validUtxo = filterByFlatValue ((cs, tn, 1) `elem`) utxos
    case Map.toList validUtxo of
      [] -> logStr "No UTxO with proof token"
      (_ : _ : _) -> logStr "Multiple UTxOs with proof token"
      [(oref, txOut)] -> do
        let maybeOldState = convertDecoratedTxOutDatum @Count txOut
        case maybeOldState of
          Nothing -> logStr "Invalid datum"
          Just oldState -> do
            let newState = oldState + 1
            submitAndConfirm
              Tx
                { lookups = scriptLookupsFor compiled `andUtxos` validUtxo
                , constraints =
                    oref `mustBeSpentWith` ()
                      <> mustPayScriptWithInlineDatum
                        compiled
                        newState
                        (lovelaceValueOf 2_000_000 <> singleton cs tn 1)
                }
            logStr ("Counter incremented to " ++ show newState)
    where
      compiled = compileValidator proofToken

-- | Define emulator test.
test :: EmulatorTest
test =
  initEmulator @StateCounter
    1
    [ SetCount proofToken `fromWallet` 1
    , replicateM_ 100 (IncCount proofToken `toWallet` 1)
    ]
  where
    proofToken =
      -- Copy currency symbol from NFT emulator test output
      assetClass "639c6e743b9c35ef96ecc37d05f895188ea23d8da90e82f77f784107" "jambtoken"
