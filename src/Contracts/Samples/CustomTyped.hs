{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-} -- Required for using `.\/` in schema type declaration

module Contracts.Samples.CustomTyped where

{- Occasionally we need to:
   a.) Hide things from the PlutusTx Prelude to prevent clashes with their Haskell equivalents.
   b.) Hide things from Jambhala.Haskell to prevent clashes with their Plutus equivalents.  -}
import Prelude          hiding ( Semigroup(..), mconcat )
import Jambhala.Haskell hiding ((<$>), (==))

import Jambhala.Plutus
import Jambhala.Utils

-- Non-Plutus imports:

import Text.Printf ( printf )
import qualified Control.Monad.Freer.Extras as Extras
import qualified Data.Map.Strict as Map

newtype Redeem = Redeem { guess :: Integer }
-- `unstableMakeIsData` uses TemplateHaskell to generate ToData/FromData instances for a custom type
-- These classes have toBuiltInData/fromBuiltInData methods to convert between Haskell/Plutus data.
unstableMakeIsData ''Redeem

-- makeIsDataIndexed ''Redeemer [('Redeemer, 0)]
-- (requires importing `makeIsDataIndexed` from Plutus.Tx)

customTyped :: () -> Redeem -> ScriptContext -> Bool
customTyped _ (Redeem i) _ = traceIfFalse "Sorry, wrong guess!" (i == 42)
{-# INLINABLE customTyped #-}

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrapped ||])
  where wrapped = wrap customTyped

-- Off-chain Code

-- 1. Define endpoints schema
type GiftSchema =
      Endpoint "give" Integer
  .\/ Endpoint "grab" Integer


-- 2. Create Cardano.CLI address
address :: AddressInEra BabbageEra
address = mkValidatorCardanoAddress (Testnet $ NetworkMagic 1) $ Versioned validator PlutusV2

-- 3. give endpoint: send UTXOs to the script address
give :: AsContractError e => Integer -> Contract w s e ()
give q = do
  logInfo @String $ printf "Start of the give action"
  submittedTxId <- getCardanoTxId <$> submitTxConstraintsWith @Void lookups constraints
  awaitTxConfirmed submittedTxId
  logInfo @String $ printf "Made transaction of %d ADA" q
  where
    vHash       = validatorHash validator
    datum       = Datum $ toBuiltinData ()
    lookups     = plutusV2OtherScript validator
    constraints = mustPayToOtherScriptWithDatumInTx vHash datum $ lovelaceValueOf q

-- 4. grab endpoint: consume UTXOs at the script address
grab :: AsContractError e => Integer -> Contract w s e ()
grab 42 = do
    utxos <- utxosAt address
    let lookups     = unspentOutputs utxos <> plutusV2OtherScript validator
        orefs       = Map.keys utxos
        redeemer    = Redeemer . toBuiltinData $ Redeem 42
        constraints = mconcat $ map (`mustSpendScriptOutput` redeemer) orefs
    submittedTxId <- getCardanoTxId <$> submitTxConstraintsWith @Void lookups constraints
    awaitTxConfirmed submittedTxId
    logInfo @String "collected gifts"
grab _ = logInfo @String $ "Wrong guess"

-- 5. Create endpoint listener
endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" grab

-- 6. Define emulator trace test
test :: EmulatorTrace ()
test = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"give" h1 20000000
    void $ waitUntilSlot 20
    callEndpoint @"grab" h2 100
    void $ waitNSlots 2
    callEndpoint @"grab" h2 42
    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s

exports :: ContractExports -- Prepare exports for jamb CLI:
exports = ContractExports { getValidator = validator, getTest = Just test }