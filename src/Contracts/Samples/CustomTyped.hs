{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}  -- Required to map validator types for `submitTxConstraintsWith`
{-# LANGUAGE TypeOperators #-} -- Required for `.\/` (row merge) in endpoints schema definition

module Contracts.Samples.CustomTyped where

{- Occasionally we need to:
   a.) Hide things from the PlutusTx Prelude to prevent clashes with their Haskell equivalents.
   b.) Hide things from Jambhala.Haskell to prevent clashes with their Plutus equivalents.  -}
import Prelude          hiding ( Semigroup(..), mconcat )
import Jambhala.Haskell hiding ((<$>), (==))

import Jambhala.Plutus
import Jambhala.Utils

-- Other Haskell imports:
import Text.Printf ( printf )
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
type Schema =
      Endpoint "give" Integer
  .\/ Endpoint "grab" Integer

-- 2. Map validator types
data VTypes
instance ValidatorTypes VTypes where
  type instance DatumType    VTypes = ()
  type instance RedeemerType VTypes = Redeem

-- 3. give endpoint: send UTXOs to the script address
give :: Integer -> Contract () Schema Text ()
give q = do
  logInfo @String $ printf "Start of the give action"
  submittedTxId <- getCardanoTxId <$> submitTxConstraintsWith @VTypes lookups constraints
  awaitTxConfirmed submittedTxId
  logInfo @String $ printf "Made transaction of %d ADA" q
  where
    vHash       = validatorHash validator
    datum       = Datum $ toBuiltinData ()
    lookups     = plutusV2OtherScript validator
    constraints = mustPayToOtherScriptWithDatumInTx vHash datum $ lovelaceValueOf q

-- 4. grab endpoint: consume UTXOs at the script address
grab :: Integer -> Contract () Schema Text ()
grab 42 = do
    addr  <- getContractAddress validator
    utxos <- utxosAt addr
    let lookups     = unspentOutputs utxos <> plutusV2OtherScript validator
        orefs       = Map.keys utxos
        redeemer    = Redeemer . toBuiltinData $ Redeem 42
        constraints = mconcat $ map (`mustSpendScriptOutput` redeemer) orefs
    submittedTxId <- getCardanoTxId <$> submitTxConstraintsWith @VTypes lookups constraints
    awaitTxConfirmed submittedTxId
    logInfo @String "collected gifts"
grab _ = logInfo @String $ "Wrong guess"

-- 5. Define endpoints listener
endpoints :: Contract () Schema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" grab

-- 6. Define emulator trace test
test :: JambEmulatorTrace
test = do
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  h3 <- activateContractWallet (knownWallet 3) endpoints
  sequence_ [
      callEndpoint @"give" h1 42_000_000
    , wait1
    , callEndpoint @"grab" h2 21
    , wait1
    , callEndpoint @"grab" h3 42
    ]

-- Exports
redeemerSuccess :: DataExport
redeemerSuccess = DataExport "ctr42" $ Redeem 42

redeemerFail :: DataExport
redeemerFail = DataExport "ctr21" $ Redeem 21

exports :: ContractExports -- Prepare exports for jamb CLI:
exports = exportValidatorWithTest validator [redeemerSuccess, redeemerFail] test 3