{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Jambhala.Utils
  ( ContractExports,
    ContractPromise,
    ContractTemplate (..),
    JambContract,
    JambContracts,
    Emulatable (..),
    DataExport (..),
    EmulatorTest,
    Transaction (..),
    (!),
    activateWallets,
    andUtxos,
    exportContract,
    fromWallet,
    getContractAddress,
    getCurrentInterval,
    getUtxosAt,
    getDatumInDatumFromQuery,
    getDecoratedTxOutDatum,
    initEmulator,
    logStr,
    lookupUtxos,
    submitAndConfirm,
    mkDatum,
    mkEndpoints,
    mkRedeemer,
    mustPayToScriptWithDatum,
    pkhForWallet,
    toWallet,
    wait,
    waitUntil,
    withScript,
    wrap,
    wrapValidator,
  )
where

import Cardano.Api (TxId)
import Cardano.Node.Emulator (Params (..))
import Control.Lens ((^?))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Jambhala.CLI.Emulator
  ( activateWallets,
    fromWallet,
    initEmulator,
    mkEndpoints,
    pkhForWallet,
    toWallet,
    wait,
    waitUntil,
    (!),
  )
import Jambhala.CLI.Emulator.Types (ContractPromise, Emulatable (..))
import Jambhala.CLI.Export (ContractTemplate (..), JambContract, exportContract, withScript)
import Jambhala.CLI.Types
import Jambhala.Plutus
import Ledger.Tx (DatumFromQuery)
import Ledger.Tx.Constraints (TxConstraints)
import Plutus.Contract.Request (getParams)
import Plutus.V2.Ledger.Api (DatumHash)

-- | Temporary replacement for the removed `mkUntypedValidator` function
wrap ::
  (UnsafeFromData d, UnsafeFromData r) =>
  (d -> r -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinData -> ())
wrap f d r sc = check $ f (ufbid d) (ufbid r) (ufbid sc)
  where
    ufbid :: UnsafeFromData a => BuiltinData -> a
    ufbid = unsafeFromBuiltinData
{-# INLINEABLE wrap #-}

{-# INLINEABLE wrapValidator #-}
wrapValidator ::
  ( UnsafeFromData d,
    UnsafeFromData r
  ) =>
  (d -> r -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinData -> ())
wrapValidator f a b ctx =
  check $
    f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData b)
      (unsafeFromBuiltinData ctx)

getContractAddress :: (IsScript (JambScript' script), AsContractError e) => script -> Contract w s e (AddressInEra BabbageEra)
getContractAddress script = do
  nId <- pNetworkId <$> getParams
  pure . addressFunc nId $ JambScript script

getUtxosAt :: (IsScript (JambScript' script), AsContractError e) => script -> Contract w s e (Map TxOutRef DecoratedTxOut)
getUtxosAt script = getContractAddress script >>= utxosAt

getCurrentInterval :: AsContractError e => Contract w s e (Interval POSIXTime)
getCurrentInterval = uncurry interval <$> currentNodeClientTimeRange

-- | A non-lens version of the `decoratedTxOutDatum` getter
getDecoratedTxOutDatum :: DecoratedTxOut -> Maybe (DatumHash, DatumFromQuery)
getDecoratedTxOutDatum dto = dto ^? decoratedTxOutDatum

-- | A non-lens version of the `datumInDatumFromQuery` getter
getDatumInDatumFromQuery :: DatumFromQuery -> Maybe Datum
getDatumInDatumFromQuery dfq = dfq ^? datumInDatumFromQuery

-- | `logInfo` with the input type as String
logStr :: String -> Contract w s e ()
logStr = logInfo @String

mkDatum :: ToData a => a -> Datum
mkDatum = Datum . toBuiltinData

mkRedeemer :: ToData a => a -> Redeemer
mkRedeemer = Redeemer . toBuiltinData

andUtxos :: ScriptLookups c -> Map TxOutRef DecoratedTxOut -> ScriptLookups c
andUtxos scriptLookups utxos = scriptLookups <> unspentOutputs utxos

lookupUtxos :: Map TxOutRef DecoratedTxOut -> ScriptLookups a
lookupUtxos = unspentOutputs

-- lookupScript :: Validator -> ScriptLookups a
-- lookupScript = plutusV2OtherScript

mustPayToScriptWithDatum :: ToData datum => Validator -> datum -> Integer -> TxConstraints rType dType
mustPayToScriptWithDatum validator datum lovelace =
  mustPayToOtherScriptWithDatumInTx
    (validatorHash validator)
    (mkDatum datum)
    (lovelaceValueOf lovelace)

mustSpendUtxoFromScript :: ToData redeemer => TxOutRef -> redeemer -> TxConstraints rType dType
mustSpendUtxoFromScript utxo redeemer = mustSpendScriptOutput utxo $ mkRedeemer redeemer

mustSpendUtxosFromScript :: ToData redeemer => Map TxOutRef DecoratedTxOut -> redeemer -> TxConstraints rType dType
mustSpendUtxosFromScript utxos redeemer = mconcatMap (`mustSpendUtxoFromScript` redeemer) txrefs
  where
    txrefs = Map.keys utxos

submitAndConfirm ::
  forall contract.
  ( ToData (RedeemerType contract),
    FromData (DatumType contract),
    ToData (DatumType contract),
    ContractM contract () ~ Contract () (Schema contract) Text ()
  ) =>
  Transaction contract ->
  ContractM contract () -- (Schema contract) Text ()
submitAndConfirm Tx {..} = submit >>= awaitTxConfirmed
  where
    submit :: Contract () (Schema contract) Text TxId
    submit =
      getCardanoTxId
        <$> submitTxConstraintsWith @contract @() @(Schema contract) @Text lookups constraints