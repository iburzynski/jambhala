{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Jambhala.Utils
  ( ContractExports,
    ContractM,
    ContractPromise,
    ContractTemplate (..),
    JambContract,
    JambContracts,
    Emulatable (..),
    DataExport (..),
    EmulatorTest,
    Transaction (..),
    (!),
    _CHANGE_ME_,
    activateWallets,
    andUtxos,
    defaultSlotBeginTime,
    defaultSlotEndTime,
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
    mustBeSpentWith,
    mustAllBeSpentWith,
    mustPayToScriptWithDatum,
    pkhForWallet,
    timestampToPOSIX,
    toJSONfile,
    toWallet,
    unsafeTimestampToPOSIX,
    wait,
    waitUntil,
    withScript,
  )
where

import Cardano.Api (TxId)
import Cardano.Node.Emulator (Params (..), slotToEndPOSIXTime)
import Control.Lens ((^?))
import Data.Default (def)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Time (defaultTimeLocale, parseTimeM)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Real (RealFrac (..))
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
import Jambhala.CLI.Emulator.Types (ContractM, ContractPromise, Emulatable (..), Schema)
import Jambhala.CLI.Export (ContractTemplate (..), JambContract, exportContract, withScript)
import Jambhala.CLI.Types
import Jambhala.Plutus
import Ledger.Tx (DatumFromQuery)
import Ledger.Tx.Constraints (TxConstraints)
import Plutus.Contract.Request (getParams)
import Plutus.V2.Ledger.Api (DatumHash)

-- | A value in a sample contract that must be replaced.
_CHANGE_ME_ :: a
_CHANGE_ME_ = error "Oops! You forgot to replace a _CHANGE_ME_ value!"

-- | Temporary replacement for the removed `mkUntypedValidator` function.
wrap ::
  (UnsafeFromData d, UnsafeFromData r) =>
  (d -> r -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinData -> ())
wrap f d r sc = check $ f (ufbid d) (ufbid r) (ufbid sc)
  where
    ufbid :: UnsafeFromData a => BuiltinData -> a
    ufbid = unsafeFromBuiltinData
{-# INLINEABLE wrap #-}

getContractAddress :: IsScript (JambScript script) => script -> ContractM contract (AddressInEra BabbageEra)
getContractAddress script = do
  nId <- pNetworkId <$> getParams
  pure . addressFunc nId $ JambScript script

getUtxosAt :: IsScript (JambScript script) => script -> ContractM contract (Map TxOutRef DecoratedTxOut)
getUtxosAt script = getContractAddress script >>= utxosAt

getCurrentInterval :: ContractM contract (Interval POSIXTime)
getCurrentInterval = uncurry interval <$> currentNodeClientTimeRange

-- | A non-lens version of the `decoratedTxOutDatum` getter
getDecoratedTxOutDatum :: DecoratedTxOut -> Maybe (DatumHash, DatumFromQuery)
getDecoratedTxOutDatum dto = dto ^? decoratedTxOutDatum

-- | A non-lens version of the `datumInDatumFromQuery` getter
getDatumInDatumFromQuery :: DatumFromQuery -> Maybe Datum
getDatumInDatumFromQuery dfq = dfq ^? datumInDatumFromQuery

-- | `logInfo` with the input type as String
logStr :: String -> ContractM contract ()
logStr = logInfo @String

mkDatum :: ToData a => a -> Datum
mkDatum = Datum . toBuiltinData

mkRedeemer :: ToData a => a -> Redeemer
mkRedeemer = Redeemer . toBuiltinData

andUtxos :: ScriptLookups contract -> Map TxOutRef DecoratedTxOut -> ScriptLookups contract
andUtxos scriptLookups utxos = scriptLookups <> unspentOutputs utxos

lookupUtxos :: Map TxOutRef DecoratedTxOut -> ScriptLookups contract
lookupUtxos = unspentOutputs

mustPayToScriptWithDatum :: ToData datum => Validator -> datum -> Integer -> TxConstraints rType dType
mustPayToScriptWithDatum validator datum lovelace =
  mustPayToOtherScriptWithDatumInTx
    (validatorHash validator)
    (mkDatum datum)
    (lovelaceValueOf lovelace)

mustBeSpentWith :: ToData redeemer => TxOutRef -> redeemer -> TxConstraints rType dType
utxo `mustBeSpentWith` redeemer = mustSpendScriptOutput utxo $ mkRedeemer redeemer

mustAllBeSpentWith :: ToData redeemer => Map TxOutRef DecoratedTxOut -> redeemer -> TxConstraints rType dType
utxos `mustAllBeSpentWith` redeemer = mconcatMap (`mustBeSpentWith` redeemer) $ Map.keys utxos

submitAndConfirm ::
  forall contract.
  ( FromData (DatumType contract),
    ToData (DatumType contract),
    ToData (RedeemerType contract)
  ) =>
  Transaction contract ->
  ContractM contract ()
submitAndConfirm Tx {..} = submit >>= awaitTxConfirmed
  where
    submit :: ContractM contract TxId
    submit =
      getCardanoTxId
        <$> submitTxConstraintsWith @contract @() @(Schema contract) @Text lookups constraints

toJSONfile :: (ToData d) => d -> FileName -> DataExport
d `toJSONfile` filename = DataExport filename d

defaultSlotBeginTime :: Slot -> POSIXTime
defaultSlotBeginTime = slotToBeginPOSIXTime def

defaultSlotEndTime :: Slot -> POSIXTime
defaultSlotEndTime = slotToEndPOSIXTime def

timestampToPOSIX :: String -> Maybe POSIXTime
timestampToPOSIX = fmap (floor . utcTimeToPOSIXSeconds) . parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S"

unsafeTimestampToPOSIX :: String -> POSIXTime
unsafeTimestampToPOSIX = fromJust . timestampToPOSIX