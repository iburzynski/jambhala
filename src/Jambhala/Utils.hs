{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Jambhala.Utils
  ( ContractExports,
    ContractPromise,
    ContractTemplate (..),
    JambContract,
    JambContracts,
    Emulatable (..),
    Exportable (..),
    DataExport (..),
    EmulatorTest,
    (!),
    activateWallets,
    fromWallet,
    getContractAddress,
    getDatumInDatumFromQuery,
    getDecoratedTxOutDatum,
    initEmulator,
    logString,
    mkEndpoints,
    pkhForWallet,
    toWallet,
    wait,
    waitUntil,
    withScript,
    wrap,
  )
where

import Cardano.Node.Emulator (Params (..))
import Control.Lens ((^?))
import Jambhala.CLI.Emulator
  ( EmulatorTest,
    activateWallets,
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
import Jambhala.CLI.Export (ContractExports, ContractTemplate (..), DataExport (..), Exportable (..), JambContract, withScript)
import Jambhala.CLI.Types
import Jambhala.Plutus
import Ledger.Tx (DatumFromQuery)
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

getContractAddress :: AsContractError e => Validator -> Contract w s e (AddressInEra BabbageEra)
getContractAddress v = do
  nId <- pNetworkId <$> getParams
  return $ mkValidatorCardanoAddress nId $ Versioned v PlutusV2

-- | A non-lens version of the `decoratedTxOutDatum` getter
getDecoratedTxOutDatum :: DecoratedTxOut -> Maybe (DatumHash, DatumFromQuery)
getDecoratedTxOutDatum dto = dto ^? decoratedTxOutDatum

-- | A non-lens version of the `datumInDatumFromQuery` getter
getDatumInDatumFromQuery :: DatumFromQuery -> Maybe Datum
getDatumInDatumFromQuery dfq = dfq ^? datumInDatumFromQuery

-- | `logInfo` with the input type as String
logString :: String -> Contract w s e ()
logString = logInfo @String