{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Jambhala.Utils
  ( ContractExports,
    ContractPromise,
    JambContract,
    JambContracts,
    Emulatable (..),
    DataExport (..),
    EmulatorParams,
    EmulatorTest,
    (!),
    activateWallets,
    exportMintingPolicy,
    exportMintingPolicyWithTest,
    exportValidator,
    exportValidatorWithTest,
    fromWallet,
    getContractAddress,
    getDatumInDatumFromQuery,
    getDecoratedTxOutDatum,
    initEmulator,
    logString,
    mkEmulatorParams,
    mkEndpoints,
    pkhForWallet,
    toWallet,
    wait,
    waitUntil,
    wrap,
  )
where

import Cardano.Node.Emulator (Params (..))
import Control.Lens ((^?))
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

export :: Maybe EmulatorTest -> (s -> JambScript) -> String -> s -> [DataExport] -> JambContract
export Nothing constructor name s des = (name, ContractExports (constructor s) des Nothing)
export test constructor name s dataExports = (name, ContractExports {..})
  where
    script = constructor s

exportNoTest :: (s -> JambScript) -> String -> s -> [DataExport] -> JambContract
exportNoTest = export Nothing

exportWithTest ::
  (s -> JambScript) ->
  String ->
  s ->
  [DataExport] ->
  EmulatorTest ->
  JambContract
exportWithTest constructor name s dataExports test =
  export (Just test) constructor name s dataExports

-- | Exports a validator for use with jamb CLI
--
-- To export with an emulator test, use `exportValidatorWithTest`
exportValidator :: String -> Validator -> [DataExport] -> JambContract
exportValidator = exportNoTest JambValidator

-- | Exports a minting policy for use with jamb CLI
--
-- To export with an emulator test, use `exportMintingPolicyWithTest`
exportMintingPolicy :: String -> MintingPolicy -> [DataExport] -> JambContract
exportMintingPolicy = exportNoTest JambMintingPolicy

-- | Exports a validator and emulator test for use with jamb CLI
exportValidatorWithTest :: String -> Validator -> [DataExport] -> EmulatorTest -> JambContract
exportValidatorWithTest = exportWithTest JambValidator

-- | Exports a minting policy and emulator test for use with jamb CLI
exportMintingPolicyWithTest :: String -> MintingPolicy -> [DataExport] -> EmulatorTest -> JambContract
exportMintingPolicyWithTest = exportWithTest JambMintingPolicy

-- | Call `logInfo` with input type fixed to String
logString :: String -> Contract w s e ()
logString = logInfo @String

-- | A non-lens version of the `decoratedTxOutDatum` getter
getDecoratedTxOutDatum :: DecoratedTxOut -> Maybe (DatumHash, DatumFromQuery)
getDecoratedTxOutDatum dto = dto ^? decoratedTxOutDatum

-- | A non-lens version of the `datumInDatumFromQuery` getter
getDatumInDatumFromQuery :: DatumFromQuery -> Maybe Datum
getDatumInDatumFromQuery dfq = dfq ^? datumInDatumFromQuery