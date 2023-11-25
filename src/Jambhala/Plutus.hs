-- Required for using `.\/` in schema type declaration
{-# LANGUAGE TypeOperators #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Jambhala.Plutus (
  Ada (..),
  Address (..),
  AddressInEra,
  AsContractError,
  AssetClass (..),
  BabbageEra,
  CardanoAddress,
  CompiledCode,
  Contract,
  ContractHandle,
  Credential (..),
  CurrencySymbol (..),
  Datum (..),
  DatumFromQuery,
  DatumHash,
  DecoratedTxOut (..),
  EmulatorEffects,
  EmulatorTrace,
  Endpoint,
  Error (..),
  FromData (..),
  Interval (..),
  Language (..),
  MintingPolicy (..),
  Network,
  NetworkId (..),
  NetworkMagic (..),
  OutputDatum (..),
  Params (..),
  PaymentPubKeyHash (..),
  PlutusScript (..),
  PlutusScriptV2,
  PlutusScriptVersion (..),
  POSIXTime,
  MintingPolicyHash,
  Promise (..),
  PubKeyHash (..),
  Redeemer (..),
  ScriptDataJsonSchema (..),
  ScriptContext (..),
  ScriptLookups (..),
  SerialiseAsRawBytes (..),
  Slot,
  StakeReference (..),
  ToData (..),
  TokenName (..),
  TxConstraints,
  TxId (..),
  TxInInfo (..),
  TxInfo (..),
  TxOut (..),
  TxOutRef (..),
  UnsafeFromData (..),
  UntypedMintingPolicy,
  UntypedValidator,
  Validator (..),
  ValidatorHash,
  ValidatorTypes (..),
  Value,
  Versioned (..),
  type (.\/),
  activateContractWallet,
  adaSymbol,
  adaToken,
  adaValueOf,
  applyCode,
  assetClass,
  assetClassValue,
  assetClassValueOf,
  awaitTxConfirmed,
  builtinDataToData,
  callEndpoint,
  compile,
  contains,
  currentNodeClientTimeRange,
  datumInDatumFromQuery,
  decoratedTxOutDatum,
  divideInteger,
  endpoint,
  findOwnInput,
  flattenValue,
  from,
  fromPlutusData,
  fromPlutusInterval,
  getContinuingOutputs,
  getCardanoTxId,
  getParams,
  getUnspentOutput,
  hashScript,
  interval,
  knownWallet,
  liftCode,
  logInfo,
  lovelaceValueOf,
  makeIsDataIndexed,
  makeLift,
  minAdaTxOutEstimated,
  mintingPolicyHash,
  mkI,
  mkForwardingMintingPolicy,
  mkMintingPolicyCardanoAddress,
  mkMintingPolicyScript,
  mkValidatorCardanoAddress,
  mkValidatorScript,
  mockWalletPaymentPubKeyHash,
  mustBeSignedBy,
  mustMintValue,
  mustMintValueWithRedeemer,
  mustPayToOtherScriptWithDatumInTx,
  mustPayToPubKey,
  mustSpendPubKeyOutput,
  mustSpendScriptOutput,
  mustValidateInTimeRange,
  ownCurrencySymbol,
  ownFirstPaymentPubKeyHash,
  ownUtxos,
  plutusV2OtherScript,
  plutusV2MintingPolicy,
  pubKeyHashAddress,
  runEmulatorTraceIO,
  runEmulatorTraceIOWithConfig,
  scriptCurrencySymbol,
  scriptDataToJson,
  scriptHashAddress,
  select,
  serialiseData,
  serialiseToBech32,
  singleton,
  slotToBeginPOSIXTime,
  slotToEndPOSIXTime,
  stringToBuiltinByteString,
  submitTxConstraintsWith,
  to,
  toData,
  tokenName,
  toPubKeyHash,
  toShelleyScriptHash,
  toTxInfoTxOut,
  toValidatorHash,
  txSignedBy,
  unitDatum,
  unitRedeemer,
  unMintingPolicyScript,
  unspentOutputs,
  unstableMakeIsData,
  utxosAt,
  validatorHash,
  valueOf,
  waitNSlots,
  waitUntilSlot,
  writeFileTextEnvelope,
)
where

import Cardano.Api (AddressInEra, BabbageEra, NetworkId (..), NetworkMagic (..))
import Cardano.Api.Shelley (
  Address (..),
  Error (..),
  PlutusScript (..),
  PlutusScriptV2,
  PlutusScriptVersion (..),
  ScriptDataJsonSchema (..),
  SerialiseAsRawBytes (..),
  fromPlutusData,
  hashScript,
  scriptDataToJson,
  serialiseToBech32,
  toShelleyScriptHash,
  writeFileTextEnvelope,
 )
import Cardano.Ledger.BaseTypes (Network)
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import Cardano.Node.Emulator (Params (..), slotToBeginPOSIXTime, slotToEndPOSIXTime)
import Ledger (CardanoAddress, DecoratedTxOut (..), Language (..), PaymentPubKeyHash (..), Slot, TxOutRef, Versioned (..), adaValueOf, contains, datumInDatumFromQuery, decoratedTxOutDatum, from, getCardanoTxId, interval, minAdaTxOutEstimated, mkValidatorCardanoAddress, to, toTxInfoTxOut, toValidatorHash, unitDatum, unitRedeemer)
import Ledger.Tx (DatumFromQuery)
import Ledger.Tx.Constraints (ScriptLookups (..), TxConstraints, mustBeSignedBy, mustMintValue, mustMintValueWithRedeemer, mustPayToOtherScriptWithDatumInTx, mustPayToPubKey, mustSpendPubKeyOutput, mustSpendScriptOutput, mustValidateInTimeRange, plutusV2MintingPolicy, plutusV2OtherScript, unspentOutputs)
import Ledger.Tx.Constraints.ValidityInterval (fromPlutusInterval)
import Plutus.Contract (AsContractError, Contract, Endpoint, Promise (..), awaitTxConfirmed, currentNodeClientTimeRange, endpoint, logInfo, ownFirstPaymentPubKeyHash, ownUtxos, select, submitTxConstraintsWith, type (.\/))
import Plutus.Contract.Request (getParams, getUnspentOutput, utxosAt)
import Plutus.Script.Utils.Ada (Ada (..), lovelaceValueOf)
import Plutus.Script.Utils.Typed (UntypedMintingPolicy, ValidatorTypes (..))
import Plutus.Script.Utils.V2.Address (mkMintingPolicyCardanoAddress)
import Plutus.Script.Utils.V2.Contexts (TxOut (..), findOwnInput, getContinuingOutputs)
import Plutus.Script.Utils.V2.Scripts (mintingPolicyHash, scriptCurrencySymbol, validatorHash)
import Plutus.Script.Utils.V2.Typed.Scripts (UntypedValidator, mkForwardingMintingPolicy)
import Plutus.Script.Utils.Value (
  AssetClass (..),
  assetClass,
  assetClassValue,
  assetClassValueOf,
 )
import Plutus.Trace (
  ContractHandle,
  EmulatorConfig,
  EmulatorEffects,
  EmulatorTrace,
  TraceConfig,
  activateContractWallet,
  callEndpoint,
  runEmulatorTraceIO,
  runEmulatorTraceIO',
  waitNSlots,
  waitUntilSlot,
 )
import Plutus.V1.Ledger.Address (scriptHashAddress, toPubKeyHash)
import Plutus.V1.Ledger.Address as Address (
  pubKeyHashAddress,
 )
import Plutus.V1.Ledger.Value (
  TokenName (..),
  adaSymbol,
  adaToken,
  flattenValue,
  tokenName,
  valueOf,
 )
import Plutus.V2.Ledger.Api (CurrencySymbol (..), Datum (..), DatumHash, Interval (..), MintingPolicy (..), MintingPolicyHash, OutputDatum (..), POSIXTime, PubKeyHash (..), Redeemer (..), ScriptContext (..), ToData (..), TxId (..), TxInInfo (..), TxOutRef (..), Validator (..), ValidatorHash, Value, mkMintingPolicyScript, mkValidatorScript, singleton, toData, unMintingPolicyScript)
import Plutus.V2.Ledger.Contexts (TxInfo (..), ownCurrencySymbol, txSignedBy)
import PlutusTx (
  CompiledCode,
  FromData (..),
  UnsafeFromData (..),
  applyCode,
  builtinDataToData,
  compile,
  liftCode,
  makeIsDataIndexed,
  makeLift,
  unstableMakeIsData,
 )
import PlutusTx.Builtins (divideInteger, mkI, serialiseData)
import PlutusTx.Builtins.Class (stringToBuiltinByteString)
import Wallet.Emulator (knownWallet, mockWalletPaymentPubKeyHash)

-- | Temporary replacement for deprecated function (not yet exported by Plutus.Trace.Emulator)
runEmulatorTraceIOWithConfig :: TraceConfig -> EmulatorConfig -> EmulatorTrace () -> IO ()
runEmulatorTraceIOWithConfig = runEmulatorTraceIO'
