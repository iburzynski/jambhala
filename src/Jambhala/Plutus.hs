-- Required for using `.\/` in schema type declaration
{-# LANGUAGE TypeOperators #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fewer imports" #-}

module Jambhala.Plutus
  ( Address (..),
    AddressInEra,
    AsContractError,
    BabbageEra,
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
    IsScriptContext (..),
    Language (..),
    MintingPolicy (..),
    Network,
    NetworkId (..),
    NetworkMagic (..),
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
    TokenName,
    TxConstraints,
    TxId,
    TxInfo (..),
    TxOutRef,
    UnsafeFromData (..),
    UntypedValidator,
    Validator (..),
    ValidatorHash,
    ValidatorTypes (..),
    Value,
    Versioned (..),
    type (.\/),
    activateContractWallet,
    applyCode,
    awaitTxConfirmed,
    builtinDataToData,
    callEndpoint,
    compile,
    contains,
    currentNodeClientTimeRange,
    datumInDatumFromQuery,
    decoratedTxOutDatum,
    endpoint,
    from,
    fromPlutusData,
    fromPlutusInterval,
    getCardanoTxId,
    getParams,
    hashScript,
    interval,
    knownWallet,
    liftCode,
    logInfo,
    lovelaceValueOf,
    makeIsDataIndexed,
    makeLift,
    mintingPolicyHash,
    mkI,
    mkMintingPolicyCardanoAddress,
    mkMintingPolicyScript,
    mkValidatorCardanoAddress,
    mkValidatorScript,
    mockWalletPaymentPubKeyHash,
    mustBeSignedBy,
    mustMintValue,
    mustPayToOtherScriptWithDatumInTx,
    mustSpendScriptOutput,
    mustValidateInTimeRange,
    ownFirstPaymentPubKeyHash,
    plutusV2OtherScript,
    plutusV2MintingPolicy,
    runEmulatorTraceIO,
    runEmulatorTraceIOWithConfig,
    scriptCurrencySymbol,
    scriptDataToJson,
    scriptHashAddress,
    select,
    serialiseToBech32,
    singleton,
    slotToBeginPOSIXTime,
    slotToEndPOSIXTime,
    submitTxConstraintsWith,
    toData,
    toShelleyScriptHash,
    txSignedBy,
    unitDatum,
    unitRedeemer,
    unMintingPolicyScript,
    unspentOutputs,
    unstableMakeIsData,
    utxosAt,
    validatorHash,
    waitNSlots,
    waitUntilSlot,
    writeFileTextEnvelope,
  )
where

import Cardano.Api (AddressInEra, BabbageEra, NetworkId (..), NetworkMagic (..), TxId)
import Cardano.Api.Shelley
  ( Address (..),
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
import Ledger (DecoratedTxOut (..), Language (..), PaymentPubKeyHash (..), Slot, TxOutRef, Versioned (..), contains, datumInDatumFromQuery, decoratedTxOutDatum, from, getCardanoTxId, interval, mkValidatorCardanoAddress, unitDatum, unitRedeemer)
import Ledger.Tx (DatumFromQuery)
import Ledger.Tx.Constraints (ScriptLookups (..), TxConstraints, mustBeSignedBy, mustMintValue, mustPayToOtherScriptWithDatumInTx, mustSpendScriptOutput, mustValidateInTimeRange, plutusV2MintingPolicy, plutusV2OtherScript, unspentOutputs)
import Ledger.Tx.Constraints.ValidityInterval (fromPlutusInterval)
import Ledger.Typed.Scripts (IsScriptContext (..))
import Plutus.Contract
  ( AsContractError,
    Contract,
    Endpoint,
    Promise (..),
    awaitTxConfirmed,
    currentNodeClientTimeRange,
    endpoint,
    logInfo,
    ownFirstPaymentPubKeyHash,
    select,
    submitTxConstraintsWith,
    type (.\/),
  )
import Plutus.Contract.Request (getParams, utxosAt)
import Plutus.Script.Utils.Ada (lovelaceValueOf)
import Plutus.Script.Utils.Typed (ValidatorTypes (..))
import Plutus.Script.Utils.V2.Address (mkMintingPolicyCardanoAddress)
import Plutus.Script.Utils.V2.Scripts (mintingPolicyHash, scriptCurrencySymbol, validatorHash)
import Plutus.Script.Utils.V2.Typed.Scripts (UntypedValidator)
import Plutus.Trace
  ( ContractHandle,
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
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V2.Ledger.Api (CurrencySymbol (..), Datum (..), DatumHash, Interval (..), MintingPolicy (..), MintingPolicyHash, POSIXTime, PubKeyHash (..), Redeemer (..), ScriptContext (..), ToData (..), TokenName, Validator (..), ValidatorHash, Value, mkMintingPolicyScript, mkValidatorScript, singleton, toData, unMintingPolicyScript)
import Plutus.V2.Ledger.Contexts (TxInfo (..), txSignedBy)
import PlutusTx
  ( CompiledCode,
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
import PlutusTx.Builtins (mkI)
import Wallet.Emulator (knownWallet, mockWalletPaymentPubKeyHash)

-- | Temporary replacement for deprecated function (not yet exported by Plutus.Trace.Emulator)
runEmulatorTraceIOWithConfig :: TraceConfig -> EmulatorConfig -> EmulatorTrace () -> IO ()
runEmulatorTraceIOWithConfig = runEmulatorTraceIO'