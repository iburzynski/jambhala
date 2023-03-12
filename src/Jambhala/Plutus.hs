{-# LANGUAGE TypeOperators #-} -- Required for using `.\/` in schema type declaration

module Jambhala.Plutus (
    Address
  , AddressInEra
  , AsContractError
  , BabbageEra
  , CompiledCode
  , Contract
  , ContractHandle
  , Datum(..)
  , DecoratedTxOut(..)
  , EmulatorEffects
  , EmulatorTrace
  , Endpoint
  , Error(..)
  , FromData(..)
  , Interval(..)
  , IsScriptContext(..)
  , Language(..)
  , NetworkId(..)
  , NetworkMagic(..)
  , PaymentPubKeyHash(..)
  , PlutusScript(..)
  , PlutusScriptV2
  , PlutusScriptVersion(..)
  , POSIXTime
  , Promise(..)
  , Redeemer(..)
  , Script(..)
  , ScriptDataJsonSchema(..)
  , ScriptContext(..)
  , SerialiseAsRawBytes(..)
  , ToData(..)
  , TxInfo(..)
  , TxOutRef
  , UnsafeFromData(..)
  , Validator
  , ValidatorHash
  , ValidatorTypes(..)
  , Versioned(..)
  , type (.\/)
  , activateContractWallet
  , applyCode
  , awaitTxConfirmed
  , builtinDataToData
  , callEndpoint
  , compile
  , contains
  , currentNodeClientTimeRange
  , datumInDatumFromQuery
  , decoratedTxOutDatum
  , endpoint
  , from
  , fromPlutusData
  , fromPlutusInterval
  , getCardanoTxId
  , hashScript
  , interval
  , knownWallet
  , liftCode
  , logInfo
  , lovelaceValueOf
  , makeLift
  , mkValidatorCardanoAddress
  , mkValidatorScript
  , mockWalletPaymentPubKeyHash
  , mustBeSignedBy
  , mustPayToOtherScriptWithDatumInTx
  , mustSpendScriptOutput
  , mustValidateInTimeRange
  , ownFirstPaymentPubKeyHash
  , plutusV2OtherScript
  , runEmulatorTraceIO
  , runEmulatorTraceIOWithConfig
  , scriptHashAddress
  , scriptDataToJson
  , select
  , slotToBeginPOSIXTime
  , submitTxConstraintsWith
  , txSignedBy
  , unitDatum
  , unitRedeemer
  , unspentOutputs
  , unstableMakeIsData
  , utxosAt
  , validatorHash
  , waitNSlots
  , waitUntilSlot
  , writeFileTextEnvelope
) where

import Cardano.Api ( AddressInEra, BabbageEra, NetworkId(..), NetworkMagic (..) )
import Cardano.Api.Shelley
  ( Error(..), PlutusScript(..), PlutusScriptV2, PlutusScriptVersion(..), Script(..)
  , ScriptDataJsonSchema(..), SerialiseAsRawBytes(..)
  , fromPlutusData, hashScript, scriptDataToJson, writeFileTextEnvelope )
import Cardano.Node.Emulator ( slotToBeginPOSIXTime )
import Ledger
  ( DecoratedTxOut(..), Language (..), PaymentPubKeyHash(..), TxOutRef, Versioned (..)
  , contains, datumInDatumFromQuery, decoratedTxOutDatum, from, getCardanoTxId, interval
  , mkValidatorCardanoAddress, unitDatum, unitRedeemer )
import Ledger.Tx.Constraints
  ( mustBeSignedBy, mustPayToOtherScriptWithDatumInTx, mustSpendScriptOutput
  , mustValidateInTimeRange, plutusV2OtherScript, unspentOutputs )
import Ledger.Tx.Constraints.ValidityInterval ( fromPlutusInterval )
import Ledger.Typed.Scripts (IsScriptContext(..))
import Plutus.Contract
  ( AsContractError, Contract, Endpoint, Promise (..), type (.\/)
  , currentNodeClientTimeRange, logInfo, ownFirstPaymentPubKeyHash, submitTxConstraintsWith
  , awaitTxConfirmed, select, endpoint )
import Plutus.Contract.Request ( utxosAt )
import Plutus.Script.Utils.Ada ( lovelaceValueOf )
import Plutus.Script.Utils.Typed ( ValidatorTypes(..) )
import Plutus.Script.Utils.V2.Scripts ( validatorHash )
import Plutus.Trace
  ( ContractHandle, EmulatorConfig, EmulatorEffects, EmulatorTrace, TraceConfig
  , activateContractWallet, callEndpoint, runEmulatorTraceIO, runEmulatorTraceIO', waitNSlots
  , waitUntilSlot )
import Plutus.V1.Ledger.Address ( Address, scriptHashAddress )
import Plutus.V2.Ledger.Api
  ( Datum(..), Interval(..), POSIXTime, Redeemer (..), ScriptContext(..), ToData(..), Validator
  , ValidatorHash, mkValidatorScript )
import Plutus.V2.Ledger.Contexts ( TxInfo(..), txSignedBy )
import PlutusTx
  ( CompiledCode, FromData(..), UnsafeFromData(..)
  , applyCode, builtinDataToData, compile, liftCode, makeLift, unstableMakeIsData )
import Wallet.Emulator ( knownWallet, mockWalletPaymentPubKeyHash )

import System.IO (IO)

-- | Temporary replacement for deprecated function (not yet exported by Plutus.Trace.Emulator)
runEmulatorTraceIOWithConfig :: TraceConfig -> EmulatorConfig -> EmulatorTrace () -> IO ()
runEmulatorTraceIOWithConfig = runEmulatorTraceIO'