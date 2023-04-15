{-# LANGUAGE TypeOperators #-} -- Required for using `.\/` in schema type declaration

module Jambhala.Plutus (
    Address(..)
  , AddressInEra
  , AsContractError
  , BabbageEra
  , CompiledCode
  , Contract
  , ContractHandle
  , Credential(..)
  , CurrencySymbol(..)
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
  , MintingPolicy(..)
  , Network
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
  , StakeReference(..)
  , ToData(..)
  , TxInfo(..)
  , TxOutRef
  , UnsafeFromData(..)
  , Validator(..)
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
  , makeIsDataIndexed
  , makeLift
  , mkI
  , mkMintingPolicyScript
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
  , scriptCurrencySymbol
  , scriptDataToJson
  , scriptHashAddress
  , select
  , serialiseToBech32
  , slotToBeginPOSIXTime
  , submitTxConstraintsWith
  , toShelleyScriptHash
  , txSignedBy
  , unitDatum
  , unitRedeemer
  , unMintingPolicyScript
  , unspentOutputs
  , unstableMakeIsData
  , utxosAt
  , validatorHash
  , waitNSlots
  , waitUntilSlot
  , writeFileTextEnvelope
) where

import Cardano.Api ( AddressInEra, BabbageEra, NetworkId(..), NetworkMagic(..) )
import Cardano.Api.Shelley
  ( Address(..), Error(..), PlutusScript(..), PlutusScriptV2, PlutusScriptVersion(..), Script(..)
  , ScriptDataJsonSchema(..), SerialiseAsRawBytes(..)
  , fromPlutusData, hashScript, scriptDataToJson, serialiseToBech32, toShelleyScriptHash
  , writeFileTextEnvelope )
import Cardano.Ledger.BaseTypes ( Network )
import Cardano.Ledger.Credential ( Credential(..), StakeReference(..) )
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
import Plutus.Script.Utils.V2.Scripts ( scriptCurrencySymbol, validatorHash )
import Plutus.Trace
  ( ContractHandle, EmulatorConfig, EmulatorEffects, EmulatorTrace, TraceConfig
  , activateContractWallet, callEndpoint, runEmulatorTraceIO, runEmulatorTraceIO', waitNSlots
  , waitUntilSlot )
import Plutus.V1.Ledger.Address ( scriptHashAddress )
import Plutus.V2.Ledger.Api
  ( CurrencySymbol(..), Datum(..), Interval(..), MintingPolicy(..), POSIXTime, Redeemer (..)
  , ScriptContext(..), ToData(..), Validator(..), ValidatorHash
  , mkMintingPolicyScript, mkValidatorScript, unMintingPolicyScript )
import Plutus.V2.Ledger.Contexts ( TxInfo(..), txSignedBy )
import PlutusTx
  ( CompiledCode, FromData(..), UnsafeFromData(..)
  , applyCode, builtinDataToData, compile, liftCode, makeIsDataIndexed, makeLift, unstableMakeIsData )
import PlutusTx.Builtins (mkI)
import Wallet.Emulator ( knownWallet, mockWalletPaymentPubKeyHash )

import System.IO (IO)

-- | Temporary replacement for deprecated function (not yet exported by Plutus.Trace.Emulator)
runEmulatorTraceIOWithConfig :: TraceConfig -> EmulatorConfig -> EmulatorTrace () -> IO ()
runEmulatorTraceIOWithConfig = runEmulatorTraceIO'