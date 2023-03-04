{-# LANGUAGE TypeOperators #-} -- Required for using `.\/` in schema type declaration

module Jambhala.Plutus (
    Address
  , AddressInEra
  , AsContractError
  , BabbageEra
  , CompiledCode
  , Contract
  , Datum(..)
  , DecoratedTxOut(..)
  , EmulatorTrace
  , Endpoint
  , Error(..)
  , FromData(..)
  , Language(..)
  , NetworkId(..)
  , NetworkMagic(..)
  , PlutusScript(..)
  , PlutusScriptV2
  , PlutusScriptVersion(..)
  , Promise(..)
  , Redeemer(..)
  , Script(..)
  , ScriptDataJsonSchema(..)
  , ScriptContext
  , SerialiseAsRawBytes(..)
  , ToData(..)
  , TxOutRef
  , UnsafeFromData(..)
  , Validator
  , ValidatorHash
  , ValidatorTypes(..)
  , Versioned(..)
  , type (.\/)
  , activateContractWallet
  , awaitTxConfirmed
  , builtinDataToData
  , callEndpoint
  , compile
  , datumInDatumFromQuery
  , decoratedTxOutDatum
  , endpoint
  , fromPlutusData
  , getCardanoTxId
  , hashScript
  , knownWallet
  , logInfo
  , lovelaceValueOf
  , mkValidatorCardanoAddress
  , mkValidatorScript
  , mustPayToOtherScriptWithDatumInTx
  , mustSpendScriptOutput
  , plutusV2OtherScript
  , runEmulatorTraceIO
  , scriptHashAddress
  , scriptDataToJson
  , select
  , submitTxConstraintsWith
  , validatorHash
  , unspentOutputs
  , unstableMakeIsData
  , utxosAt
  , waitNSlots
  , waitUntilSlot
  , writeFileTextEnvelope
) where

import Cardano.Api ( AddressInEra, BabbageEra, NetworkId(..), NetworkMagic (..) )
import Cardano.Api.Shelley
  ( Error(..), PlutusScript(..), PlutusScriptV2, PlutusScriptVersion(..), Script(..)
  , ScriptDataJsonSchema(..), SerialiseAsRawBytes(..)
  , fromPlutusData, hashScript, scriptDataToJson, writeFileTextEnvelope )
import Ledger
  ( DecoratedTxOut(..), Language (..), TxOutRef, Versioned (..)
  , datumInDatumFromQuery, decoratedTxOutDatum, getCardanoTxId, mkValidatorCardanoAddress )
import Ledger.Tx.Constraints
  ( mustPayToOtherScriptWithDatumInTx, mustSpendScriptOutput, plutusV2OtherScript, unspentOutputs )
import Plutus.Contract
  ( AsContractError, Contract, Endpoint, Promise (..), type (.\/)
  , logInfo, submitTxConstraintsWith,  awaitTxConfirmed, select, endpoint )
import Plutus.Contract.Request ( utxosAt )
import Plutus.Script.Utils.Ada ( lovelaceValueOf )
import Plutus.Script.Utils.Typed (ValidatorTypes(..))
import Plutus.Script.Utils.V2.Scripts ( validatorHash )
import Plutus.Trace
  ( EmulatorTrace
  , activateContractWallet, callEndpoint, runEmulatorTraceIO, waitNSlots, waitUntilSlot )
import Plutus.V1.Ledger.Address ( Address, scriptHashAddress )
import Plutus.V2.Ledger.Api
  ( Datum(..), Redeemer (..), ToData(..), Validator, ValidatorHash, mkValidatorScript )
import Plutus.V2.Ledger.Contexts ( ScriptContext )
import PlutusTx
  ( CompiledCode, FromData(..), UnsafeFromData(..), builtinDataToData, compile, unstableMakeIsData)
import Wallet.Emulator ( knownWallet )