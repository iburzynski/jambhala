{-# LANGUAGE TypeOperators #-} -- Required for using `.\/` in schema type declaration

module Jambhala.Plutus (
    Address
  , AddressInEra
  , AsContractError
  , BabbageEra
  , CompiledCode
  , Contract
  , Datum(..)
  , EmulatorTrace
  , Endpoint
  , Error(..)
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
  , UnsafeFromData(..)
  , Validator
  , ValidatorHash
  , Versioned(..)
  , type (.\/)
  , activateContractWallet
  , awaitTxConfirmed
  , builtinDataToData
  , callEndpoint
  , compile
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

import PlutusTx ( CompiledCode, UnsafeFromData(..), builtinDataToData, compile, unstableMakeIsData)
import Plutus.V1.Ledger.Address ( Address, scriptHashAddress )
import Plutus.Script.Utils.V2.Scripts ( validatorHash )
import Plutus.V2.Ledger.Contexts ( ScriptContext )
import Cardano.Api ( AddressInEra, BabbageEra, NetworkId(..), NetworkMagic (..) )
import Ledger ( Language (..), Versioned (..), getCardanoTxId, mkValidatorCardanoAddress )
import Ledger.Tx.Constraints
  ( mustPayToOtherScriptWithDatumInTx, mustSpendScriptOutput, plutusV2OtherScript, unspentOutputs )
import Plutus.Contract
    ( AsContractError, Contract, Endpoint, Promise (..), type (.\/)
    , logInfo, submitTxConstraintsWith,  awaitTxConfirmed, select, endpoint )
import Plutus.Contract.Request ( utxosAt )
import Plutus.Script.Utils.Ada ( lovelaceValueOf )
import Plutus.V2.Ledger.Api ( Datum(..), Redeemer (..), ToData(..), Validator, ValidatorHash, mkValidatorScript )
import Plutus.Trace
  ( activateContractWallet, callEndpoint, runEmulatorTraceIO, waitNSlots, waitUntilSlot, EmulatorTrace )
import Wallet.Emulator ( knownWallet )
import Cardano.Api.Shelley
    ( hashScript,
      PlutusScriptV2,
      PlutusScriptVersion(PlutusScriptV2),
      PlutusScript(..),
      Script(PlutusScript),
      scriptDataToJson,
      writeFileTextEnvelope,
      Error(displayError),
      ScriptDataJsonSchema(ScriptDataJsonDetailedSchema),
      SerialiseAsRawBytes(serialiseToRawBytes),
      fromPlutusData )