{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jambhala.CLI.Export where

import Codec.Serialise (Serialise (..), serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as BSS
import Jambhala.CLI.Emulator (notImplemented)
import Jambhala.CLI.Types
import Jambhala.Plutus

{- | A pair containing a contract's name and its exported data.
  `JambExports` values can be imported into the `Contracts` module and added to the list of contracts for use with the `jamb` CLI.
-}
type JambExports = (ContractName, ContractExports)

{- | An intermediary record type used to prepare scripts and their associated data for
  conversion to a `JambExports` value.
-}
data ExportTemplate = ExportTemplate
  { contractName :: !ContractName
  , scriptExport :: !ScriptExport
  , dataExports :: ![DataExport]
  , emulatorTest :: !EmulatorTest
  }

{- | The default `ExportTemplate` constructor: includes only the contract script with no data exports or emulator test.
  The template can optionally be extended using record update syntax, i.e.
  @
  (defExports contract) {
    dataExports = myValue `toJSONfile` "my-value",
    emulatorTest = test
    }
  @
-}
defExports :: (IsScript contract) => contract -> ExportTemplate
defExports c = ExportTemplate (getContractName c) (toScriptExport c) [] notImplemented

{- | Converts an `ExportTemplate` into a `JambExports` value,
  which can be imported into the `Contracts` module and added to the list of contracts for use with the `jamb` CLI.
-}
export :: ExportTemplate -> JambExports
export c = (contractName c, ContractExports (scriptExport c) (dataExports c) (emulatorTest c))

-- | Serialises a value to a V2 `PlutusScript`.
getSerialised :: (Serialise a) => a -> PlutusScript PlutusScriptV2
getSerialised = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise

-- | Creates a `DataExport` value to be serialised into a JSON file with the given name.
toJSONfile :: (ToData d) => d -> FileName -> DataExport
d `toJSONfile` filename = DataExport filename d
