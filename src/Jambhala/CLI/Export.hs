{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jambhala.CLI.Export where

import Codec.Serialise (Serialise (..), serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Data.Char (isUpper, toLower)
import Data.Typeable (typeOf)
import Jambhala.CLI.Emulator (EmulatorTest, notImplemented)
import Jambhala.Plutus

type ContractName = String

type JambContract = (String, ContractExports)

data JambScript
  = JambValidator !Validator
  | JambMintingPolicy !MintingPolicy
  deriving (Generic)

instance Serialise JambScript where
  encode (JambValidator v) = encode v
  encode (JambMintingPolicy p) = encode p

data ContractExports = ContractExports
  { script :: !JambScript,
    dExports :: ![DataExport],
    test :: !EmulatorTest
  }

class ToJambScript s where
  toJambScript :: s -> JambScript

instance ToJambScript Validator where
  toJambScript = JambValidator

instance ToJambScript MintingPolicy where
  toJambScript = JambMintingPolicy

class (Typeable c) => Exportable c where
  exportScript :: c -> JambScript
  exportTest :: c -> EmulatorTest
  exportData :: c -> [DataExport]
  exportData _ = []
  exportName :: c -> ContractName
  exportName c = show $ typeOf c
  exportContract :: c -> JambContract
  exportContract c = (exportName c, ContractExports (exportScript c) (exportData c) (exportTest c))

toKebab :: String -> String
toKebab "" = ""
toKebab (x : xs) = toLower x : foldr go "" xs
  where
    go y acc
      | isUpper y = '-' : toLower y : acc
      | otherwise = y : acc

getSerialised :: Serialise a => a -> PlutusScript PlutusScriptV2
getSerialised = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise

data DataExport where
  DataExport :: (ToData a) => String -> a -> DataExport

instance ToData DataExport where
  toBuiltinData (DataExport _ x) = toBuiltinData x

data ContractTemplate s = ContractTemplate
  { dataExports :: [DataExport],
    emulatorTest :: EmulatorTest,
    contractName :: ContractName,
    validatorOrMintingPolicy :: s
  }

instance (Typeable s, ToJambScript s) => Exportable (ContractTemplate s) where
  exportScript = toJambScript . validatorOrMintingPolicy
  exportTest = emulatorTest
  exportData = dataExports
  exportName = contractName

withScript :: (ToJambScript s) => ContractName -> s -> ContractTemplate s
withScript = ContractTemplate [] notImplemented