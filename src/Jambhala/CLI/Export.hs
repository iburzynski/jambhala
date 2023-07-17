{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jambhala.CLI.Export where

import Codec.Serialise (Serialise (..), serialise)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import Data.Char (isUpper, toLower)
import Jambhala.CLI.Emulator (notImplemented)
import Jambhala.CLI.Types
import Jambhala.Plutus

type JambContract = (String, ContractExports)

exportContract :: (IsScript (JambScript s)) => ContractTemplate s -> JambContract
exportContract c = (contractName c, ContractExports (toScriptExport $ jambScript' c) (dataExports c) (emulatorTest c))

toKebab :: String -> String
toKebab "" = ""
toKebab (x : xs) = toLower x : foldr go "" xs
  where
    go y acc
      | isUpper y = '-' : toLower y : acc
      | otherwise = y : acc

getSerialised :: Serialise a => a -> PlutusScript PlutusScriptV2
getSerialised = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise

data ContractTemplate s = ContractTemplate
  { dataExports :: ![DataExport],
    emulatorTest :: !EmulatorTest,
    contractName :: !ContractName,
    jambScript' :: !(JambScript s)
  }

withScript :: IsScript (JambScript s) => ContractName -> s -> ContractTemplate s
withScript n s = ContractTemplate [] notImplemented n (JambScript s)