{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Jambhala.Utils where

import Jambhala.Plutus
import Jambhala.Haskell

import Cardano.Binary (serialize')
import Codec.Serialise ( serialise )
import Data.Aeson ( encode )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Base16 as B16
import qualified Plutus.V1.Ledger.Scripts as V1Scripts

type Contracts       = Map String ContractExports
data ContractExports = ContractExports { getValidator :: !Validator
                                       , getTest      :: !(Maybe (EmulatorTrace ())) }

-- Boilerplate version of the (removed) `mkUntypedValidator` function.
wrap :: (UnsafeFromData d, UnsafeFromData r)
     => (d -> r -> ScriptContext -> Bool)
     -> (BuiltinData -> BuiltinData -> BuiltinData -> ())
wrap f d r sc = check $ f (ufbid d) (ufbid r) (ufbid sc)
  where
    ufbid :: UnsafeFromData a => BuiltinData -> a
    ufbid = unsafeFromBuiltinData

getSerialised :: Validator -> PlutusScript PlutusScriptV2
getSerialised = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise

viewCBOR :: PlutusScript PlutusScriptV2 -> ByteString
viewCBOR = B16.encode . serialize'

scriptHash :: V1Scripts.Script -> V1Scripts.ScriptHash
scriptHash =
  V1Scripts.ScriptHash
    . toBuiltin
    . serialiseToRawBytes
    . hashScript
    . toCardanoApiScript

toCardanoApiScript :: V1Scripts.Script -> Script PlutusScriptV2
toCardanoApiScript =
  PlutusScript PlutusScriptV2
    . PlutusScriptSerialised
    . BSS.toShort
    . BSL.toStrict
    . serialise

writePlutusFile :: MonadIO m => String -> Validator -> m ()
writePlutusFile fileName validator =
  liftIO $ writeFileTextEnvelope fp Nothing (getSerialised validator) >>= \case
    Left err -> liftIO $ print $ displayError err
    Right () -> liftIO $ putStrLn $ "wrote validator to file " ++ fp
  where fp = "compiled/" ++ fileName ++ ".plutus"

encodePlutusData :: ToData a => a -> BSL.ByteString
encodePlutusData a = encode . scriptDataToJson ScriptDataJsonDetailedSchema
                   . fromPlutusData . builtinDataToData $ toBuiltinData a