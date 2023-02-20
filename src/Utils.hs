{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Utils ( wrap, getSerialised, viewCBOR, writePlutusFile, encodePlutusData ) where

import Cardano.Api.Shelley
import Codec.Serialise ( serialise )
import Data.Aeson ( encode )
import PlutusTx ( builtinDataToData, ToData (toBuiltinData), UnsafeFromData(..) )
import System.IO ( print, putStrLn )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Base16 as B16
import qualified Plutus.V1.Ledger.Scripts as Scripts
import Plutus.Script.Utils.V2.Scripts (Validator)
import Plutus.V2.Ledger.Contexts ( ScriptContext )
import Cardano.Binary (serialize')
import Data.String (String)
import Control.Monad.IO.Class (MonadIO(..))

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

scriptHash :: Scripts.Script -> Scripts.ScriptHash
scriptHash =
  Scripts.ScriptHash
    . toBuiltin
    . serialiseToRawBytes
    . hashScript
    . toCardanoApiScript

toCardanoApiScript :: Scripts.Script -> Script PlutusScriptV2
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