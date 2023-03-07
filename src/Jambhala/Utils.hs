{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Jambhala.Utils
 ( ContractExports
 , Contracts
 , JambEmulatorTrace
 , activateWallets
 , exportValidator
 , exportValidatorWithTest
 , getContractAddress
 , wait1
 , wrap
 , writePlutusFile ) where

import Prelude hiding ( Enum(..), AdditiveSemigroup(..), Monoid(..), Ord(..), Traversable(..), (<$>), error )

import Jambhala.CLI ( writePlutusFile )
import Jambhala.CLI.Emulator ( activateWallets, wait1 )
import Jambhala.CLI.Types
import Jambhala.Haskell hiding ( ask )
import Jambhala.Plutus hiding ( lovelaceValueOf )

import Cardano.Binary ( serialize' )
import Cardano.Node.Emulator ( Params(..) )
import Codec.Serialise ( serialise )
import Data.Aeson ( encode )
import Plutus.Contract.Request ( getParams )
import qualified Data.ByteString.Base16   as B16
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.ByteString.Short    as BSS
import qualified Plutus.V1.Ledger.Scripts as V1Scripts

-- | Replacement for the removed `mkUntypedValidator` function
wrap :: (UnsafeFromData d, UnsafeFromData r)
     => (d -> r -> ScriptContext -> Bool)
     -> (BuiltinData -> BuiltinData -> BuiltinData -> ())
wrap f d r sc = check $ f (ufbid d) (ufbid r) (ufbid sc)
  where
    ufbid :: UnsafeFromData a => BuiltinData -> a
    ufbid = unsafeFromBuiltinData
{-# INLINABLE wrap #-}

getContractAddress :: AsContractError e => Validator -> Contract w s e (AddressInEra BabbageEra)
getContractAddress v = do
  nId <- pNetworkId <$> getParams
  return $ mkValidatorCardanoAddress nId $ Versioned v PlutusV2

-- | Exports a validator for use with jamb CLI
--
-- To export with an emulator test, use `exportValidatorWithTest`
exportValidator :: Validator -> ContractExports
exportValidator v = ContractExports v Nothing

-- | Exports a validator and emulator test for use with jamb CLI
exportValidatorWithTest :: Validator -> JambEmulatorTrace -> WalletQuantity -> ContractExports
exportValidatorWithTest validator jTrace numWallets | numWallets > 0 = ContractExports{..}
  where test = Just EmulatorExport{..}
exportValidatorWithTest _ _ _ = error "Wallet quantity must be greater than zero"

viewCBOR :: PlutusScript PlutusScriptV2 -> ByteString
viewCBOR = B16.encode . serialize'

-- Leftover util functions from ADA Philippines starter:
encodePlutusData :: ToData a => a -> BSL.ByteString
encodePlutusData a = encode . scriptDataToJson ScriptDataJsonDetailedSchema
                   . fromPlutusData . builtinDataToData $ toBuiltinData a

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