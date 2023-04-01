{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Jambhala.Utils
 ( ContractExports
 , Contracts
 , JambEmulatorTrace
 , activateWallets
 , exportMintingPolicy
 , exportMintingPolicyWithTest
 , exportValidator
 , exportValidatorWithTest
 , getContractAddress
 , getMainnetAddress
 , getTestnetAddress
 , wait1
 , wrap
 , writePlutusFile ) where

import Prelude hiding ( Enum(..), AdditiveSemigroup(..), Monoid(..), Ord(..), Traversable(..), (<$>), error )

import Jambhala.CLI ( scriptAddressBech32, writePlutusFile )
import Jambhala.CLI.Emulator ( activateWallets, wait1 )
import Jambhala.CLI.Types
import Jambhala.Haskell hiding ( ask )
import Jambhala.Plutus hiding (Mainnet, Testnet, lovelaceValueOf )

import Cardano.Ledger.BaseTypes ( Network(..) )
import Cardano.Binary ( serialize' )
import Cardano.Node.Emulator ( Params(..) )
import Data.Aeson ( encode )
import Plutus.Contract.Request ( getParams )
import qualified Data.ByteString.Base16   as B16
import qualified Data.ByteString.Lazy     as BSL

-- | Temporary replacement for the removed `mkUntypedValidator` function
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

export :: Maybe EmulatorExport -> (s -> JambScript) -> s -> ContractExports
export Nothing constructor s = flip ContractExports Nothing $ constructor s
export test@(Just (EmulatorExport _ numWallets)) constructor s
  | numWallets > 0 = ContractExports{..}
  where script = constructor s
export _ _ _ = error "Wallet quantity must be greater than zero"

exportNoTest :: (s -> JambScript) -> s -> ContractExports
exportNoTest = export Nothing

exportWithTest :: (s -> JambScript) -> s -> JambEmulatorTrace -> WalletQuantity -> ContractExports
exportWithTest constructor s jTrace numWallets = export (Just EmulatorExport{..}) constructor s

-- | Exports a validator for use with jamb CLI
--
-- To export with an emulator test, use `exportValidatorWithTest`
exportValidator :: Validator -> ContractExports
exportValidator = exportNoTest JambValidator

-- | Exports a minting policy for use with jamb CLI
--
-- To export with an emulator test, use `exportMintingPolicyWithTest`
exportMintingPolicy :: MintingPolicy -> ContractExports
exportMintingPolicy = exportNoTest JambMintingPolicy

-- | Exports a validator and emulator test for use with jamb CLI
exportValidatorWithTest :: Validator -> JambEmulatorTrace -> WalletQuantity -> ContractExports
exportValidatorWithTest = exportWithTest JambValidator

-- | Exports a minting policy and emulator test for use with jamb CLI
exportMintingPolicyWithTest :: MintingPolicy -> JambEmulatorTrace -> WalletQuantity -> ContractExports
exportMintingPolicyWithTest = exportWithTest JambMintingPolicy

viewCBOR :: PlutusScript PlutusScriptV2 -> ByteString
viewCBOR = B16.encode . serialize'

getTestnetAddress :: JambScript -> String
getTestnetAddress = scriptAddressBech32 Testnet

getMainnetAddress :: JambScript -> String
getMainnetAddress = scriptAddressBech32 Mainnet

-- Leftover util functions from ADA Philippines starter:
encodePlutusData :: ToData a => a -> BSL.ByteString
encodePlutusData a = encode . scriptDataToJson ScriptDataJsonDetailedSchema
                   . fromPlutusData . builtinDataToData $ toBuiltinData a

-- scriptHash :: V1Scripts.Script -> V1Scripts.ScriptHash
-- scriptHash =
--   V1Scripts.ScriptHash
--     . toBuiltin
--     . serialiseToRawBytes
--     . hashScript
--     . toCardanoApiScript