{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Jambhala.Utils
 ( ContractExports
 , Contracts
 , DataExport(..)
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
 ) where

import Prelude hiding ( Enum(..), AdditiveSemigroup(..), Monoid(..), Ord(..), Traversable(..), (<$>), error )

import Jambhala.CLI ( scriptAddressBech32 )
import Jambhala.CLI.Emulator ( activateWallets, wait1 )
import Jambhala.CLI.Types
import Jambhala.Haskell hiding ( ask )
import Jambhala.Plutus hiding (Mainnet, Testnet, lovelaceValueOf )

import Cardano.Ledger.BaseTypes ( Network(..) )
import Cardano.Node.Emulator ( Params(..) )
import Plutus.Contract.Request ( getParams )

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

export :: Maybe EmulatorExport -> (s -> JambScript) -> s -> [DataExport] -> ContractExports
export Nothing constructor s des = ContractExports (constructor s) des Nothing
export test@(Just (EmulatorExport _ numWallets)) constructor s dataExports
  | numWallets > 0 = ContractExports{..}
  where script = constructor s
export _ _ _ _ = error "Wallet quantity must be greater than zero"

exportNoTest :: (s -> JambScript) -> s -> [DataExport] -> ContractExports
exportNoTest = export Nothing

exportWithTest :: (s -> JambScript) -> s -> [DataExport] -> JambEmulatorTrace -> WalletQuantity
               -> ContractExports
exportWithTest constructor s dataExports jTrace numWallets =
  export (Just EmulatorExport{..}) constructor s dataExports

-- | Exports a validator for use with jamb CLI
--
-- To export with an emulator test, use `exportValidatorWithTest`
exportValidator :: Validator -> [DataExport] -> ContractExports
exportValidator = exportNoTest JambValidator

-- | Exports a minting policy for use with jamb CLI
--
-- To export with an emulator test, use `exportMintingPolicyWithTest`
exportMintingPolicy :: MintingPolicy -> [DataExport] -> ContractExports
exportMintingPolicy = exportNoTest JambMintingPolicy

-- | Exports a validator and emulator test for use with jamb CLI
exportValidatorWithTest :: Validator -> [DataExport] -> JambEmulatorTrace -> WalletQuantity -> ContractExports
exportValidatorWithTest = exportWithTest JambValidator

-- | Exports a minting policy and emulator test for use with jamb CLI
exportMintingPolicyWithTest :: MintingPolicy -> [DataExport] -> JambEmulatorTrace -> WalletQuantity -> ContractExports
exportMintingPolicyWithTest = exportWithTest JambMintingPolicy

getTestnetAddress :: JambScript -> String
getTestnetAddress = scriptAddressBech32 Testnet

getMainnetAddress :: JambScript -> String
getMainnetAddress = scriptAddressBech32 Mainnet