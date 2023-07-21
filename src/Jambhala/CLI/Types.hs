{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Jambhala.CLI.Types where

import Codec.Serialise (Serialise (..))
import Control.Monad.Freer (Eff)
import Data.Map.Strict (Map)
import GHC.Natural (Natural)
import GHC.Real (Integral, Real)
import Jambhala.Plutus
import Ledger.Tx.Constraints (TxConstraints, plutusV2MintingPolicy)
import Plutus.Script.Utils.V2.Address (mkMintingPolicyCardanoAddress)
import Plutus.Script.Utils.V2.Scripts (mintingPolicyHash)
import Plutus.V2.Ledger.Api (MintingPolicyHash)

type ContractName = String

data DataExport where
  DataExport :: (ToData d) => String -> d -> DataExport

instance ToData DataExport where
  toBuiltinData (DataExport _ x) = toBuiltinData x

newtype JambScript s = JambScript {unJambScript :: s}
  deriving (Generic)

data ScriptExport
  = JambValidator !Validator
  | JambMintingPolicy !MintingPolicy
  deriving (Generic)

instance Serialise ScriptExport where
  encode (JambValidator v) = encode v
  encode (JambMintingPolicy p) = encode p

newtype WalletQuantity = WalletQuantity {walletQ :: Natural}
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral)

data EmulatorTest = ETest {numWallets :: !WalletQuantity, jTrace :: !(Eff EmulatorEffects ())}

data ContractExports = ContractExports
  { script :: !ScriptExport,
    dExports :: ![DataExport],
    test :: !EmulatorTest
  }

type family JambHash s where
  JambHash (JambScript Validator) = ValidatorHash
  JambHash (JambScript MintingPolicy) = MintingPolicyHash

class IsScript s where
  scriptLookupFunc :: forall c. s -> ScriptLookups c
  hashFunc :: s -> JambHash s
  toScriptExport :: s -> ScriptExport
  addressFunc :: NetworkId -> s -> AddressInEra BabbageEra

instance IsScript (JambScript Validator) where
  scriptLookupFunc = plutusV2OtherScript . unJambScript
  hashFunc = validatorHash . unJambScript
  toScriptExport = JambValidator . unJambScript
  addressFunc n (JambScript v) = mkValidatorCardanoAddress n $ Versioned v PlutusV2

instance IsScript (JambScript MintingPolicy) where
  scriptLookupFunc = plutusV2MintingPolicy . unJambScript
  hashFunc = mintingPolicyHash . unJambScript
  toScriptExport = JambMintingPolicy . unJambScript
  addressFunc n (JambScript p) = mkMintingPolicyCardanoAddress n p

type JambContracts = Map ContractName ContractExports

type FileName = String

data Command
  = List
  | Addr !ContractName !Network
  | Hash !ContractName
  | Test !ContractName
  | Write !ContractName !(Maybe FileName)
  | Update !(Maybe String)

data Transaction c = Tx
  { lookups :: !(ScriptLookups c),
    constraints :: !(TxConstraints (RedeemerType c) (DatumType c))
  }