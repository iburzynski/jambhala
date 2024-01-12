{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Jambhala.CLI.Types where

import Codec.Serialise (Serialise (..))
import Control.Monad.Freer (Eff)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (..))
import Data.Row (Row)
import GHC.Natural (Natural)
import GHC.Real (Real)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Jambhala.Plutus

-- | A string identifier used to reference an exported contract in the `jamb` CLI.
type ContractName = String

{- | Additional input data to accompany a contract.
  Serialised to a `.json` file by the `jamb -w` command.
-}
data DataExport where
  DataExport :: (ToData d) => String -> d -> DataExport

instance ToData DataExport where
  toBuiltinData (DataExport _ x) = toBuiltinData x

data ScriptExport
  = JambValidator !Validator
  | JambMintingPolicy !MintingPolicy
  deriving (Generic)

instance Serialise ScriptExport where
  encode (JambValidator v) = encode v
  encode (JambMintingPolicy p) = encode p

-- | The number of wallets required in an `EmulatorTest`.
newtype WalletQuantity = WalletQuantity {walletQ :: Natural}
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral)

{- | A test that interacts with a contract in an emulated blockchain environment.
  `EmulatorTest` values are constructed using the `initEmulator` function.
-}
data EmulatorTest = ETest {numWallets :: !WalletQuantity, jTrace :: !(Eff EmulatorEffects ())}

instance Semigroup EmulatorTest where
  (<>) :: EmulatorTest -> EmulatorTest -> EmulatorTest
  (ETest n1 trace1) <> (ETest n2 trace2) = ETest (max n1 n2) (sequence_ [trace1, trace2])

instance Monoid EmulatorTest where
  mempty :: EmulatorTest
  mempty = ETest 1 $ pure ()

data ContractExports = ContractExports
  { script :: !ScriptExport
  , dExports :: ![DataExport]
  , test :: !EmulatorTest
  }

newtype ValidatorContract (contract :: Symbol) = ValidatorContract {unValidatorContract :: Validator}

newtype MintingContract (contract :: Symbol) = MintingContract {unMintingContract :: MintingPolicy}

-- | A type family that maps Jambhala contract types to their respective hash types.
type family JambHash s where
  JambHash (ValidatorContract sym) = ValidatorHash
  JambHash (MintingContract sym) = MintingPolicyHash

-- | A helper class facilitating polymorphic functions for `ValidatorContract` and `MintingContract` values.
class IsScript s where
  scriptLookupFunc :: forall c. s -> ScriptLookups c
  hashFunc :: s -> JambHash s
  toScriptExport :: s -> ScriptExport
  addressFunc :: NetworkId -> s -> AddressInEra BabbageEra
  getContractName :: s -> ContractName

instance (KnownSymbol sym) => IsScript (ValidatorContract sym) where
  scriptLookupFunc = plutusV2OtherScript . unValidatorContract
  hashFunc = validatorHash . unValidatorContract
  toScriptExport = JambValidator . unValidatorContract
  addressFunc n (ValidatorContract v) = mkValidatorCardanoAddress n $ Versioned v PlutusV2
  getContractName _ = symbolVal @sym Proxy

instance (KnownSymbol sym) => IsScript (MintingContract sym) where
  scriptLookupFunc = plutusV2MintingPolicy . unMintingContract
  hashFunc = mintingPolicyHash . unMintingContract
  toScriptExport = JambMintingPolicy . unMintingContract
  addressFunc n (MintingContract p) = mkMintingPolicyCardanoAddress n p
  getContractName _ = symbolVal @sym Proxy

{- | A synonym for the `Contract w s e` monad with `w` = `()`, `e` = `Text`,
  and `s` (schema) = the result of the `Schema` type family applied to the `contract` type.
-}
type ContractM contract = Contract () (Schema contract) Text

{- | An internal class used to provide automatic emulatability for instances of
  the `ValidatorEndpoints` and `MintingEndpoint` classes.
-}
class (IsScript contract) => Emulatable contract where
  type Schema contract :: Row (*)
  mkEndpoints :: ContractM contract ()
  scriptLookupsFor :: contract -> ScriptLookups contract

{- | A typeclass instantiated for type synonyms defined with the `ValidatorContract` type constructor.
  Instantiating this class for such synonyms provides an automatic `Emulatable` instance.
-}
class ValidatorEndpoints contract where
  -- | The type of the input parameter for the `give` endpoint action.
  data GiveParam contract :: *

  -- | The type of the input parameter for the `grab` endpoint action.
  data GrabParam contract :: *

  -- | Optionally defines the type of the datum used by the contract,
  --   which is used to provide an instance of `ValidatorTypes`.
  --   Defaults to `()`.
  type VDatum contract :: *

  type VDatum contract = ()

  -- | Optionally defines the type of the redeemer used by the contract,
  --   which is used to provide an instance of `ValidatorTypes`.
  --   Defaults to `()`.
  type VRedeemer contract :: *

  type VRedeemer contract = ()

  -- | Emulator endpoint action to lock UTxOs at the script address.
  give :: GiveParam contract -> ContractM contract ()

  -- | Emulator endpoint action to unlock UTxOs at the script address.
  grab :: GrabParam contract -> ContractM contract ()

{- | A typeclass instantiated for type synonyms defined with the `MintingContract` type constructor.
  Instantiating this class for such synonyms provides an automatic `Emulatable` instance.
-}
class MintingEndpoint contract where
  -- | The type of the input parameter for the `mint` endpoint action.
  data MintParam contract :: *

  -- | Optionally defines the type of the redeemer used by the contract,
  --   which is used to provide an instance of `ValidatorTypes`.
  --   Defaults to `()`.
  type MRedeemer contract :: *

  type MRedeemer contract = ()

  -- | Emulator endpoint action to mint assets using the minting policy script.
  mint :: MintParam contract -> ContractM contract ()

instance
  ( ValidatorEndpoints (ValidatorContract sym)
  , FromJSON (GiveParam (ValidatorContract sym))
  , FromJSON (GrabParam (ValidatorContract sym))
  , KnownSymbol sym
  ) =>
  Emulatable (ValidatorContract sym)
  where
  type
    Schema (ValidatorContract sym) =
      Endpoint "give" (GiveParam (ValidatorContract sym))
        .\/ Endpoint "grab" (GrabParam (ValidatorContract sym))
  mkEndpoints :: ContractM (ValidatorContract sym) ()
  mkEndpoints = endpoints
    where
      endpoints :: ContractM (ValidatorContract sym) ()
      endpoints = awaitPromise (give' `select` grab') >> endpoints
      give' :: Promise () (Schema (ValidatorContract sym)) Text ()
      give' = endpoint @"give" @(GiveParam (ValidatorContract sym)) @_ @(Schema (ValidatorContract sym)) $ give
      grab' :: Promise () (Schema (ValidatorContract sym)) Text ()
      grab' = endpoint @"grab" @(GrabParam (ValidatorContract sym)) @_ @(Schema (ValidatorContract sym)) $ grab
  scriptLookupsFor :: ValidatorContract sym -> ScriptLookups (ValidatorContract sym)
  scriptLookupsFor vc =
    scriptLookupFunc @_ @(ValidatorContract sym) vc
      <> (plutusV2MintingPolicy . mkForwardingMintingPolicy . validatorHash $ unValidatorContract vc)

instance
  ( MintingEndpoint (MintingContract sym)
  , FromJSON (MintParam (MintingContract sym))
  , KnownSymbol sym
  ) =>
  Emulatable (MintingContract sym)
  where
  type Schema (MintingContract sym) = Endpoint "mint" (MintParam (MintingContract sym))
  mkEndpoints :: ContractM (MintingContract sym) ()
  mkEndpoints = endpoint'
    where
      endpoint' :: ContractM (MintingContract sym) ()
      endpoint' = awaitPromise mint' >> endpoint'
      mint' :: Promise () (Schema (MintingContract sym)) Text ()
      mint' = endpoint @"mint" @(MintParam (MintingContract sym)) @_ @(Schema (MintingContract sym)) $ mint
  scriptLookupsFor :: MintingContract sym -> ScriptLookups (MintingContract sym)
  scriptLookupsFor = scriptLookupFunc @_ @(MintingContract sym)

instance ValidatorTypes (ValidatorContract sym) where
  type DatumType (ValidatorContract sym) = VDatum (ValidatorContract sym)
  type RedeemerType (ValidatorContract sym) = VRedeemer (ValidatorContract sym)

instance ValidatorTypes (MintingContract sym) where
  type DatumType (MintingContract sym) = Void
  type RedeemerType (MintingContract sym) = MRedeemer (MintingContract sym)

{- | A record containing `ScriptLookups` and `TxConstraints`,
  used for transaction submission in emulator endpoints.
-}
data Transaction c = Tx
  { lookups :: !(ScriptLookups c)
  , constraints :: !(TxConstraints (RedeemerType c) (DatumType c))
  }

{- | A `Map` between `ContractName` keys and `ContractExports` values.
  Used by the `jamb` CLI to look up and operate on available contracts.
-}
type JambContracts = Map ContractName ContractExports

type FileName = String

-- | Internal representation of `jamb` CLI.
data Command
  = List
  | Addr !ContractName !Network
  | Bech32 !Int !Text
  | Hash !ContractName
  | Test !ContractName
  | Write !ContractName !(Maybe FileName) !Network
  | Update !(Maybe String)
