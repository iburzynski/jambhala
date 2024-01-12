module Jambhala.Utils (
  ContractExports,
  ContractM,
  ExportTemplate (..),
  JambExports,
  JambContracts,
  DataExport (..),
  EmulatorTest,
  MintingContract (..),
  MintingEndpoint (..),
  Transaction (..),
  ValidatorContract (..),
  ValidatorEndpoints (..),
  addConstraints,
  andUtxos,
  convertDecoratedTxOutDatum,
  datumSatisfies,
  defExports,
  defaultSlotBeginTime,
  defaultSlotEndTime,
  export,
  filterByDatum,
  filterByFlatValue,
  filterByValue,
  forWallet,
  fromWallet,
  getContractAddress,
  getCurrencySymbol,
  getCurrentInterval,
  getUtxosAt,
  getDatumInDatumFromQuery,
  getDecoratedTxOutDatum,
  getDecoratedTxOutValue,
  getFwdMintingPolicy,
  getFwdMintingPolicyId,
  getOwnPKH,
  getPubKeyUtxos,
  getWalletAddress,
  initEmulator,
  logStr,
  submitAndConfirm,
  mkDatum,
  mkMintingContract,
  mkMintingValue,
  mkRedeemer,
  mkUntypedMintingPolicy,
  mkUntypedValidator,
  mkValidatorContract,
  mustAllBeSpentWith,
  mustSign,
  mustBeSpentWith,
  mustMint,
  mustMintWithRedeemer,
  mustPayPKH,
  mustPayScriptWithDatum,
  mustPayScriptWithInlineDatum,
  pkhForWallet,
  scriptLookupsFor,
  toJSONfile,
  tokenNameToString,
  toWallet,
  unsafeMkTxOutRef,
  wait,
  waitUntil,
) where

import Jambhala.CLI.Emulator (
  addConstraints,
  andUtxos,
  convertDecoratedTxOutDatum,
  datumSatisfies,
  defaultSlotBeginTime,
  defaultSlotEndTime,
  filterByDatum,
  filterByFlatValue,
  filterByValue,
  forWallet,
  fromWallet,
  getContractAddress,
  getCurrencySymbol,
  getCurrentInterval,
  getDatumInDatumFromQuery,
  getDecoratedTxOutDatum,
  getDecoratedTxOutValue,
  getOwnPKH,
  getPubKeyUtxos,
  getUtxosAt,
  getWalletAddress,
  initEmulator,
  logStr,
  mkDatum,
  mkMintingValue,
  mkRedeemer,
  mustAllBeSpentWith,
  mustBeSpentWith,
  mustMint,
  mustMintWithRedeemer,
  mustPayPKH,
  mustPayScriptWithDatum,
  mustPayScriptWithInlineDatum,
  mustSign,
  pkhForWallet,
  submitAndConfirm,
  toWallet,
  unsafeMkTxOutRef,
  wait,
  waitUntil,
 )
import Jambhala.CLI.Export (ExportTemplate (..), JambExports, defExports, export, toJSONfile)
import Jambhala.CLI.Types
import Jambhala.Plutus
import Plutus.V1.Ledger.Value (toString)

-- Make Jambhala-compatible contracts

-- | Converts a validator's compiled UPLC code into a Jambhala `ValidatorContract` value.
mkValidatorContract ::
  CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) ->
  ValidatorContract contract
mkValidatorContract = ValidatorContract . mkValidatorScript

-- | Converts a minting policy's compiled UPLC code into a Jambhala `MintingContract` value.
mkMintingContract ::
  CompiledCode (BuiltinData -> BuiltinData -> ()) ->
  MintingContract contract
mkMintingContract = MintingContract . mkMintingPolicyScript

{-# INLINEABLE mkUntypedValidator #-}

-- | A more efficient implementation of the `mkUntypedValidator` method of the `IsScriptContext` typeclass.
mkUntypedValidator ::
  ( UnsafeFromData a
  , UnsafeFromData b
  ) =>
  (a -> b -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> BuiltinData -> ())
mkUntypedValidator f a b ctx =
  check $
    f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData b)
      (unsafeFromBuiltinData ctx)

{-# INLINEABLE mkUntypedMintingPolicy #-}

-- | A more efficient implementation of the `mkUntypedMintingPolicy` method of the `IsScriptContext` typeclass.
mkUntypedMintingPolicy ::
  (UnsafeFromData a) =>
  (a -> ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinData -> ())
mkUntypedMintingPolicy f a ctx =
  check $
    f
      (unsafeFromBuiltinData a)
      (unsafeFromBuiltinData ctx)

-- | Returns the compiled forwarding `MintingContract` for a compiled Jambhala `ValidatorContract`.
getFwdMintingPolicy :: ValidatorContract validatorName -> MintingContract policyName
getFwdMintingPolicy = MintingContract . mkForwardingMintingPolicy . validatorHash . unValidatorContract

-- | Returns the policy ID (currency symbol) of the forwarding `MintingContract` for a compiled Jambhala `ValidatorContract`.
getFwdMintingPolicyId :: ValidatorContract validatorName -> CurrencySymbol
getFwdMintingPolicyId = scriptCurrencySymbol . mkForwardingMintingPolicy . validatorHash . unValidatorContract

-- | Converts a `TokenName` value to a `String` value, removing the `"0x"` prefix.
tokenNameToString :: TokenName -> String
tokenNameToString = drop 2 . toString
