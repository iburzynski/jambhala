{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

module Jambhala.CLI.Emulator (
  EmulatorTest,
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
  getCurrentInterval,
  getCurrencySymbol,
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
  notImplemented,
  pkhForWallet,
  runJambEmulator,
  submitAndConfirm,
  toWallet,
  unsafeMkTxOutRef,
  wait,
  waitUntil,
  (IntMap.!),
)
where

import Cardano.Api (AsType (..), deserialiseFromRawBytesHex)
import Cardano.Api qualified as C
import Control.Lens ((^?))
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Reader (asks, runReader)
import Data.Default (def)
import Data.IntMap.Strict (Key, (!))
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Row (KnownSymbol, Row)
import Data.Text.Encoding qualified as Text
import Jambhala.CLI.Emulator.Types
import Jambhala.CLI.Types
import Jambhala.Plutus
import Ledger (decoratedTxOutValue)
import Ledger.Tx.CardanoAPI (fromCardanoTxId, fromCardanoValue)
import Ledger.Tx.Constraints (mustPayToOtherScriptWithInlineDatum)
import Plutus.Contract (HasEndpoint)
import Plutus.Contract.Trace (defaultDistFor)
import Plutus.Trace (EmulatorConfig (..), EmulatorRuntimeError (..))
import Plutus.Trace.Effects.Waiting (Waiting)
import Plutus.Trace.Emulator (ContractConstraints, throwError)
import Wallet.Emulator.Types (knownWallets, mockWalletAddress)

-- | Returns the `PaymentPubKeyHash` for the specified mock wallet.
pkhForWallet :: Key -> PubKeyHash
pkhForWallet = unPaymentPubKeyHash . mockWalletPaymentPubKeyHash . knownWallet . fromIntegral

getWalletAddress :: Key -> CardanoAddress
getWalletAddress = mockWalletAddress . knownWallet . fromIntegral

-- Emulator Actions

{- | Calls the validator contract's `give` endpoint with the provided `GiveParam` value,
  locking the output from the specified mock wallet at the script address.
  This function should be used infix.
-}
fromWallet ::
  forall contract.
  ( ToJSON (GiveParam contract)
  , ContractConstraints (Schema contract)
  , HasEndpoint "give" (GiveParam contract) (Schema contract)
  ) =>
  GiveParam contract ->
  WalletID ->
  EmulatorAction contract
p `fromWallet` w = do
  h <- asks (! w)
  callEndpoint @"give" @(GiveParam contract) @() @(Schema contract) @Text h p
  wait

{- | Calls the validator contract's `grab` endpoint with the provided `GrabParam` value,
  sending the output to the specified mock wallet.
  This function should be used infix.
-}
toWallet ::
  forall contract.
  ( ToJSON (GrabParam contract)
  , ContractConstraints (Schema contract)
  , HasEndpoint "grab" (GrabParam contract) (Schema contract)
  ) =>
  GrabParam contract ->
  WalletID ->
  EmulatorAction contract
p `toWallet` w = do
  h <- asks (! w)
  callEndpoint @"grab" @(GrabParam contract) @() @(Schema contract) @Text h p
  wait

{- | Calls the contract's `mint` endpoint with the provided `MintParam` value,
  sending the minted assets to the specified mock wallet.
  This function should be used infix.
-}
forWallet ::
  forall contract.
  ( ToJSON (MintParam contract)
  , ContractConstraints (Schema contract)
  , HasEndpoint "mint" (MintParam contract) (Schema contract)
  ) =>
  MintParam contract ->
  WalletID ->
  EmulatorAction contract
p `forWallet` w = do
  h <- asks (! w)
  callEndpoint @"mint" @(MintParam contract) @() @(Schema contract) @Text h p
  wait

-- | Wait 1 slot in an `EmulatorTest`.
wait :: (Member Waiting effs) => Eff effs ()
wait = void $ waitNSlots 1

-- | Wait until the specified slot in an `EmulatorTest`.
waitUntil :: (Member Waiting effs) => Slot -> Eff effs ()
waitUntil slot = void $ waitUntilSlot slot

-- | Submits a `Transaction` value to the emulated blockchain and waits until confirmation.
submitAndConfirm ::
  forall contract.
  ( FromData (DatumType contract)
  , ToData (DatumType contract)
  , ToData (RedeemerType contract)
  , Emulatable contract
  ) =>
  Transaction contract ->
  ContractM contract ()
submitAndConfirm Tx {..} = submit >>= awaitTxConfirmed
  where
    submit :: ContractM contract C.TxId
    submit =
      getCardanoTxId
        <$> submitTxConstraintsWith @contract @() @(Schema contract) @Text lookups constraints

{- | Constructs an `EmulatorTest` with a specified number of mock wallets and a list of `EmulatorAction` values.
  This function requires the `contract` type to be disambiguated using a type application, i.e.:
@ initEmulator \@MyContract @
-}
initEmulator ::
  forall contract.
  (ContractConstraints (Schema contract), Emulatable contract) =>
  WalletQuantity ->
  [EmulatorAction contract] ->
  EmulatorTest
initEmulator numWallets@(WalletQuantity wq) effs = ETest {..}
  where
    jTrace = do
      hs <- traverse ((`activateContractWallet` mkEndpoints @contract) . knownWallet) [1 .. fromIntegral wq]
      let hs' = IntMap.fromList $ zip [1 .. fromIntegral wq] hs
      runReader hs' (sequence_ effs)

{- | A default `EmulatorTest` value used in the `defExports` template,
  which throws an error if the test is run.
-}
notImplemented :: EmulatorTest
notImplemented = ETest {numWallets = 1, jTrace = logNotImplemented}
  where
    logNotImplemented :: Eff EmulatorEffects ()
    logNotImplemented = do
      throwError $ GenericError "No emulator test implemented for contract"

-- | Runs the `EmulatorTest` in the `IO` monad.
runJambEmulator :: EmulatorTest -> IO ()
runJambEmulator ETest {..} =
  runEmulatorTraceIOWithConfig def (mkEmulatorConfig numWallets) jTrace
  where
    mkEmulatorConfig (WalletQuantity n) = EmulatorConfig (Left $ defaultDistFor $ take (fromIntegral n) knownWallets) def

-- Make Transaction Inputs

-- | Converts a value to `BuiltinData` and makes it a `Datum`.
mkDatum :: (ToData a) => a -> Datum
mkDatum = Datum . toBuiltinData

-- | Converts a value to `BuiltinData` and makes it a `Redeemer`.
mkRedeemer :: (ToData a) => a -> Redeemer
mkRedeemer = Redeemer . toBuiltinData

-- | Returns the script address for a validator or minting policy.
getContractAddress :: (IsScript contract) => contract -> ContractM contract (AddressInEra BabbageEra)
getContractAddress script = do
  nId <- pNetworkId <$> getParams
  pure $ addressFunc nId script

-- | Returns a `Map` of the UTxOs locked at a `ValidatorContract`.
getUtxosAt :: forall sym. (KnownSymbol sym) => ValidatorContract sym -> ContractM (ValidatorContract sym) (Map TxOutRef DecoratedTxOut)
getUtxosAt script = getContractAddress @(ValidatorContract sym) script >>= utxosAt

getPubKeyUtxos ::
  forall w (s :: Row (*)) e.
  (AsContractError e) =>
  CardanoAddress ->
  Contract w s e (Map TxOutRef DecoratedTxOut)
getPubKeyUtxos = utxosAt

-- | Returns the `PubKeyHash` for the mock wallet initiating the transaction.
getOwnPKH :: (AsContractError e) => Contract w s e PubKeyHash
getOwnPKH = fmap unPaymentPubKeyHash ownFirstPaymentPubKeyHash

-- | Returns the currency symbol for the `MintingContract`.
getCurrencySymbol :: MintingContract sym -> CurrencySymbol
getCurrencySymbol = scriptCurrencySymbol . unMintingContract

-- | Returns the node's current `POSIXTime` `Interval`.
getCurrentInterval :: (AsContractError e) => Contract w s e (Interval POSIXTime)
getCurrentInterval = uncurry interval <$> currentNodeClientTimeRange

-- | Returns the POSIXTime value for the beginning of a slot.
defaultSlotBeginTime :: Slot -> POSIXTime
defaultSlotBeginTime = slotToBeginPOSIXTime def

-- | Returns the POSIXTime value for the end of a slot.
defaultSlotEndTime :: Slot -> POSIXTime
defaultSlotEndTime = slotToEndPOSIXTime def

-- Get Datum from UTxO

-- | Filter a UTxO `Map` to include only UTxOs whose value satisfies a given predicate.
filterByValue :: (Value -> Bool) -> Map TxOutRef DecoratedTxOut -> Map TxOutRef DecoratedTxOut
filterByValue p = Map.filter (maybe False p . getDecoratedTxOutValue)

-- | Filter a UTxO `Map` to include only UTxOs whose flattened value satisfies a given predicate.
filterByFlatValue :: ([(CurrencySymbol, TokenName, Integer)] -> Bool) -> Map TxOutRef DecoratedTxOut -> Map TxOutRef DecoratedTxOut
filterByFlatValue p = Map.filter (maybe False p . getDecoratedTxOutFlatValue)

-- | Filter a UTxO `Map` to include only UTxOs with datum satisfying a given predicate.
filterByDatum :: (FromData datum) => (datum -> Bool) -> Map TxOutRef DecoratedTxOut -> Map TxOutRef DecoratedTxOut
filterByDatum p = Map.filter (datumSatisfies p)

datumSatisfies :: (FromData datum) => (datum -> Bool) -> DecoratedTxOut -> Bool
datumSatisfies p dto = isJust $ convertDecoratedTxOutDatum dto >>= guard . p

-- | Gets the `Datum` from a `DecoratedTxOut` value (if present), then attempts to convert it to a value of a specified type.
convertDecoratedTxOutDatum :: (FromData datum) => DecoratedTxOut -> Maybe datum
convertDecoratedTxOutDatum dto = do
  (_, dfq) <- getDecoratedTxOutDatum dto
  Datum d <- getDatumInDatumFromQuery dfq
  fromBuiltinData d

-- | A non-lens version of the `decoratedTxOutDatum` getter.
getDecoratedTxOutDatum :: DecoratedTxOut -> Maybe (DatumHash, DatumFromQuery)
getDecoratedTxOutDatum dto = dto ^? decoratedTxOutDatum

getDecoratedTxOutValue :: DecoratedTxOut -> Maybe Value
getDecoratedTxOutValue dto = fromCardanoValue <$> dto ^? decoratedTxOutValue

getDecoratedTxOutFlatValue :: DecoratedTxOut -> Maybe [(CurrencySymbol, TokenName, Integer)]
getDecoratedTxOutFlatValue dto = flattenValue . fromCardanoValue <$> dto ^? decoratedTxOutValue

-- | A non-lens version of the `datumInDatumFromQuery` getter.
getDatumInDatumFromQuery :: DatumFromQuery -> Maybe Datum
getDatumInDatumFromQuery dfq = dfq ^? datumInDatumFromQuery

-- ScriptLookups

-- | Adds a `Map` of UTxOs to a `ScriptLookups` value.
andUtxos :: ScriptLookups contract -> Map TxOutRef DecoratedTxOut -> ScriptLookups contract
andUtxos scriptLookups utxos = scriptLookups <> unspentOutputs utxos

-- TXConstraints

-- | Add additional transaction constraints to a `Transaction` value.
addConstraints :: Transaction contract -> TxConstraints (RedeemerType contract) (DatumType contract) -> Transaction contract
addConstraints (Tx lookups commonConstraints) extraConstraints = Tx lookups (commonConstraints <> extraConstraints)

-- | Requires the transaction to pay a given value to a spending validator's script address with a specified datum value.
mustPayScriptWithDatum :: (ToData datum) => ValidatorContract sym -> datum -> Value -> TxConstraints rType dType
mustPayScriptWithDatum validator datum =
  mustPayToOtherScriptWithDatumInTx
    (validatorHash $ unValidatorContract validator)
    (mkDatum datum)

-- | Requires the transaction to pay a given value to a spending validator's script address with a specified inline datum value.
mustPayScriptWithInlineDatum :: (ToData datum) => ValidatorContract contract -> datum -> Value -> TxConstraints rType dType
mustPayScriptWithInlineDatum validator datum =
  mustPayToOtherScriptWithInlineDatum
    (validatorHash $ unValidatorContract validator)
    (mkDatum datum)

-- | Requires the transaction to pay a given value to a public key address.
mustPayPKH :: PubKeyHash -> Value -> TxConstraints rType dType
mustPayPKH pkh = mustPayToPubKey (PaymentPubKeyHash pkh)

-- | Requires the specified UTxO to be spent in the transaction with the specified redeemer value.
mustBeSpentWith :: (ToData redeemer) => TxOutRef -> redeemer -> TxConstraints rType dType
utxo `mustBeSpentWith` redeemer = mustSpendScriptOutput utxo $ mkRedeemer redeemer

-- | Requires a `Map` of UTxOs to all be spent in the transaction with the specified redeemer value.
mustAllBeSpentWith :: (ToData redeemer) => Map TxOutRef DecoratedTxOut -> redeemer -> TxConstraints rType dType
utxos `mustAllBeSpentWith` redeemer = mconcatMap (`mustBeSpentWith` redeemer) $ Map.keys utxos

-- | Requires the transaction be signed by the private key corresponding to a `PubKeyHash`.
mustSign :: PubKeyHash -> TxConstraints i o
mustSign = mustBeSignedBy . PaymentPubKeyHash

-- | Requires the transaction to mint a specified quantity of a given token with the `MintingContract`'s currency symbol.
mustMint :: MintingContract sym -> TokenName -> Integer -> TxConstraints i o
mustMint policy tName tokenQuantity = mustMintValue $ mkMintingValue policy tName tokenQuantity

-- | Requires the transaction to mint a specified quantity of a given token with the `MintingContract`'s currency symbol with a redeemer.
mustMintWithRedeemer ::
  (ToData redeemer) =>
  MintingContract sym ->
  redeemer ->
  TokenName ->
  Integer ->
  TxConstraints i o
mustMintWithRedeemer policy redeemer tName tokenQuantity =
  mustMintValueWithRedeemer (mkRedeemer redeemer) $ mkMintingValue policy tName tokenQuantity

-- | Returns a `Value` containing a specified quantity of a given token with the `MintingContract`'s currency symbol.
mkMintingValue :: MintingContract sym -> TokenName -> Integer -> Value
mkMintingValue policy = singleton (getCurrencySymbol policy)

-- | `logInfo` with the input type as `String`.
logStr :: String -> Contract () schema Text ()
logStr = logInfo @String

{- | Converts a Text representation of a TxHash and an index into a Plutus Ledger TxId value.
  Throws a runtime error if the TxHash cannot be converted.
  Can be used in emulator tests
-}
unsafeMkTxOutRef :: Text -> Integer -> TxOutRef
unsafeMkTxOutRef txHash = TxOutRef txId
  where
    txId =
      either (const (error "Unable to deserialise to TxId")) fromCardanoTxId
        . deserialiseFromRawBytesHex AsTxId
        $ Text.encodeUtf8 txHash
