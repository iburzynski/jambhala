-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}

module Contracts.Samples.NFT where

import Jambhala.Plutus
import Jambhala.Utils

-- | Define a custom type to parameterize the minting policy:
data PolicyParam = PolicyParam {getUtxo :: TxOutRef, getTokenName :: TokenName}

{- | Generate `Lift` instance for the custom parameter type with Template Haskell.
  Allows argument value to be pre-compiled to UPLC, so the compiled parameterized script can be applied to it.
-}
makeLift ''PolicyParam

-- | Custom redeemer type to indicate minting mode.
data Mode = Minting | Burning

{- | For custom types with multiple constructors, `makeIsDataIndexed` must be used to generate ToData/FromData instances.
  Unlike `unstableMakeIsData`, this generates `BuiltinData` values with constructors indexed in a stable order.
-}
makeIsDataIndexed ''Mode [('Minting, 0), ('Burning, 1)]

-- 2. Define Lambda

-- | Helper function to check that the UTxO parameterizing the script is being spent in the transaction.
hasUtxo :: TxOutRef -> [TxInInfo] -> Bool
hasUtxo oref inputs =
  traceIfFalse "UTxO not consumed" $ pany (\(TxInInfo oref' _) -> oref' #== oref) inputs
{-# INLINEABLE hasUtxo #-}

{- | Helper function to check if *only* the NFT is minted in the transaction.
     The safest way to prevent "other token name" vulnerability
     (see https://library.mlabs.city/common-plutus-security-vulnerabilities#2.othertokenname)
-}
checkMintRestrictive :: AssetClass -> Value -> Integer -> Bool
checkMintRestrictive ac minted qty =
  traceIfFalse "Invalid mint" $ minted #== assetClassValue ac qty
{-# INLINEABLE checkMintRestrictive #-}

{- | Helper function that permits other assets to be minted in the same transaction,
     but no other assets with the NFT's currency symbol can be minted
     (useful if application requires minting multiple assets in the same transaction.)
-}
checkMintPermissive :: AssetClass -> Value -> Integer -> Bool
checkMintPermissive (AssetClass (cs, tn)) minted qty =
  case filter (\(cs', _, _) -> cs' #== cs) $ flattenValue minted of
    [(_, tn', qty')] ->
      traceIfFalse "Wrong token name" (tn' #== tn)
        && traceIfFalse "Invalid mint quantity" (qty' #== qty)
    _invalidMint -> trace "Multiple assets" False
{-# INLINEABLE checkMintPermissive #-}

-- | Typed parameterized minting policy lambda.
nftLambda :: PolicyParam -> Mode -> ScriptContext -> Bool
nftLambda (PolicyParam oref tn) mode ctx@(ScriptContext TxInfo {..} _) =
  let checkMint' = checkMintRestrictive (AssetClass (ownCurrencySymbol ctx, tn)) txInfoMint
   in case mode of
        Minting -> hasUtxo oref txInfoInputs && checkMint' 1
        Burning -> checkMint' (-1)
{-# INLINEABLE nftLambda #-}

-- | Untyped version of the parameterized minting policy lambda.
untypedLambda :: PolicyParam -> UntypedMintingPolicy
untypedLambda p = mkUntypedMintingPolicy $ nftLambda p
{-# INLINEABLE untypedLambda #-}

-- | Declare contract synonym with unique symbolic identifier.
type NFTMinting = MintingContract "nft"

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
compileScript :: PolicyParam -> NFTMinting
compileScript p =
  mkMintingContract $
    $$(compile [||untypedLambda||])
      `applyCode` liftCode p

-- 4. Export Contract to Jambhala

-- | Define `exports` value for use with `j cli`.
exports :: JambExports
exports =
  export
    (defExports policy)
      { -- Export JSON representations of our redeemer Mode values for transaction construction.
        dataExports = [Minting `toJSONfile` "mintmode", Burning `toJSONfile` "burnmode"]
      , emulatorTest = test
      }
  where
    -- To produce the finished minting policy, select an arbitrary UTxO at your address to consume during the mint.
    -- Apply unsafeMkTxOutRef to its TxHash and TxIx values to construct a TxOutRef value, and provide this as argument to the parameterized policy.
    -- This UTxO must be included as an input to your minting transaction.
    oref = unsafeMkTxOutRef "65c4e89843cfcbd41e4fbdf94bdf002f04c46789067ba9ae90072c6d757abdd8" 1 -- Replace with TxHash and TxIx values for a UTxO at your address
    policy = compileScript (PolicyParam oref "jambtoken")

-- 5. Define Emulator Component

instance MintingEndpoint NFTMinting where
  data MintParam NFTMinting
    = Mint TokenName
    | Burn TxOutRef TokenName
    deriving (Generic, FromJSON, ToJSON)

  mint :: MintParam NFTMinting -> ContractM NFTMinting ()
  mint (Mint tName) = do
    oref <- getUnspentOutput
    let policy = compileScript (PolicyParam oref tName)
    minterUtxos <- ownUtxos
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor policy `andUtxos` minterUtxos
        , constraints =
            mustSpendPubKeyOutput oref
              <> mustMintWithRedeemer policy Minting tName 1
              -- Uncomment below to test for "other token name" vulnerability:
              -- <> mustMintWithRedeemer policy Minting "oops!" 1
        }
    logStr $ "Minted 1 " ++ show tName
  mint (Burn mintTxOutRef tName) = do
    let policy = compileScript (PolicyParam mintTxOutRef tName)
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor policy
        , constraints = mustMintWithRedeemer policy Burning tName (-1)
        }
    logStr $ "Burned 1 " ++ show tName

-- | Define emulator test.
test :: EmulatorTest
test =
  initEmulator @NFTMinting
    1
    [ Mint "jambtoken" `forWallet` 1
    , Mint "jambtoken" `forWallet` 1
    , -- To test a burn transaction, first run the test with just the actions above.
      -- Use the emulator output to identify the TxHash/TxIx of the UTxO used as input to mint the NFT we want to burn.
      -- Then apply unsafeMkTxOutRef to the TxHash and TxIx to reconstruct a corresponding TxOutRef value, and provide it to the Burn constructor.
      Burn (unsafeMkTxOutRef "019b759d7d22f8f93125c27229debf4771194f9d9776acd31e3b0ac4bda04c9a" 0) "jambtoken" `forWallet` 1
    ]
