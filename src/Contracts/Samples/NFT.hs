-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}

module Contracts.Samples.NFT where

import Jambhala.Plutus
import Jambhala.Utils

-- | Define a custom type to parameterize the minting policy:
data PolicyParam = PolicyParam {getUtxo :: TxOutRef, getTokenName :: TokenName}

-- | Generate `Lift` instance for the custom parameter type with Template Haskell.
--   Allows argument value to be pre-compiled to UPLC, so the compiled parameterized script can be applied to it.
makeLift ''PolicyParam

-- | Custom redeemer type to indicate minting mode.
data Mode = Minting | Burning

-- | For custom types with multiple constructors, `makeIsDataIndexed` must be used to generate ToData/FromData instances.
--   Unlike `unstableMakeIsData`, this generates `BuiltinData` values with constructors indexed in a stable order.
makeIsDataIndexed ''Mode [('Minting, 0), ('Burning, 1)]

-- 2. Define Lambda

-- | Helper function to check if the correct quantity of the given asset is minted in the transaction.
checkMintAmount :: CurrencySymbol -> TokenName -> Mode -> Value -> Bool
checkMintAmount cs tn mode minted =
  let q = case mode of
        Minting -> 1
        Burning -> -1
   in pany (\(cs', tn', q') -> cs' #== cs && tn' #== tn && q' #== q) $ flattenValue minted
{-# INLINEABLE checkMintAmount #-}

-- | Helper function to check that the UTxO parameterizing the script is being spent in the transaction.
hasUtxo :: TxOutRef -> [TxInInfo] -> Bool
hasUtxo oref = pany (\(TxInInfo oref' _) -> oref' #== oref)
{-# INLINEABLE hasUtxo #-}

-- | Typed parameterized minting policy lambda.
nftLambda :: PolicyParam -> Mode -> ScriptContext -> Bool
nftLambda (PolicyParam oref tn) mode ctx@(ScriptContext TxInfo {..} _) =
  -- check a list of conditions that must be met to mint with the policy
  pand $
    -- transactions must always mint the correct amount of the asset
    traceIfFalse "Wrong amount" (checkMintAmount (ownCurrencySymbol ctx) tn mode txInfoMint) :
    -- "cons" this condition (head) onto one of two possible tails:
    case mode of
      -- if minting, the UTxO parameterizing the policy must be consumed as an input
      Minting -> [traceIfFalse "UTxO not consumed" $ hasUtxo oref txInfoInputs]
      -- if burning, no additional conditions are required: construct a singleton list with empty tail
      Burning -> []
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

-- | Define `exports` value for use with `jamb` CLI.
exports :: JambExports
exports =
  export
    (defExports policy)
      { -- Export JSON representations of our redeemer Mode values for transaction construction.
        dataExports = [Minting `toJSONfile` "mintmode", Burning `toJSONfile` "burnmode"],
        emulatorTest = test
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
        { lookups = scriptLookupsFor policy `andUtxos` minterUtxos,
          constraints =
            mustSpendPubKeyOutput oref
              <> mustMintWithRedeemer policy Minting tName 1
        }
    logStr $ "Minted 1 " ++ show tName
  mint (Burn mintTxOutRef tName) = do
    let policy = compileScript (PolicyParam mintTxOutRef tName)
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor policy,
          constraints = mustMintWithRedeemer policy Burning tName (-1)
        }
    logStr $ "Burned 1 " ++ show tName

-- | Define emulator test.
test :: EmulatorTest
test =
  initEmulator @NFTMinting
    2
    [ Mint "jambtoken" `forWallet` 1,
      Mint "jambtoken" `forWallet` 1,
      Mint "jambtoken" `forWallet` 2,
      -- To test a burn transaction, first run the test with just the actions above.
      -- Use the emulator output to identify the TxHash/TxIx of the UTxO used as input to mint the NFT we want to burn.
      -- Then apply unsafeMkTxOutRef to the TxHash and TxIx to reconstruct a corresponding TxOutRef value, and provide it to the Burn constructor.
      Burn (unsafeMkTxOutRef "899b40a640d4d3df5bb4a85b0d03be7df0509bcd7f6c1e99075423852a35a2a4" 10) "jambtoken" `forWallet` 1
    ]