-- 0. Create Module & Declare Imports
module Contracts.Samples.FreeMinting where

import Jambhala.Plutus
import Jambhala.Utils

-- 2. Define Lambda

-- | Define typed version of the validator lambda.
freeMintingLambda :: () -> ScriptContext -> Bool
freeMintingLambda _ _ = True
{-# INLINEABLE freeMintingLambda #-}

-- type UntypedMintingPolicy = BuiltinData -> BuiltinData -> ()
untypedLambda :: UntypedMintingPolicy
untypedLambda = mkUntypedMintingPolicy freeMintingLambda
{-# INLINEABLE untypedLambda #-}

-- 3. Pre-Compile Lambda

-- | Declare contract synonym with unique symbolic identifier.
type FreeMinting = MintingContract "free-minting"

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
compiledScript :: FreeMinting
compiledScript = mkMintingContract $$(compile [||untypedLambda||])

-- 4. Export Contract to Jambhala

-- | Define `exports` value for use with `j cli`.
exports :: JambExports
exports =
  export
    (defExports compiledScript)
      { emulatorTest = test
      }

-- 5. Define Emulator Component

-- | Define `MintingEndpoint` instance for the contract synonym.
instance MintingEndpoint FreeMinting where
  data MintParam FreeMinting
    = Mint !Integer !TokenName
    | Burn !Integer !TokenName
    deriving (Generic, FromJSON, ToJSON)

  mint :: MintParam FreeMinting -> ContractM FreeMinting ()
  mint mp = case mp of
    Mint tQuantity tName -> mint' tQuantity tName
    Burn tQuantity tName -> mint' (negate tQuantity) tName
    where
      mint' :: Integer -> TokenName -> ContractM FreeMinting ()
      mint' tQuantity tName = do
        submitAndConfirm
          Tx
            { lookups = scriptLookupsFor compiledScript
            , constraints = mustMint compiledScript tName tQuantity
            }
        let mintAction = if tQuantity > 0 then "Minted" :: String else "Burned"
        logStr $ printf "%s %d %s" mintAction (abs tQuantity) (show tName)

-- | Define emulator test.
test :: EmulatorTest
test =
  initEmulator @FreeMinting
    2
    [ Mint 1_000_000 "jambcoin" `forWallet` 1
    , Mint 1_000_000 "jambcoin" `forWallet` 2
    , Burn 100_000 "jambcoin" `forWallet` 1
    ]
