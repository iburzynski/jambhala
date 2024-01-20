-- 0. Create Module & Declare Imports
module Contracts.Samples.SignedMinting where

import Jambhala.Plutus
import Jambhala.Utils

-- 1. Declare Types: N/A

-- 2. Define Lambda

signedMintingLambda :: PubKeyHash -> () -> ScriptContext -> Bool
signedMintingLambda pkh _ (ScriptContext txInfo _) =
  traceIfFalse "Wrong signature" $ txSignedBy txInfo pkh
{-# INLINEABLE signedMintingLambda #-}

untypedLambda :: PubKeyHash -> UntypedMintingPolicy
untypedLambda = mkUntypedMintingPolicy . signedMintingLambda
{-# INLINEABLE untypedLambda #-}

-- 3. Pre-Compile Lambda

-- | Declare contract synonym with unique symbolic identifier.
type SignedMinting = MintingContract "signed-minting"

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
compileScript :: PubKeyHash -> SignedMinting
compileScript pkh = mkMintingContract ($$(compile [||untypedLambda||]) `applyCode` liftCode pkh)

-- 4. Export Contract to Jambhala

-- | Define `exports` value for use with `j cli`.
exports :: JambExports
exports =
  export
    (defExports appliedScript)
      { emulatorTest = test
      }
  where
    -- The minting policy must be applied to some parameter to generate a hash or serialise the script to a file.
    -- 1. Use the `key-hash` script from cardano-cli-guru to get the pubkey hash for a beneficiary address.
    -- 2. Replace the placeholder hex string below with the address pubkey hash.
    appliedScript = compileScript "3a5039efcafd4c82c9169b35afb27a17673f6ed785ea087139a65a5d"

-- 5. Define Emulator Component

instance MintingEndpoint SignedMinting where
  data MintParam SignedMinting
    = Mint !Integer !TokenName
    | Burn !Integer !TokenName
    deriving (Generic, FromJSON, ToJSON)
  mint :: MintParam SignedMinting -> ContractM SignedMinting ()
  mint mp = do
    pkh <- getOwnPKH
    let appliedScript = compileScript pkh
        mint' :: Integer -> TokenName -> ContractM SignedMinting ()
        mint' tQuantity tName = do
          submitAndConfirm
            Tx
              { lookups = scriptLookupsFor appliedScript
              , constraints =
                  mustMint appliedScript tName tQuantity
                    <> mustSign pkh
              }
          let mintAction = if tQuantity > 0 then "Minted" :: String else "Burned"
          logStr $ printf "%s %d %s" mintAction (abs tQuantity) (show tName)
    case mp of
      Mint tQuantity tName -> mint' tQuantity tName
      Burn tQuantity tName -> mint' (negate tQuantity) tName

-- | Define emulator test.
test :: EmulatorTest
test =
  initEmulator @SignedMinting
    2
    [ Mint 1_000_000 "jambcoin" `forWallet` 1
    , Mint 1_000_000 "jambcoin" `forWallet` 2 -- different currency symbol!
    , Burn 100_000 "jambcoin" `forWallet` 1
    ]
