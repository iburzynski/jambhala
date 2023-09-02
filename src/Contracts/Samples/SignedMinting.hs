module Contracts.Samples.SignedMinting where

import Jambhala.Plutus
import Jambhala.Utils

signedMinting :: PubKeyHash -> () -> ScriptContext -> Bool
signedMinting pkh _ (ScriptContext txInfo _) =
  traceIfFalse "Wrong signature" $ txSignedBy txInfo pkh
{-# INLINEABLE signedMinting #-}

untyped :: PubKeyHash -> UntypedMintingPolicy
untyped = mkUntypedMintingPolicy . signedMinting
{-# INLINEABLE untyped #-}

type SignedMinting = MintingContract "signed-minting"

contract :: PubKeyHash -> SignedMinting
contract pkh = mkMintingContract ($$(compile [||untyped||]) `applyCode` liftCode pkh)

instance MintingEndpoint SignedMinting where
  data MintParam SignedMinting
    = Mint !Integer !TokenName
    | Burn !Integer !TokenName
    deriving (Generic, FromJSON, ToJSON)
  mint :: MintParam SignedMinting -> ContractM SignedMinting ()
  mint mp = do
    pkh <- getOwnPkh
    let contract' = contract pkh
        mint' :: Integer -> TokenName -> ContractM SignedMinting ()
        mint' tQuantity tName = do
          submitAndConfirm
            Tx
              { lookups = scriptLookupsFor contract',
                constraints =
                  mustMint contract' tName tQuantity
                    <> mustSign pkh
              }
          let mintAction = if tQuantity > 0 then "Minted" :: String else "Burned"
          logStr $ printf "%s %d %s" mintAction (abs tQuantity) (show tName)
    case mp of
      Mint tQuantity tName -> mint' tQuantity tName
      Burn tQuantity tName -> mint' (negate tQuantity) tName

test :: EmulatorTest
test =
  initEmulator @SignedMinting
    2
    [ Mint 1_000_000 "jambcoin" `forWallet` 1,
      Mint 1_000_000 "jambcoin" `forWallet` 2, -- different currency symbol!
      Burn 100_000 "jambcoin" `forWallet` 1
    ]

exports :: JambExports
exports =
  export
    (defExports contract')
      { emulatorTest = test
      }
  where
    -- The minting policy must be applied to some parameter to generate a hash or serialise the script to a file.
    -- 1. Use the `key-hash` script from cardano-cli-guru to get the pubkey hash for a beneficiary address.
    -- 2. Replace the placeholder hex string below with the address pubkey hash.
    contract' = contract "3a5039efcafd4c82c9169b35afb27a17673f6ed785ea087139a65a5d"