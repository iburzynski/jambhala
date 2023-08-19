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
  data MintParam SignedMinting = Mint
    { signatory :: !PubKeyHash,
      tokenName :: !TokenName,
      tokenQuantity :: !Integer
    }
    deriving (Generic, FromJSON, ToJSON)
  mint :: MintParam SignedMinting -> ContractM SignedMinting ()
  mint Mint {..} = do
    let contract' = contract signatory
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor contract',
          constraints =
            mustMint contract' tokenName tokenQuantity
              <> mustSign signatory
        }

test :: EmulatorTest
test =
  initEmulator @SignedMinting
    2
    [ Mint
        { signatory = pkhForWallet 1,
          tokenName = "jambcoin",
          tokenQuantity = 1_000_000
        }
        `forWallet` 1, -- can mint
      Mint
        { signatory = pkhForWallet 1,
          tokenName = "jambcoin",
          tokenQuantity = 1_000_000
        }
        `forWallet` 2 -- can't mint (wrong signatory)
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