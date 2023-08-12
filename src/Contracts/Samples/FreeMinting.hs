module Contracts.Samples.FreeMinting where

import Jambhala.Plutus
import Jambhala.Utils

freeMinting :: () -> ScriptContext -> Bool
freeMinting _ _ = True
{-# INLINEABLE freeMinting #-}

type FreeMinting = MintingContract "free-minting"

contract :: FreeMinting
contract = mkMintingContract $$(compile [||mkUntypedMintingPolicy freeMinting||])

instance MintingEndpoint FreeMinting where
  data MintParam FreeMinting = Mint
    { tokenName :: !TokenName,
      tokenQuantity :: !Integer
    }
    deriving (Generic, FromJSON, ToJSON)
  mint :: MintParam FreeMinting -> ContractM FreeMinting ()
  mint Mint {..} = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor contract,
          constraints = mustMint contract tokenName tokenQuantity
        }

test :: EmulatorTest
test =
  initEmulator @FreeMinting
    1
    [ Mint
        { tokenName = "jambcoin",
          tokenQuantity = 1_000_000
        }
        `forWallet` 1
    ]

exports :: JambExports
exports =
  export
    (defExports contract)
      { emulatorTest = test
      }