module Contracts.Samples.FreeMinting where

import Jambhala.Plutus
import Jambhala.Utils

freeMinting :: () -> ScriptContext -> Bool
freeMinting _ _ = True
{-# INLINEABLE freeMinting #-}

-- type UntypedMintingPolicy = BuiltinData -> BuiltinData -> ()
untyped :: UntypedMintingPolicy
untyped = mkUntypedMintingPolicy freeMinting
{-# INLINEABLE untyped #-}

type FreeMinting = MintingContract "free-minting"

contract :: FreeMinting -- MintingContract "free-minting"
contract = mkMintingContract $$(compile [||untyped||])

instance MintingEndpoint FreeMinting where
  data MintParam FreeMinting
    = Mint !Integer !TokenName
    | Burn !Integer !TokenName
    deriving (Generic, FromJSON, ToJSON)

  mint :: MintParam FreeMinting -> ContractM FreeMinting ()
  mint (Mint tQuantity tName) = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor contract,
          constraints = mustMint contract tName tQuantity
        }
    logStr $ printf "Minted %d %s" tQuantity (show tName)
  mint (Burn tQuantity tName) = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor contract,
          constraints = mustMint contract tName (negate tQuantity)
        }
    logStr $ printf "Burned %d %s" tQuantity (show tName)

test :: EmulatorTest
test =
  initEmulator @FreeMinting
    2
    [ Mint 1_000_000 "jambcoin" `forWallet` 1,
      Mint 1_000_000 "jambcoin" `forWallet` 2,
      Burn 100_000 "jambcoin" `forWallet` 1
    ]

exports :: JambExports
exports =
  export
    (defExports contract)
      { emulatorTest = test
      }