module Contracts.Samples.NFT where

import Jambhala.Plutus
import Jambhala.Utils

data Mode = Minting | Burning

makeIsDataIndexed ''Mode [('Minting, 0), ('Burning, 1)]

checkMintAmount :: TokenName -> Mode -> Value -> Bool
checkMintAmount tn mode minted =
  let q = case mode of
        Minting -> 1
        Burning -> -1
   in case flattenValue minted of
        [(_, tn', q')] -> tn' #== tn && q' #== q
        _wrongAmt -> False
{-# INLINEABLE checkMintAmount #-}

hasUTxO :: TxOutRef -> [TxInInfo] -> Bool
hasUTxO oref = pany (\(TxInInfo oref' _) -> oref' #== oref)
{-# INLINEABLE hasUTxO #-}

nftPolicy :: TxOutRef -> TokenName -> Mode -> ScriptContext -> Bool
nftPolicy oref tn mode (ScriptContext TxInfo {..} _) =
  pand $
    traceIfFalse "Wrong amount" (checkMintAmount tn mode txInfoMint) :
    case mode of
      Minting -> [traceIfFalse "UTxO not consumed" $ hasUTxO oref txInfoInputs]
      Burning -> []
{-# INLINEABLE nftPolicy #-}

untyped :: TxOutRef -> TokenName -> UntypedMintingPolicy
untyped oref tn = mkUntypedMintingPolicy $ nftPolicy oref tn
{-# INLINEABLE untyped #-}

type NFTMinting = MintingContract "nft"

contract :: TxOutRef -> TokenName -> NFTMinting
contract oref tn =
  mkMintingContract $
    $$(compile [||untyped||])
      `applyCode` liftCode oref
      `applyCode` liftCode tn

instance MintingEndpoint NFTMinting where
  data MintParam NFTMinting
    = Mint !TokenName
    | Burn !TxOutRef !TokenName
    deriving (Generic, FromJSON, ToJSON)
  mint :: MintParam NFTMinting -> ContractM NFTMinting ()
  mint mp = do
    minterUtxos <- ownUtxos
    case mp of
      Mint tName -> do
        oref <- getUnspentOutput
        let contract' = contract oref tName
        submitAndConfirm
          Tx
            { lookups = scriptLookupsFor contract' `andUtxos` minterUtxos,
              constraints =
                mustMintWithRedeemer contract' Minting tName 1
                  <> mustSpendPubKeyOutput oref
            }
        logStr $ "Minted 1 " ++ show tName
      Burn mintTxOutRef tName -> do
        let contract' = contract mintTxOutRef tName
            cSymbol = getCurrencySymbol contract'
        submitAndConfirm
          Tx
            { lookups = scriptLookupsFor contract',
              constraints = mustMintWithRedeemer contract' Burning tName (-1)
            }
        logStr $ "Burned 1 " ++ show tName

test :: EmulatorTest
test =
  initEmulator @NFTMinting
    2
    [ Mint "jambtoken" `forWallet` 1,
      Mint "jambtoken" `forWallet` 1,
      Mint "jambtoken" `forWallet` 2,
      Burn (unsafeMkTxOutRef "899b40a640d4d3df5bb4a85b0d03be7df0509bcd7f6c1e99075423852a35a2a4" 10) "jambtoken"
        `forWallet` 1
    ]

exports :: JambExports
exports =
  export
    (defExports contract')
      { dataExports = [Minting `toJSONfile` "mintmode", Burning `toJSONfile` "burnmode"],
        emulatorTest = test
      }
  where
    oref = unsafeMkTxOutRef "8a2dfceaa95b54e0f9cf1416b879366fa99dd073ac1e1558151e7154972fdd8a" 1
    contract' = contract oref "jambtoken"