-- 0. Create Module & Declare Imports
module Contracts.Samples.ForwardMinting where

import Data.Map qualified as Map
import Jambhala.Plutus
import Jambhala.Utils

-- 1. Declare Types

-- Synonyms for clearer semantics.
type HostPKH = PubKeyHash

type RedeemerCode = BuiltinByteString

type RedeemerCodeString = String

type TicketPrice = Integer

-- | Custom datum type.
data TicketDatum = TicketDatum
  { policySymbol :: CurrencySymbol
  , redeemerCodeHash :: BuiltinByteString
  , ticketPrice :: TicketPrice
  }
  -- Must be convertible to/from JSON due to reuse in off-chain emulator code
  deriving (Generic, FromJSON, ToJSON)

unstableMakeIsData ''TicketDatum

-- 2. Define Helper Functions & Lambda

-- | Helper function to check if any UTxO pays the ticket price to the host.
checkPayment :: HostPKH -> TicketPrice -> [TxOut] -> Bool
checkPayment hostPKH price = pany (isValidOutput hostPKH price)
{-# INLINEABLE checkPayment #-}

-- | Helper function to check if an individual UTxO pays the ticket price to the host.
isValidOutput :: HostPKH -> TicketPrice -> TxOut -> Bool
isValidOutput hostPKH price TxOut {..} = case toPubKeyHash txOutAddress of
  Nothing -> False
  Just pkh -> pkh #== hostPKH && valueOf txOutValue adaSymbol adaToken #== price
{-# INLINEABLE isValidOutput #-}

-- | Helper function to check that the correct quantity of the given token is minted.
checkMint :: CurrencySymbol -> TokenName -> Value -> Bool
checkMint cs tn minting = pany (\(cs', tn', q) -> cs' #== cs && tn' #== tn && q #== 1) $ flattenValue minting
{-# INLINEABLE checkMint #-}

-- | Helper function to generate a unique token name from a UTxO input.
mkTicketName :: TxOutRef -> TokenName
mkTicketName (TxOutRef (TxId txHash) txIdx) =
  TokenName . sha2_256 $ consByteString txIdx txHash
{-# INLINEABLE mkTicketName #-}

-- | Define typed version of the spending validator lambda.
ticketingLambda :: HostPKH -> TicketDatum -> RedeemerCode -> ScriptContext -> Bool
ticketingLambda pkh TicketDatum {..} redeemerCode ctx@(ScriptContext TxInfo {..} _) =
  case findOwnInput ctx of
    Nothing -> trace "Input not found" False
    Just (TxInInfo oref _) ->
      traceIfFalse "Invalid redeemer code" (sha2_256 redeemerCode #== redeemerCodeHash)
        && traceIfFalse "Insufficient payment to host" (checkPayment pkh ticketPrice txInfoOutputs)
        && traceIfFalse "Invalid ticket mint" (checkMint policySymbol (mkTicketName oref) txInfoMint)
{-# INLINEABLE ticketingLambda #-}

-- | Untyped version of the spending validator lambda.
untypedLambda :: HostPKH -> UntypedValidator -- BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedLambda = mkUntypedValidator . ticketingLambda
{-# INLINEABLE untypedLambda #-}

-- 3. Pre-compilation

-- | The type synonym for the compiled spending validator script.
type TicketValidator = ValidatorContract "forward-minting"

-- | Function for producing the compiled spending validator script.
compileValidator :: HostPKH -> TicketValidator
compileValidator pkh = mkValidatorContract ($$(compile [||untypedLambda||]) `applyCode` liftCode pkh)

-- | Synonym for the forwarding minting policy script.
type TicketPolicy = MintingContract "forward-minting-policy"

-- | Function for producing the compiled forward minting policy script.
compilePolicy :: HostPKH -> TicketPolicy
compilePolicy = getFwdMintingPolicy . compileValidator

-- 4. Export Contract to Jambhala

-- | Get a PKH for a testnet address to apply the parameterized script to.
sampleHost :: HostPKH
sampleHost = "3a5039efcafd4c82c9169b35afb27a17673f6ed785ea087139a65a5d"

-- | Helper function for assembling a datum.
mkTicketDatum :: HostPKH -> RedeemerCodeString -> TicketPrice -> TicketDatum
mkTicketDatum hostPKH redeemerCodeStr price =
  TicketDatum
    { policySymbol = getFwdMintingPolicyId (compileValidator hostPKH)
    , redeemerCodeHash = sha2_256 $ stringToBuiltinByteString redeemerCodeStr
    , ticketPrice = price
    }

-- | Define spending validator exports value for use with `j cli`.
validatorExports :: JambExports
validatorExports =
  export
    (defExports $ compileValidator sampleHost)
      { dataExports =
          [ mkSampleTicketDatum "E875RPSE9M" 50_000_000 `toJSONfile` "goldTicket"
          , mkSampleTicketDatum "YIMNUUU528" 25_000_000 `toJSONfile` "silverTicket"
          , stringToBuiltinByteString "E875RPSE9M" `toJSONfile` "goldRedeemer"
          , stringToBuiltinByteString "YIMNUUU528" `toJSONfile` "silverRedeemer"
          ]
      , emulatorTest = test
      }
  where
    mkSampleTicketDatum = mkTicketDatum sampleHost

-- | Define minting policy exports value for use with `jamb` CLI.
policyExports :: JambExports
policyExports =
  export (defExports $ compilePolicy sampleHost) {dataExports = [() `toJSONfile` "unit"]}

-- EMULATOR COMPONENT ---

instance ValidatorEndpoints TicketValidator where
  newtype GiveParam TicketValidator = Register TicketDatum
    deriving (Generic, FromJSON, ToJSON)
  data GrabParam TicketValidator = Claim
    { hostPKH :: HostPKH
    , redeemerCode :: BuiltinByteString
    }
    deriving (Generic, FromJSON, ToJSON)

  give :: GiveParam TicketValidator -> ContractM TicketValidator ()
  give (Register datum) = do
    hostPKH <- getOwnPKH
    let appliedValidator = compileValidator hostPKH
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor appliedValidator
        , constraints = mustPayScriptWithDatum appliedValidator datum (lovelaceValueOf 2_000_000)
        }
    logStr $
      printf
        "Registered ticket with price %d lovelace"
        (ticketPrice datum)

  grab :: GrabParam TicketValidator -> ContractM TicketValidator ()
  grab Claim {..} = do
    utxos <- getUtxosAt appliedValidator
    let validUtxos = filterByDatum (hasCode redeemerCode) utxos
    case Map.toList validUtxos of
      (oref, dTxOut) : _ -> do
        let TicketDatum {..} = fromJust $ convertDecoratedTxOutDatum dTxOut
        submitAndConfirm
          Tx
            { lookups = scriptLookupsFor appliedValidator `andUtxos` validUtxos
            , constraints =
                mconcat
                  [ oref `mustBeSpentWith` redeemerCode
                  , mustPayPKH hostPKH (lovelaceValueOf ticketPrice)
                  , mustMint (compilePolicy hostPKH) (mkTicketName oref) 1
                  ]
            }
        logStr $ printf "Claimed ticket with redeemer code %s" (show redeemerCode)
      _noTicket -> logStr "No eligible ticket found"
    where
      appliedValidator = compileValidator hostPKH
      hasCode :: RedeemerCode -> TicketDatum -> Bool
      hasCode rc ticketDatum = sha2_256 rc == redeemerCodeHash ticketDatum

-- | Define emulator test.
test :: EmulatorTest
test =
  initEmulator @TicketValidator
    3
    [ Register (mkTicketDatum1 "E875RPSE9M" 50_000_000) `fromWallet` 1
    , Register (mkTicketDatum1 "YIMNUUU528" 25_000_000) `fromWallet` 1
    , Claim {hostPKH = pkh1, redeemerCode = "E875RPSE9M"} `toWallet` 2
    , Claim {hostPKH = pkh1, redeemerCode = "YIMNUUU528"} `toWallet` 3
    ]
  where
    pkh1 = pkhForWallet 1
    mkTicketDatum1 :: RedeemerCodeString -> TicketPrice -> TicketDatum
    mkTicketDatum1 = mkTicketDatum pkh1

-- Misc. Utilities

{- | Helper function to get the token name for the ticket NFT that will be minted when consuming a given UTxO "voucher".
   This can be used to create a token whitelist for admission at the event.
-}
getTicketName :: Text -> Integer -> String
getTicketName txHash txIdx = tokenNameToString . mkTicketName $ unsafeMkTxOutRef txHash txIdx
