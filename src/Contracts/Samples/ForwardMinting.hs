-- 0. Create Module & Declare Imports
module Contracts.Samples.ForwardMinting where

import Data.Map qualified as Map
import Data.Maybe (fromJust)
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
  { policySymbol :: CurrencySymbol,
    redeemerCodeHash :: BuiltinByteString,
    ticketPrice :: Integer
  }
  -- Must be convertible to/from JSON due to reuse in off-chain emulator code
  deriving (Generic, FromJSON, ToJSON)

unstableMakeIsData ''TicketDatum

-- 2. Define Lambda

-- | Helper function to check if any UTxO pays the ticket price to the host.
checkPayment :: HostPKH -> TicketPrice -> [TxOut] -> Bool
checkPayment pkh price = pany (isValidOutput pkh price)
{-# INLINEABLE checkPayment #-}

-- | Helper function to check if an individual UTxO pays the ticket price to the host.
isValidOutput :: HostPKH -> TicketPrice -> TxOut -> Bool
isValidOutput pkh price TxOut {..} = case toPubKeyHash txOutAddress of
  Nothing -> False
  Just pkh' -> pkh' #== pkh && valueOf txOutValue adaSymbol adaToken #== price
{-# INLINEABLE isValidOutput #-}

-- | Helper function to check that the correct quantity of the given token is minted.
checkMint :: CurrencySymbol -> TokenName -> Value -> Bool
checkMint cs tn minted =
  pany (\(cs', tn', q) -> cs' #== cs && tn' #== tn && q #== 1) $ flattenValue minted
{-# INLINEABLE checkMint #-}

-- | Helper function to generate a unique token name from a UTxO input.
mkTicketName :: TxOutRef -> TokenName
mkTicketName (TxOutRef (TxId txHash) txIdx) =
  -- 1. Convert txIdx (:: Integer) to BuiltinData with mkI
  -- 2. Convert BuiltinData to BuiltinByteString with serialiseData
  -- 3. Concatenate txHash and index bytestring with appendByteString
  -- 4. Hash the concatenated bytestring (to prevent exceeding token name size limit)
  -- 5. Apply TokenName constructor to hashed bytestring
  TokenName . sha2_256 . appendByteString txHash . serialiseData $ mkI txIdx
{-# INLINEABLE mkTicketName #-}

-- | Define typed version of the spending validator lambda.
ticketingLambda :: HostPKH -> TicketDatum -> RedeemerCode -> ScriptContext -> Bool
ticketingLambda pkh TicketDatum {..} redeemerCode ctx@(ScriptContext TxInfo {..} _) =
  case findOwnInput ctx of
    Nothing -> trace "Input not found" False
    Just (TxInInfo oref _) ->
      traceIfFalse "Invalid redeemer code" (sha2_256 redeemerCode #== redeemerCodeHash)
        && traceIfFalse "Insufficient payment" (checkPayment pkh ticketPrice txInfoOutputs)
        && traceIfFalse "Invalid ticket mint" (checkMint policySymbol (mkTicketName oref) txInfoMint)
{-# INLINEABLE ticketingLambda #-}

-- | Untyped version of the spending validator lambda.
untypedLambda :: HostPKH -> UntypedValidator
untypedLambda = mkUntypedValidator . ticketingLambda
{-# INLINEABLE untypedLambda #-}

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
sampleHostPKH :: HostPKH
sampleHostPKH = "3a5039efcafd4c82c9169b35afb27a17673f6ed785ea087139a65a5d"

-- | Define spending validator exports value for use with `jamb` CLI.
validatorExports :: JambExports
validatorExports =
  export
    (defExports $ compileValidator sampleHostPKH)
      { dataExports =
          [ mkTicketDatum' "DWGCGQkW7i" 25_000_000 `toJSONfile` "ticketSilver",
            mkTicketDatum' "JvXw7RJtvo" 50_000_000 `toJSONfile` "ticketGold"
          ],
        emulatorTest = test
      }
  where
    mkTicketDatum' = mkTicketDatum sampleHostPKH

-- | Define minting policy exports value for use with `jamb` CLI.
policyExports :: JambExports
policyExports = export (defExports $ compilePolicy sampleHostPKH)

-- 5. Define Emulator Component

-- | Define `ValidatorEndpoints` instance for the validator synonym (`MintingEndpoint` instance not required for the policy synonym).
instance ValidatorEndpoints TicketValidator where
  newtype GiveParam TicketValidator = Register TicketDatum
    deriving (Generic, FromJSON, ToJSON)
  data GrabParam TicketValidator = Claim
    { hostPKH :: PubKeyHash,
      redeemerCode :: BuiltinByteString
    }
    deriving (Generic, FromJSON, ToJSON)

  give :: GiveParam TicketValidator -> ContractM TicketValidator ()
  give (Register datum@TicketDatum {..}) = do
    hostPKH <- getOwnPKH
    let appliedValidator = compileValidator hostPKH
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor appliedValidator, -- lookups for the associated forwarding minting policy are automatically included
          constraints = mustPayScriptWithDatum appliedValidator datum (lovelaceValueOf 2_000_000)
        }
    logStr $
      printf
        "Registered ticket with price %d lovelace"
        ticketPrice

  grab :: GrabParam TicketValidator -> ContractM TicketValidator ()
  grab Claim {..} = do
    utxos <- getUtxosAt appliedValidator
    let validUtxos = filterByDatum (hasCode redeemerCode) utxos
    case Map.toList validUtxos of
      (oref, dTxOut) : _ -> do
        let TicketDatum {..} = fromJust $ convertDecoratedTxOutDatum dTxOut
        submitAndConfirm
          Tx
            { lookups = scriptLookupsFor appliedValidator `andUtxos` validUtxos,
              constraints =
                mconcat
                  [ oref `mustBeSpentWith` redeemerCode,
                    mustPayPKH hostPKH (lovelaceValueOf ticketPrice),
                    mustMint (compilePolicy hostPKH) (mkTicketName oref) 1
                  ]
            }
        logStr $ printf "Claimed ticket with redeemer code %s" (show redeemerCode)
      _noTicket -> logStr "No eligible ticket found"
    where
      appliedValidator :: TicketValidator
      appliedValidator = compileValidator hostPKH
      hasCode :: RedeemerCode -> TicketDatum -> Bool
      hasCode rc ticketDatum = sha2_256 rc == redeemerCodeHash ticketDatum

-- | Helper function for assembling a datum.
mkTicketDatum :: HostPKH -> RedeemerCodeString -> TicketPrice -> TicketDatum
mkTicketDatum hostPKH redeemerCodeStr price =
  TicketDatum
    { policySymbol = getFwdMintingPolicyId (compileValidator hostPKH),
      redeemerCodeHash = sha2_256 $ stringToBuiltinByteString redeemerCodeStr,
      ticketPrice = price
    }

-- | Define emulator test.
test :: EmulatorTest
test =
  initEmulator @TicketValidator
    3
    [ Register (mkTicketDatum1 "DWGCGQkW7i" 25_000_000) `fromWallet` 1,
      Register (mkTicketDatum1 "JvXw7RJtvo" 50_000_000) `fromWallet` 1,
      Claim {hostPKH = pkhForWallet 1, redeemerCode = stringToBuiltinByteString "DWGCGQkW7i"} `toWallet` 2,
      Claim {hostPKH = pkhForWallet 1, redeemerCode = stringToBuiltinByteString "JvXw7RJtvo"} `toWallet` 3
    ]
  where
    mkTicketDatum1 :: RedeemerCodeString -> TicketPrice -> TicketDatum
    mkTicketDatum1 = mkTicketDatum $ pkhForWallet 1 -- construct datum with Wallet 1 as host

-- Misc. Utilities

-- | Helper function to get the token name for the ticket NFT that will be minted when consuming a given UTxO "voucher".
--   This can be used to create a token whitelist for admission at the event.
getTicketName :: Text -> Integer -> TokenName
getTicketName txHash = mkTicketName . unsafeMkTxOutRef txHash