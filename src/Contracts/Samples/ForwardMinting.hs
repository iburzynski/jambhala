module Contracts.Samples.ForwardMinting where

import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Jambhala.Plutus
import Jambhala.Utils

data TicketDatum = TicketDatum
  { cSymbol :: CurrencySymbol,
    code :: BuiltinByteString,
    ticketPrice :: Integer
  }
  deriving (Generic, FromJSON, ToJSON)

unstableMakeIsData ''TicketDatum

checkPayment :: PubKeyHash -> Integer -> [TxOut] -> Bool
checkPayment pkh price = pany (isValidOutput pkh price)
{-# INLINEABLE checkPayment #-}

isValidOutput :: PubKeyHash -> Integer -> TxOut -> Bool
isValidOutput pkh price TxOut {..} = case toPubKeyHash txOutAddress of
  Just pkh' -> pkh' #== pkh && valueOf txOutValue adaSymbol adaToken #>= price
  Nothing -> False
{-# INLINEABLE isValidOutput #-}

checkMint :: CurrencySymbol -> TokenName -> Value -> Bool
checkMint cs tn minted = pany (\(cs', tn', q) -> cs' #== cs && tn' #== tn && q #== 1) $ flattenValue minted
{-# INLINEABLE checkMint #-}

ticketing :: PubKeyHash -> TicketDatum -> BuiltinByteString -> ScriptContext -> Bool
ticketing pkh TicketDatum {..} claimCode (ScriptContext TxInfo {..} _) =
  traceIfFalse "Invalid access code" (sha2_256 claimCode #== code)
    && traceIfFalse "Insufficient payment" (checkPayment pkh ticketPrice txInfoOutputs)
    && traceIfFalse "Invalid ticket mint" (checkMint cSymbol (TokenName claimCode) txInfoMint)
{-# INLINEABLE ticketing #-}

untyped :: PubKeyHash -> UntypedValidator
untyped = mkUntypedValidator . ticketing
{-# INLINEABLE untyped #-}

type TicketValidator = ValidatorContract "forward-minting-validator"

validator :: PubKeyHash -> TicketValidator
validator pkh = mkValidatorContract ($$(compile [||untyped||]) `applyCode` liftCode pkh)

type TicketPolicy = MintingContract "forward-minting-policy"

ticketPolicy :: PubKeyHash -> TicketPolicy
ticketPolicy = getFwdMintingPolicy . validator

instance ValidatorEndpoints TicketValidator where
  newtype GiveParam TicketValidator = Register TicketDatum
    deriving (Generic, FromJSON, ToJSON)
  data GrabParam TicketValidator = Claim
    { payee :: PubKeyHash,
      claimCode :: BuiltinByteString
    }
    deriving (Generic, FromJSON, ToJSON)

  give :: GiveParam TicketValidator -> ContractM TicketValidator ()
  give (Register datum@TicketDatum {..}) = do
    pkh <- getOwnPkh
    let validator' = validator pkh
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor validator',
          constraints = mustPayToScriptWithDatum validator' datum 2_000_000
        }
    logStr $
      printf
        "Registered ticket with price %d lovelace"
        ticketPrice

  grab :: GrabParam TicketValidator -> ContractM TicketValidator ()
  grab Claim {..} = do
    utxos <- getUtxosAt validator'
    let validUtxos = Map.mapMaybe (isEligible claimCode) utxos
    case Map.toList validUtxos of
      (txOutRef, txOut) : _ -> do
        let TicketDatum {..} = fromJust $ getTicketDatum txOut
        submitAndConfirm
          Tx
            { lookups = scriptLookupsFor validator' `andUtxos` validUtxos,
              constraints =
                mconcat
                  [ txOutRef `mustBeSpentWith` claimCode,
                    mustPayToPubKey (PaymentPubKeyHash payee) (singleton adaSymbol adaToken ticketPrice),
                    mustMint (ticketPolicy payee) (TokenName claimCode) 1
                  ]
            }
        logStr $ printf "Claimed ticket %s" (show claimCode)
      _noTicket -> logStr "No eligible ticket found"
    where
      validator' = validator payee
      getTicketDatum :: DecoratedTxOut -> Maybe TicketDatum
      getTicketDatum dto = do
        (_, dfq) <- getDecoratedTxOutDatum dto
        Datum d <- getDatumInDatumFromQuery dfq
        fromBuiltinData d
      isEligible :: BuiltinByteString -> DecoratedTxOut -> Maybe DecoratedTxOut
      isEligible cc dto = do
        TicketDatum {..} <- getTicketDatum dto
        guard (sha2_256 cc == code)
        Just dto

mkTicketDatum :: PubKeyHash -> String -> Integer -> TicketDatum
mkTicketDatum pkh codeStr price =
  TicketDatum
    { cSymbol = getFwdMintingPolicyId (validator pkh),
      code = sha2_256 $ stringToBuiltinByteString codeStr,
      ticketPrice = price
    }

test :: EmulatorTest
test =
  initEmulator @TicketValidator
    3
    [ Register (mkTicketDatum' "DWGCGQkW7i" 25_000_000) `fromWallet` 1,
      Register (mkTicketDatum' "JvXw7RJtvo" 50_000_000) `fromWallet` 1,
      Claim {payee = pkhForWallet 1, claimCode = stringToBuiltinByteString "DWGCGQkW7i"} `toWallet` 2,
      Claim {payee = pkhForWallet 1, claimCode = stringToBuiltinByteString "JvXw7RJtvo"} `toWallet` 3
    ]
  where
    mkTicketDatum' = mkTicketDatum $ pkhForWallet 1

-- EXPORTS
samplePkh :: PubKeyHash
samplePkh = "3a5039efcafd4c82c9169b35afb27a17673f6ed785ea087139a65a5d"

validatorExports :: JambExports
validatorExports =
  export
    (defExports validator')
      { dataExports =
          [ mkTicketDatum' "DWGCGQkW7i" 25_000_000 `toJSONfile` "ticketSilver",
            mkTicketDatum' "JvXw7RJtvo" 50_000_000 `toJSONfile` "ticketGold"
          ],
        emulatorTest = test
      }
  where
    validator' = validator samplePkh
    mkTicketDatum' = mkTicketDatum samplePkh

policyExports :: JambExports
policyExports = export (defExports $ ticketPolicy samplePkh)