-- 0. Create Module & Declare Imports
module Contracts.Samples.Auction where

import Contracts.Samples.NFT qualified as NFT
import Data.Map qualified as Map
import Jambhala.Plutus
import Jambhala.Utils
import Plutus.V1.Ledger.Value (geq)

-- 1. Declare Types
type BidAmt = Integer

data AuctionParam = AuctionParam
  { apAsset :: Value
  -- ^ The asset being auctioned. It can be a single token, multiple tokens of the same
  -- kind, or tokens of different kinds, and the token(s) can be fungible or non-fungible.
  -- These can all be encoded as a `Value`.
  , apSeller :: PubKeyHash
  -- ^ Seller's wallet address. The highest bid (if exists) will be sent to the seller.
  -- If there is no bid, the asset auctioned will be sent to the seller.
  , apMinBid :: BidAmt
  -- ^ The minimum bid in Lovelace.
  , apEndTime :: POSIXTime
  -- ^ The deadline for placing a bid. This is the earliest time the auction can be closed.
  }
  deriving (Generic, FromJSON, ToJSON)

makeLift ''AuctionParam

data Bid = Bid
  { bBidder :: PubKeyHash
  -- ^ Bidder's wallet address.
  , bAmount :: BidAmt
  -- ^ Bid amount.
  }

unstableMakeIsData ''Bid

{- | Datum represents the state of a smart contract. In this case
 it contains the highest bid so far (if exists).
-}
type AuctionDatum = Maybe Bid

{- | Redeemer is the input that changes the state of a smart contract.
 In this case it is either a new bid, or a request to close the auction
 and pay out the seller and the highest bidder.
-}
data AuctionRedeemer = NewBid Bid | Payout

makeIsDataIndexed ''AuctionRedeemer [('NewBid, 0), ('Payout, 1)]

-- 2. Define Lambda

-- | Define typed version of the validator lambda.
auctionLambda ::
  AuctionParam ->
  AuctionDatum ->
  AuctionRedeemer ->
  ScriptContext ->
  Bool
auctionLambda AuctionParam {..} highBid redeemer ctx@(ScriptContext TxInfo {..} _) =
  traceIfFalse "end time not reached" $
    pand $ case redeemer of
      NewBid bid@(Bid _ bidAmt) ->
        [ maybe
            -- If this is the first bid, it must be at least as high as the minimum bid:
            (bidAmt #>= apMinBid)
            -- Otherwise, the new bid must be at least the highest bid + 1%:
            (\(Bid _ curBid) -> bidAmt #>= curBid #+ (curBid `divideInteger` 100))
            highBid
        , -- The bid is not too late.
          to apEndTime `contains` txInfoValidRange
        , -- The previous highest bid should be refunded.
          refundsPrevHighBidder txInfoOutputs highBid
        , -- A correct new datum is produced, containing the new highest bid.
          validContOutput (getContinuingOutputs ctx) apAsset bid
        ]
      Payout ->
        [ -- The end time has occurred.
          from apEndTime `contains` txInfoValidRange
        , -- The seller gets the highest bid.
          paysSeller txInfoOutputs apSeller highBid
        , -- The highest bidder gets the asset (seller reclaims if no bid).
          paysAsset txInfoOutputs apSeller apAsset highBid
        ]
{-# INLINEABLE auctionLambda #-}

paysValue :: Value -> [TxOut] -> PubKeyHash -> BuiltinString -> Bool
paysValue val txInfoOutputs recipient errDetails =
  traceIfFalse ("Output not found: " #<> errDetails) $
    pany
      ( \o ->
          txOutAddress o #== pubKeyHashAddress recipient
            && txOutValue o #== val
      )
      txInfoOutputs
{-# INLINEABLE paysValue #-}

paysAda :: BidAmt -> [TxOut] -> PubKeyHash -> BuiltinString -> Bool
paysAda = paysValue . lovelaceValueOf
{-# INLINEABLE paysAda #-}

refundsPrevHighBidder :: [TxOut] -> Maybe Bid -> Bool
refundsPrevHighBidder txInfoOutputs =
  -- mBid
  maybe True (\(Bid bidder prevHighBid) -> paysAda prevHighBid txInfoOutputs bidder "bid refund")
-- case mBid of
-- Nothing -> True
-- Just (Bid bidder prevHighBid) ->
{-# INLINEABLE refundsPrevHighBidder #-}

paysSeller :: [TxOut] -> PubKeyHash -> Maybe Bid -> Bool
paysSeller txInfoOutputs seller =
  maybe True (\(Bid _ amt) -> paysAda amt txInfoOutputs seller "seller payment")
{-# INLINEABLE paysSeller #-}

paysAsset :: [TxOut] -> PubKeyHash -> Value -> Maybe Bid -> Bool
paysAsset txInfoOutputs seller asset =
  maybe
    -- Seller reclaims asset if no bid
    (paysValue asset txInfoOutputs seller "asset reclamation")
    -- Else asset paid to highest bidder
    (\(Bid bidder _) -> paysValue asset txInfoOutputs bidder "winner asset")
{-# INLINEABLE paysAsset #-}

validContOutput :: [TxOut] -> Value -> Bid -> Bool
validContOutput [o] asset (Bid bidder amt) = case (txOutDatum o, txOutValue o) of
  (OutputDatum (Datum newDatum), val)
    | val #== (asset #<> lovelaceValueOf amt) ->
        maybe
          (traceError "Failed to decode output datum")
          ( traceIfFalse "Invalid bid in output datum"
              . maybe
                False
                (\(Bid bidder' amt') -> bidder' #== bidder && amt' #== amt)
          )
          $ fromBuiltinData newDatum
    | otherwise -> traceError "Invalid continuing output value"
  _noInlineDatum -> traceError "No inline datum on continuing output"
validContOutput [] _ _ = traceError "No continuing output"
validContOutput _ _ _ = traceError "Multiple continuing outputs"
{-# INLINEABLE validContOutput #-}

-- | Convert lambda into "untyped" form before pre-compilation.
untypedLambda :: AuctionParam -> UntypedValidator
untypedLambda p = mkUntypedValidator (auctionLambda p)
{-# INLINEABLE untypedLambda #-}

-- 3. Pre-Compile Lambda

-- | Declare contract synonym with unique symbolic identifier.
type Auction = ValidatorContract "auction"

compileScript :: AuctionParam -> Auction
compileScript p = mkValidatorContract ($$(compile [||untypedLambda||]) `applyCode` liftCode p)

mkAuctionParam :: CurrencySymbol -> TokenName -> PubKeyHash -> BidAmt -> POSIXTime -> AuctionParam
mkAuctionParam cs tn = AuctionParam asset
  where
    asset = singleton adaSymbol adaToken 2_000_000 <> singleton cs tn 1

-- 4. Export Contract to Jambhala

exports :: JambExports
exports = export (defExports compiled) {emulatorTest = NFT.test <> test}
  where
    compiled :: Auction
    compiled =
      compileScript $
        mkAuctionParam
          -- Copy/paste the currency symbol (policy ID) for the NFT being auction
          ""
          -- Choose a token name
          ""
          -- Copy/paste the pubkey hash for the testnet address holding the auctioned NFT
          ""
          -- Choose a minimum bid (in lovelace):
          2_000_000
          -- Select an end time for the auction (use `calc-time --plus X` script, replacing X with a number of minutes from now)
          -- (Note the slot time to set a compatible validity interval in cardano-cli)
          0

-- 5. Define Emulator Component

data AuctionSubmission = AS
  { compiledScript :: Auction
  , redeemer :: AuctionRedeemer
  , noBidConstraints :: TxConstraints () ()
  , hasBidConstraints :: Bid -> TxConstraints () ()
  , bidMsg :: String
  }

data SubmitAction = TestBid BidAmt | TestPayout deriving (Generic, FromJSON, ToJSON)

instance ValidatorEndpoints Auction where
  data GiveParam Auction = Auction AuctionParam
    deriving (Generic, FromJSON, ToJSON)
  data GrabParam Auction = Submit AuctionParam SubmitAction
    deriving (Generic, FromJSON, ToJSON)

  give :: GiveParam Auction -> ContractM Auction ()
  give (Auction aParam@AuctionParam {..}) = do
    let compiled = compileScript aParam
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor compiled
        , constraints = mustPayScriptWithInlineDatum @AuctionDatum compiled Nothing apAsset
        }
    logStr $
      printf
        "%s started auction for %s with min bid %d lovelace and end time %s"
        (show apSeller)
        (show apAsset)
        apMinBid
        (show apEndTime)

  grab :: GrabParam Auction -> ContractM Auction ()
  grab (Submit aParam@(AuctionParam asset seller _ _) grabAction) = do
    pkh <- getOwnPKH
    let compiledScript = compileScript aParam
        submission = case grabAction of
          TestBid bidAmt ->
            let bid = Bid pkh bidAmt
                contOutputConstr =
                  mustPayScriptWithInlineDatum
                    compiledScript
                    (Just bid)
                    (asset <> lovelaceValueOf bidAmt)
             in AS
                  { compiledScript
                  , redeemer = NewBid bid
                  , noBidConstraints = contOutputConstr
                  , hasBidConstraints = \(Bid prevHighBidder prevBid) -> contOutputConstr <> mustPayPKH prevHighBidder (lovelaceValueOf prevBid)
                  , bidMsg = printf "%s placed bid of %d lovelace" (show pkh) bidAmt
                  }
          TestPayout ->
            AS
              { compiledScript
              , redeemer = Payout
              , noBidConstraints = mustPayPKH seller asset
              , hasBidConstraints = \(Bid finalHighBidder finalBid) ->
                  mustPayPKH seller (lovelaceValueOf finalBid)
                    <> mustPayPKH finalHighBidder asset
              , bidMsg = printf "%s closed auction" (show pkh)
              }
    grab' submission
    where
      grab' :: AuctionSubmission -> ContractM Auction ()
      grab' AS {..} = do
        utxos <- getUtxosAt compiledScript
        case Map.toList $ filterByValue (`geq` asset) utxos of
          [(oref, txOut)] -> do
            let submit extraConstraints = do
                  now <- getCurrentInterval
                  submitAndConfirm
                    . Tx (scriptLookupsFor compiledScript `andUtxos` Map.singleton oref txOut)
                    $ mconcat
                      [ oref `mustBeSpentWith` redeemer
                      , mustValidateInTimeRange (fromPlutusInterval now)
                      , extraConstraints
                      ]
                  logStr bidMsg
            case convertDecoratedTxOutDatum txOut of
              Nothing -> logStr "Error: invalid bid UTxO datum"
              Just maybeBid -> maybe (submit noBidConstraints) (submit . hasBidConstraints) maybeBid
          [] -> logStr "Error: no bid UTxO"
          _multi -> logStr "Error: multiple bid UTxOs"

test :: EmulatorTest
test =
  initEmulator @Auction
    3
    [ Auction aParam `fromWallet` 1
    , Submit aParam (TestBid 2_000_000) `toWallet` 2
    , Submit aParam (TestBid 3_000_000) `toWallet` 3
    , Submit aParam (TestBid 4_000_000) `toWallet` 2
    , waitUntil 20
    , Submit aParam TestPayout `toWallet` 1
    ]
  where
    aParam =
      mkAuctionParam
        "7d1acd68ccc77651952963dd983b01f3b39b92ab43aa3c4f4c7558da"
        "jambtoken"
        (pkhForWallet 1)
        2_000_000
        (defaultSlotBeginTime 20)
