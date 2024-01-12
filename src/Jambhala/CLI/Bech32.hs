module Jambhala.CLI.Bech32 where

import Codec.Binary.Bech32 (DataPart, HumanReadablePart, dataPartFromBytes, encode, humanReadablePartFromText)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bifunctor (bimap, first)
import Data.ByteArray.Encoding (convertFromBase)
import Data.ByteArray.Encoding qualified as BA
import Data.Either.Extra (maybeToEither)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import System.Environment (lookupEnv)

data AddrError
  = MissingNetworkID
  | InvalidNetworkID
  | HRPError
  | InvalidKeyHash
  | EncodingError

instance Show AddrError where
  show MissingNetworkID = "CARDANO_NODE_NETWORK_ID not set"
  show InvalidNetworkID = "Invalid CARDANO_NODE_NETWORK_ID"
  show HRPError = "Invalid human readable part"
  show InvalidKeyHash = "Key hash contains invalid characters"
  show EncodingError = "Encoded address is too long"

networkIDtoHRP :: String -> Either AddrError HumanReadablePart
networkIDtoHRP network = case network of
  "mainnet" -> getHRP "addr"
  "1" -> getHRP "addr_test"
  "2" -> getHRP "addr_test"
  _ -> Left InvalidNetworkID
  where
    getHRP = first (const HRPError) . humanReadablePartFromText

addNetworkTag :: Text -> String -> Either AddrError Text
addNetworkTag keyHash network = (<> keyHash) <$> nTag
  where
    nTag = case network of
      "mainnet" -> Right "1"
      "1" -> Right "0"
      "2" -> Right "0"
      _ -> Left InvalidNetworkID

encodeWrapper :: HumanReadablePart -> DataPart -> Either AddrError Text
encodeWrapper hrp dp = first (const EncodingError) $ encode hrp dp

bech32 :: (MonadIO m) => Int -> Text -> m String
bech32 header keyHash = do
  eitherNetwork <- liftIO $ maybeToEither MissingNetworkID <$> lookupEnv "CARDANO_NODE_NETWORK_ID"
  let eitherHRP = eitherNetwork >>= networkIDtoHRP
      eitherKeyHash = (eitherNetwork >>= addNetworkTag keyHash) <&> (T.pack (show header) <>)
      eitherDP = do
        keyHash' <- eitherKeyHash
        bimap (const InvalidKeyHash) dataPartFromBytes . convertFromBase BA.Base16 . T.encodeUtf8 $ keyHash'
      eitherAddr = do
        hrp <- eitherHRP
        dp <- eitherDP
        encodeWrapper hrp dp
  pure $ either (error . show) T.unpack eitherAddr
