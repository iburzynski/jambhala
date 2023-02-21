{-# LANGUAGE FlexibleContexts #-}

module CLI ( runJamb ) where

import CLI.Update ( updatePlutusApps )
import Utils ( writePlutusFile )

import Prelude hiding ( Functor(..), Semigroup(..), Monoid(..), Applicative(..), elem, mconcat )

import Control.Monad.Reader ( MonadReader, MonadIO(..), ReaderT (..), ask, asks )
import Data.Functor ( Functor(fmap) )
import Data.List ( unlines )
import Data.Map.Strict (Map)
import Data.Monoid ( Monoid(..), (<>) )
import Data.String ( String )
import Options.Applicative
import Plutus.Script.Utils.V2.Scripts ( validatorHash )
import Plutus.V2.Ledger.Api ( Validator )
import System.IO ( putStrLn, print )
import qualified Data.Map.Strict as M

type ContractName = String
type FileName = String
type Contracts = Map String Validator

data Command = List
             | Write !ContractName !(Maybe FileName)
             | Hash !ContractName
             | Update

runJamb :: MonadIO m => Contracts -> m ()
runJamb = runReaderT (commandParser >>= liftIO . execParser >>= runCommand)

runCommand :: (MonadReader Contracts m, MonadIO m) => Command -> m ()
runCommand = \case
  List -> asks contractsPretty >>= liftIO . putStrLn . ("Available Contracts:\n\n" ++)
  Write c mFName -> go c (writePlutusFile $ fromMaybe c mFName)
  Hash c -> go c (liftIO . print . validatorHash)
  Update -> updatePlutusApps
  where
    go contract eff = asks (M.lookup contract) >>=
      maybe (liftIO . putStrLn $ "Error: contract \"" ++ contract ++ "\" not found") eff

commandParser :: MonadReader Contracts m => m (ParserInfo Command)
commandParser = do
  pw <- parseWrite
  ph <- parseHash
  let p = parseList <|> pw <|> ph <|> parseUpdate
  pure . info (helper <*> p) $ mconcat [fullDesc, progDesc "Create sample smart contracts"]

parseList, parseUpdate :: Parser Command
parseList = flag' List (long "list" <> short 'l' <> help "List the available contracts")
parseUpdate = flag' Update (long "update" <> short 'u' <> help "Update plutus-apps")

parseWrite :: MonadReader Contracts m => m (Parser Command)
parseWrite = fmap ((<*> parseFName) . fmap Write) . parseContractName $ mconcat [
        long "write"
      , short 'w'
      , metavar "CONTRACT [FILENAME]"
      , help "Write CONTRACT to file with optional FILENAME (default is CONTRACT.plutus)"
      ]
  where parseFName = optional (argument str mempty)

parseHash :: MonadReader Contracts m => m (Parser Command)
parseHash = fmap (fmap Hash) . parseContractName $ mconcat [
        long "hash"
      , short 's'
      , metavar "CONTRACT"
      , help "Hash validator for CONTRACT"
      ]

parseContractName :: MonadReader Contracts m => Mod OptionFields String -> m (Parser String)
parseContractName fields = do
  contracts <- ask
  let readMName = eitherReader $ \c -> if c `M.member` contracts then Right c else Left $
        "Error: contract not found - choose one of the contracts below:\n\n"
        ++ contractsPretty contracts
  pure $ option readMName fields

contractsPretty :: Contracts -> String
contractsPretty = unlines . map ('\t':) . M.keys