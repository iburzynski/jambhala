{-# LANGUAGE FlexibleContexts #-}

module Jambhala.CLI ( runJamb ) where

import Prelude hiding ( Applicative(..), Functor(..), Monoid(..), Semigroup(..), elem, mconcat )
import Jambhala.CLI.Update ( updatePlutusApps )
import Jambhala.Haskell
import Jambhala.Plutus
import Jambhala.Utils ( ContractExports(..), Contracts, writePlutusFile )
import Options.Applicative
import qualified Data.Map.Strict as M

type ContractName = String
type FileName = String

data Command = List
             | Hash !ContractName
             | Test !ContractName
             | Write !ContractName !(Maybe FileName)
             | Update

runJamb :: MonadIO m => Contracts -> m ()
runJamb = runReaderT (commandParser >>= liftIO . execParser >>= runCommand)

runCommand :: (MonadReader Contracts m, MonadIO m) => Command -> m ()
runCommand = \case
  List -> asks contractsPretty >>= liftIO . putStrLn . ("Available Contracts:\n\n" ++)
  Hash c -> go c (liftIO . print . validatorHash . getValidator)
  Test c -> go c $ maybe (liftIO . putStrLn $ "No test defined for " ++ c)
                         (liftIO . runEmulatorTraceIO)
                 . getTest
  Write c mFName -> go c (writePlutusFile (fromMaybe c mFName) . getValidator)
  Update -> updatePlutusApps
  where
    go contract eff = asks (M.lookup contract) >>=
      maybe (liftIO . putStrLn $ "Error: contract \"" ++ contract ++ "\" not found") eff

commandParser :: MonadReader Contracts m => m (ParserInfo Command)
commandParser = do
  pw <- parseWrite
  ph <- parseHash
  pt <- parseTest
  let p = parseList <|> ph <|> pt <|> pw <|> parseUpdate
  pure . info (helper <*> p) $ mconcat [fullDesc, progDesc "Create sample smart contracts"]

parseList, parseUpdate :: Parser Command
parseList = flag' List (long "list" <> short 'l' <> help "List the available contracts")
parseUpdate = flag' Update (long "update" <> short 'u' <> help "Update plutus-apps")

parseHash :: MonadReader Contracts m => m (Parser Command)
parseHash = fmap (fmap Hash) . parseContractName $ mconcat [
    long "hash"
  , short 's'
  , metavar "CONTRACT"
  , help "Hash validator for CONTRACT" ]

parseTest :: MonadReader Contracts m => m (Parser Command)
parseTest = fmap (fmap Test) . parseContractName $ mconcat [
    long "test"
  , short 't'
  , metavar "CONTRACT"
  , help "Test CONTRACT with emulator" ]

parseWrite :: MonadReader Contracts m => m (Parser Command)
parseWrite = fmap ((<*> parseFName) . fmap Write) . parseContractName $ mconcat [
    long "write"
  , short 'w'
  , metavar "CONTRACT [FILENAME]"
  , help "Write CONTRACT to file with optional FILENAME (default is CONTRACT.plutus)" ]
  where parseFName = optional (argument str mempty)

parseContractName :: MonadReader Contracts m => Mod OptionFields String -> m (Parser String)
parseContractName fields = do
  contracts <- ask
  let readMName = eitherReader $ \c -> if c `M.member` contracts then Right c else Left $
        "Error: contract not found - choose one of the contracts below:\n\n"
        ++ contractsPretty contracts
  pure $ option readMName fields

contractsPretty :: Contracts -> String
contractsPretty = unlines . map ('\t':) . M.keys