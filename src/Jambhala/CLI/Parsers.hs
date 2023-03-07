{-# LANGUAGE FlexibleContexts #-}

module Jambhala.CLI.Parsers ( commandParser, contractsPretty ) where

import Prelude hiding
  ( Applicative(..), Functor(..), Monoid(..), Semigroup(..), (<$>), elem, mconcat )
import Jambhala.CLI.Types
import Jambhala.Haskell
import Options.Applicative
import qualified Data.Map.Strict as M

commandParser :: MonadReader Contracts m => m (ParserInfo Command)
commandParser = do
  pw <- parseWrite
  ph <- parseHash
  pt <- parseTest
  let p = parseList <|> ph <|> pt <|> pw <|> parseUpdate
  pure . info (helper <*> p) $ mconcat [fullDesc, progDesc "Jambhala Plutus Development Suite"]

parseList :: Parser Command
parseList = flag' List (long "list" <> short 'l' <> help "List the available contracts")

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
  , metavar "CONTRACT"
  , help "Write CONTRACT to file with optional FILENAME (default is CONTRACT.plutus)" ]
  where parseFName = optional $ argument str (metavar "FILENAME")

parseUpdate :: Parser Command
parseUpdate = flag' Update (mconcat [
     long "update"
   , short 'u'
   , help "Update plutus-apps to optional TAG or COMMIT (default is latest commit)"
   ]) <*> optional (argument str $ metavar "TAG/COMMIT")

parseContractName :: MonadReader Contracts m => Mod OptionFields String -> m (Parser String)
parseContractName fields = do
  contracts <- ask
  let readMName = eitherReader $ \c -> if c `M.member` contracts then Right c else Left $
        "Error: contract not found - choose one of the contracts below:\n\n"
        ++ contractsPretty contracts
  pure $ option readMName fields

contractsPretty :: Contracts -> String
contractsPretty = unlines . map ('\t':) . M.keys