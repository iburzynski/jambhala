module Utils.CLI ( commandParser, runCommand ) where

import Contracts.Contracts ( contracts )
import Utils.Scripts ( updatePlutusApps )

import Prelude hiding (Functor, Semigroup, Monoid, Applicative, elem, (<$>), (<*>), (<>), mconcat)
import Data.String ( String )
import Plutus.Script.Utils.V2.Scripts ( validatorHash )
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Monoid (mconcat)
import Data.List (elem, unlines)
import qualified Data.Map.Strict as M
import System.IO (IO, putStrLn, print)
import Utils.Utils (writePlutusFile)

type ContractName = String
type FileName = String

data Command = List
             | Write !ContractName !(Maybe FileName)
             | Hash !ContractName
             | Update

contractNames :: [String]
contractNames = M.keys contracts

contractsPretty :: String
contractsPretty = unlines $ map ('\t':) contractNames

commandParser :: ParserInfo Command
commandParser = info (helper <*> parseCommand) . mconcat $ [fullDesc, progDesc "Create sample smart contracts"]
  where parseCommand = parseList <|> parseWrite <|> parseHash <|> parseUpdate

parseList :: Parser Command
parseList = flag' List (long "list" <> short 'l' <> help "List the available contracts")

parseUpdate :: Parser Command
parseUpdate = flag' Update (long "update" <> short 'u' <> help "Update plutus-apps")

parseWrite :: Parser Command
parseWrite = Write <$> option contractReader
  ( long "write"
  <> short 'w'
  <> metavar "CONTRACT"
  <> help "Write CONTRACT to file" )
  <*> optional (strOption
  (  metavar "FILENAME"
  <> help "Optional FILENAME" ))

parseHash :: Parser Command
parseHash = Hash <$> option contractReader
  ( long "hash"
  <> short 's'
  <> metavar "CONTRACT"
  <> help "Hash validator for CONTRACT" )

contractReader :: ReadM String
contractReader = eitherReader $ \c ->
  if c `elem` contractNames
    then Right c
    else Left err
  where
    err = "Error: contract not found - choose one of the contracts below:\n\n" ++ contractsPretty

runCommand :: Command -> IO ()
runCommand cmd  = case cmd of
  List               -> putStrLn $ "Available Contracts:\n\n" ++ contractsPretty
  Write c maybeFName -> go c (writePlutusFile $ fromMaybe c maybeFName)
  Hash c             -> go c (print . validatorHash)
  Update             -> updatePlutusApps
  where
    go c act =
      maybe (putStrLn $ "Error: contract \"" ++ c ++ "\" not found") act $ M.lookup c contracts