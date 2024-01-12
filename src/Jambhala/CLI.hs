{-# LANGUAGE FlexibleContexts #-}

module Jambhala.CLI (runJamb, scriptAddressBech32) where

import Cardano.Api (Script (..), prettyPrintJSON, writeFileJSON)
import Cardano.Api.Shelley (scriptDataToJsonDetailedSchema)
import Control.Monad.Reader (MonadIO (..), MonadReader, ReaderT (..), asks)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.Char (toUpper)
import Data.Map.Strict qualified as M
import Data.Text qualified as Text
import Jambhala.CLI.Bech32 (bech32)
import Jambhala.CLI.Emulator
import Jambhala.CLI.Export (getSerialised)
import Jambhala.CLI.Parsers
import Jambhala.CLI.Types
import Jambhala.CLI.Update (updatePlutusApps)
import Jambhala.Plutus
import Options.Applicative
import System.Directory.Extra (doesDirectoryExist, doesFileExist)
import System.Environment (lookupEnv)
import System.IO (BufferMode (..), getChar, hSetBuffering, putStr, stdin, stdout, writeFile)

runJamb :: (MonadIO m) => JambContracts -> m ()
runJamb = runReaderT (commandParser >>= liftIO . execParser >>= runCommand)

runCommand :: (MonadReader JambContracts m, MonadIO m) => Command -> m ()
runCommand = \case
  List -> asks contractsPretty >>= liftIO . putStrLn . ("Available Contracts:\n\n" ++)
  Addr c n -> go c (liftIO . putStrLn . scriptAddressBech32 n . script)
  Bech32 h kh -> bech32 h kh >>= liftIO . putStrLn
  Hash c -> go c (hash . script)
  Test c ->
    go c (liftIO . runJambEmulator . test)
  Write c mfn n -> go c (writeScriptWithData (fromMaybe c mfn) n)
  Update mRev -> updatePlutusApps mRev
  where
    hash = \case
      JambValidator v -> liftIO . print $ validatorHash v
      JambMintingPolicy p -> liftIO . print $ mintingPolicyHash p
    go contract eff =
      asks (M.lookup contract)
        >>= maybe (liftIO . putStrLn $ "Error: contract \"" ++ contract ++ "\" not found") eff

scriptAddressBech32 :: Network -> ScriptExport -> String
scriptAddressBech32 network script =
  Text.unpack $
    serialiseToBech32 $
      ShelleyAddress
        network
        (ScriptHashObj $ toShelleyScriptHash $ scriptHash script)
        StakeRefNull
  where
    scriptHash s = hashScript $ PlutusScript PlutusScriptV2 $ getSerialised s

writeScriptWithData :: (MonadIO m) => FileName -> Network -> ContractExports -> m ()
writeScriptWithData fn n (ContractExports s ds _) = do
  _ <- liftIO $ writeScriptToFile fn n s
  liftIO $ traverse_ writeDataToFile ds

writeScriptToFile :: (MonadIO m) => FileName -> Network -> ScriptExport -> m ()
writeScriptToFile fn network script = do
  fp <- liftIO $ mkFilePath "PLUTUS_SCRIPTS_PATH" "cardano-cli-guru/assets/scripts/plutus" fn ".plutus"
  fileOverwritePrompt fp $
    liftIO (writeFileTextEnvelope fp Nothing (getSerialised script)) >>= \case
      Left err -> liftIO . print $ displayError err
      Right () -> do
        liftIO . putStrLn $ "Wrote script to '" ++ fp ++ "'"
        writeScriptAddressToFile fn network script

type EnvVar = String

type DefDir = String

type FileExt = String

mkFilePath :: (MonadIO m) => EnvVar -> DefDir -> FileName -> FileExt -> m FilePath
mkFilePath envVar defDir fn ext = do
  mDir <- liftIO $ lookupEnv envVar
  case mDir >>= stripTrailingSlash of
    Nothing -> do
      liftIO . putStrLn $ mconcat [envVar, " not set. Using default directory '", defDir, "'."]
      mkDefFilePath
    Just dir -> do
      envDirExists <- liftIO $ doesDirectoryExist dir
      if envDirExists
        then pure $ mkFilePath' dir
        else do
          liftIO . putStrLn $ mconcat ["'", dir, "' does not exist. Using default directory '", defDir, "'."]
          mkDefFilePath
  where
    stripTrailingSlash :: String -> Maybe String
    stripTrailingSlash [] = Nothing
    stripTrailingSlash fp = if last fp == '/' then Just (init fp) else Just fp

    mkFilePath' :: String -> String
    mkFilePath' dir =
      concat
        [ dir
        , "/"
        , fn
        , ext
        ]

    mkDefFilePath :: (MonadIO m) => m FilePath
    mkDefFilePath = do
      defDirExists <- liftIO $ doesDirectoryExist defDir
      if defDirExists
        then pure $ mkFilePath' defDir
        else error $ concat ["Error: default directory '", defDir, "' does not exist."]

fileOverwritePrompt :: (MonadIO m) => FileName -> m () -> m ()
fileOverwritePrompt fn writeAction = do
  liftIO $ hSetBuffering stdin NoBuffering
  liftIO $ hSetBuffering stdout NoBuffering
  fileExists <- liftIO $ doesFileExist fn
  if fileExists
    then do
      liftIO $ putStr $ "File '" ++ fn ++ "' already exists. Do you want to overwrite it? (Y/N): "
      response <- liftIO getChar
      liftIO $ putStrLn ""
      case toUpper response of
        'Y' -> writeAction
        'N' -> pure ()
        _ -> liftIO (putStrLn "Invalid response. Please enter Y or N.") >> fileOverwritePrompt fn writeAction
    else writeAction

writeDataToFile :: (MonadIO m) => DataExport -> m ()
writeDataToFile (DataExport fn d) = do
  fp <- mkFilePath "DATA_PATH" "cardano-cli-guru/assets/data" fn ".json"
  let v = dataToJSON d
  fileOverwritePrompt fp $
    liftIO (writeFileJSON fp v) >>= \case
      Left err -> liftIO . print $ displayError err
      Right () -> liftIO . printf "Wrote data to '%s':\n\n%s\n\n" fp . BS8.unpack $ prettyPrintJSON v
  where
    dataToJSON :: (ToData a) => a -> Aeson.Value
    dataToJSON = scriptDataToJsonDetailedSchema . fromPlutusData . toData

writeScriptAddressToFile :: (MonadIO m) => FileName -> Network -> ScriptExport -> m ()
writeScriptAddressToFile fn n script = do
  fp <- liftIO $ mkFilePath "ADDR_PATH" "cardano-cli-guru/assets/addr/" fn ".addr"
  let addr = scriptAddressBech32 n script
  fileOverwritePrompt fp $ do
    liftIO $ writeFile fp addr
    liftIO . putStrLn $
      concat
        [ "Wrote address '"
        , addr
        , "' to '"
        , fp
        , "'"
        ]
