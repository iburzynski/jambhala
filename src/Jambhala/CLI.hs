{-# LANGUAGE FlexibleContexts #-}

module Jambhala.CLI (runJamb, scriptAddressBech32) where

import Cardano.Api (Script (..), prettyPrintJSON, writeFileJSON)
import Cardano.Api.Shelley (scriptDataToJsonDetailedSchema)
import Control.Monad.Reader (MonadIO (..), MonadReader, ReaderT (..), asks)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict qualified as M
import Data.Text qualified as Text
import Jambhala.CLI.Emulator
import Jambhala.CLI.Export (getSerialised)
import Jambhala.CLI.Parsers
import Jambhala.CLI.Types
import Jambhala.CLI.Update (updatePlutusApps)
import Jambhala.Plutus
import Options.Applicative
import System.Environment (lookupEnv)

runJamb :: MonadIO m => JambContracts -> m ()
runJamb = runReaderT (commandParser >>= liftIO . execParser >>= runCommand)

runCommand :: (MonadReader JambContracts m, MonadIO m) => Command -> m ()
runCommand = \case
  List -> asks contractsPretty >>= liftIO . putStrLn . ("Available Contracts:\n\n" ++)
  Addr c n -> go c (liftIO . putStrLn . scriptAddressBech32 n . script)
  Hash c -> go c (hash . script)
  Test c ->
    go c (liftIO . runJambEmulator . test)
  Write c mfn -> go c (writeScriptWithData $ fromMaybe c mfn)
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

writeScriptWithData :: MonadIO m => FileName -> ContractExports -> m ()
writeScriptWithData fn (ContractExports s ds _) = do
  _ <- liftIO $ writeScriptToFile fn s
  liftIO $ traverse_ writeDataToFile ds

writeScriptToFile :: MonadIO m => FileName -> ScriptExport -> m ()
writeScriptToFile fileName script = do
  mfp <- liftIO $ lookupEnv "PLUTUS_SCRIPTS_PATH"
  let fp =
        mconcat
          [ fromMaybe "cardano-cli-guru/assets/scripts/plutus" mfp,
            "/",
            fileName,
            ".plutus"
          ]
  liftIO $
    writeFileTextEnvelope fp Nothing (getSerialised script) >>= \case
      Left err -> liftIO . print $ displayError err
      Right () -> liftIO . putStrLn $ "wrote script to file '" ++ fp ++ "'"

writeDataToFile :: DataExport -> IO ()
writeDataToFile (DataExport fn d) = do
  mfp <- liftIO $ lookupEnv "DATA_PATH"
  let fp =
        mconcat
          [ fromMaybe "cardano-cli-guru/assets/data/" mfp,
            "/",
            fn,
            ".json"
          ]
      v = dataToJSON d
  writeFileJSON fp v >>= \case
    Left err -> print $ displayError err
    Right () -> printf "wrote data to file '%s':\n\n%s\n\n" fp $ BS8.unpack $ prettyPrintJSON v

dataToJSON :: ToData a => a -> Aeson.Value
dataToJSON = scriptDataToJsonDetailedSchema . fromPlutusData . toData