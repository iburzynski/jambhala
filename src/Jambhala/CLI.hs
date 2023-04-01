{-# LANGUAGE FlexibleContexts #-}

module Jambhala.CLI ( ContractExports(..), Contracts, getSerialised, runJamb, scriptAddressBech32, writePlutusFile ) where

import Prelude hiding ( Applicative(..), Functor(..), Monoid(..), Semigroup(..), (<$>), elem, mconcat )
import Jambhala.CLI.Emulator
import Jambhala.CLI.Parsers
import Jambhala.CLI.Types
import Jambhala.CLI.Update ( updatePlutusApps )
import Jambhala.Haskell
import Jambhala.Plutus

import Codec.Serialise ( serialise, Serialise )
import Options.Applicative
import System.Environment ( lookupEnv )
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.ByteString.Short as BSS
import qualified Data.Map.Strict       as M
import qualified Data.Text             as Text

runJamb :: MonadIO m => Contracts -> m ()
runJamb = runReaderT (commandParser >>= liftIO . execParser >>= runCommand)

runCommand :: (MonadReader Contracts m, MonadIO m) => Command -> m ()
runCommand = \case
  List -> asks contractsPretty >>= liftIO . putStrLn . ("Available Contracts:\n\n" ++)
  Addr c n -> go c (liftIO . putStrLn . scriptAddressBech32 n . script)
  Hash c -> go c (hash . script)
  Test c -> go c $ maybe (liftIO . putStrLn $ "No test defined for " ++ c)
                         (liftIO . runJambEmulator)
                 . test
  Write c mFName -> go c (writePlutusFile (fromMaybe c mFName) . script)
  Update mRev -> updatePlutusApps mRev
  where
    hash = \case
      JambValidator v     -> liftIO . print $ validatorHash v
      JambMintingPolicy p -> liftIO . print $ scriptCurrencySymbol p
    go contract eff = asks (M.lookup contract) >>=
      maybe (liftIO . putStrLn $ "Error: contract \"" ++ contract ++ "\" not found") eff

scriptAddressBech32 :: Network -> JambScript -> String
scriptAddressBech32 network script =
    Text.unpack $
    serialiseToBech32 $
    ShelleyAddress
      network
      (ScriptHashObj $ toShelleyScriptHash $ scriptHash script)
      StakeRefNull
  where scriptHash s = hashScript $ PlutusScript PlutusScriptV2 $ getSerialised s

getSerialised :: Serialise a => a -> PlutusScript PlutusScriptV2
getSerialised = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise

writePlutusFile :: MonadIO m => FileName -> JambScript -> m ()
writePlutusFile fileName script = do
  mfp <- liftIO $ lookupEnv "PLUTUS_SCRIPTS_PATH"
  let fp = mconcat
         [ fromMaybe "assets/scripts/plutus" mfp
         , "/"
         , fileName
         , ".plutus"
         ]
  liftIO $ writeFileTextEnvelope fp Nothing (getSerialised script) >>= \case
    Left err -> liftIO . print $ displayError err
    Right () -> liftIO . putStrLn $ "wrote script to file " ++ fp