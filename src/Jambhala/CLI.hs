{-# LANGUAGE FlexibleContexts #-}

module Jambhala.CLI ( ContractExports(..), Contracts, runJamb, writePlutusFile ) where

import Prelude hiding ( Applicative(..), Functor(..), Monoid(..), Semigroup(..), (<$>), elem, mconcat )
import Jambhala.CLI.Emulator
import Jambhala.CLI.Parsers
import Jambhala.CLI.Types
import Jambhala.CLI.Update ( updatePlutusApps )
import Jambhala.Haskell
import Jambhala.Plutus

import Codec.Serialise ( serialise )
import Options.Applicative
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.ByteString.Short as BSS
import qualified Data.Map.Strict       as M

runJamb :: MonadIO m => Contracts -> m ()
runJamb = runReaderT (commandParser >>= liftIO . execParser >>= runCommand)

runCommand :: (MonadReader Contracts m, MonadIO m) => Command -> m ()
runCommand = \case
  List -> asks contractsPretty >>= liftIO . putStrLn . ("Available Contracts:\n\n" ++)
  Hash c -> go c (liftIO . print . validatorHash . validator)
  Test c -> go c $ maybe (liftIO . putStrLn $ "No test defined for " ++ c)
                         (liftIO . runJambEmulator)
                 . test
  Write c mFName -> go c (writePlutusFile (fromMaybe c mFName) . validator)
  Update mRev -> updatePlutusApps mRev
  where
    go contract eff = asks (M.lookup contract) >>=
      maybe (liftIO . putStrLn $ "Error: contract \"" ++ contract ++ "\" not found") eff

writePlutusFile :: MonadIO m => FileName -> Validator -> m ()
writePlutusFile fileName validator =
  liftIO $ writeFileTextEnvelope fp Nothing (getSerialised validator) >>= \case
    Left err -> liftIO . print $ displayError err
    Right () -> liftIO . putStrLn $ "wrote validator to file " ++ fp
  where
    fp = "compiled/" ++ fileName ++ ".plutus"
    getSerialised :: Validator -> PlutusScript PlutusScriptV2
    getSerialised = PlutusScriptSerialised . BSS.toShort . BSL.toStrict . serialise