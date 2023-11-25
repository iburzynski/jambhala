{-# LANGUAGE FlexibleContexts #-}

module Jambhala.CLI.Update (updatePlutusApps) where

import Control.Foldl qualified as Fold
import Control.Monad (filterM, unless)
import Control.Monad.Reader (MonadIO (..), MonadReader (..), ReaderT (..))
import Data.ByteString qualified as BS
import Data.List (break, isPrefixOf)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as TIO
import Data.Time (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Jambhala.CLI.Update.Parsers (
  CabalProjectData (..),
  Dependency (..),
  cabalProjectParser,
  prefetchGitParser,
 )
import Text.Megaparsec (errorBundlePretty, runParser)
import Turtle (ExitCode (..))
import Turtle qualified as Sh

type Revision = Text

updatePlutusApps :: (MonadIO m) => Maybe String -> m ()
updatePlutusApps mRev = do
  rev <- getCurrentPlutusAppsRev
  fp <- findPlutusApps
  rev' <- runReaderT (resetPlutusApps (T.pack <$> mRev)) fp
  unless (rev == rev') $ do
    liftIO $ putStrLn "Updating plutus-apps...\n"
    cProjConts <- T.decodeUtf8 <$> liftIO (BS.readFile $ fp ++ "/cabal.project")
    let CabalProjectData deps allOtherContent = runCProjParser rev' cProjConts
    fDeps <- liftIO $ makeFlakeDependencies deps
    (timestamp, _) <- break (== '.') . iso8601Show <$> liftIO getCurrentTime
    let backup = mconcat ["backups/cabal.project.", timestamp, ".backup"]
    Sh.cp "cabal.project" backup
    liftIO . putStrLn $ "cabal.project saved to " ++ backup
    liftIO $ TIO.writeFile "cabal.project" $ allOtherContent <> fDeps
    _ <- Sh.proc "direnv" ["reload"] Sh.empty
    _ <- Sh.proc "jbuild" [] Sh.empty
    void $ Sh.proc "jamb" ["-h"] Sh.empty
  where
    runCProjParser rv =
      either (error . errorBundlePretty) id
        . runParser (cabalProjectParser rv) ""

getCurrentPlutusAppsRev :: (MonadIO m) => m Revision
getCurrentPlutusAppsRev = do
  mRev <- fmap Sh.lineToText <$> Sh.fold (Sh.inshell cmd Sh.empty) Fold.head
  maybe (error "plutus-apps source-repository-package not found in cabal.project!") pure mRev
  where
    cmd =
      "grep -A 1 \"location: https://github.com/input-output-hk/plutus-apps.git\" cabal.project"
        <> " | tail -n 1 | awk '{ sub(/\\r?\\n/, \"\"); print $NF }'"

findPlutusApps :: (MonadIO m) => m FilePath
findPlutusApps =
  Sh.fold (Sh.ls src) Fold.list
    >>= filterM (\d -> Sh.testdir d <&> ((src ++ "plutus-ap_") `isPrefixOf` d &&))
    >>= ( \case
            [fp] -> pure fp
            _else -> err
        )
  where
    src = "dist-newstyle/src/"
    err = error "plutus-apps directory not found! Run `cabal build` and try again."

git :: (MonadIO m) => [Text] -> m ExitCode
git = flip (Sh.proc "git") Sh.empty

resetPlutusApps :: (MonadReader FilePath m, MonadIO m) => Maybe Revision -> m Revision
resetPlutusApps mRev = do
  fp <- ask
  _ <- Sh.cd fp
  exitCode <- git ["pull"]
  runOrDie revParse exitCode
  where
    revParse = do
      _ <- maybe (pure ()) gitReset mRev
      (_, commitHash) <- Sh.procStrict "git" ["rev-parse", "HEAD"] Sh.empty
      _ <- cdRoot
      pure $ T.strip commitHash

gitReset :: (MonadReader FilePath m, MonadIO m) => Revision -> m ()
gitReset rev = do
  exitCode <- resetTo rev
  case exitCode of
    ExitSuccess -> pure ()
    -- if invalid rev, reset head back to current plutus-apps revision in cabal.project
    ExitFailure _ -> do
      fp <- ask
      _ <- cdRoot
      currentRev <- getCurrentPlutusAppsRev
      _ <- Sh.cd fp
      exitCode' <- resetTo currentRev
      runOrDie (pure ()) exitCode'
  where
    resetTo r = git ["reset", "--hard", r]

runOrDie :: (MonadIO m) => m a -> ExitCode -> m a
runOrDie onSuccess = \case
  ExitSuccess -> onSuccess
  ExitFailure n -> cdRoot >> Sh.die ("Failed with exit code: " <> Sh.repr n)

cdRoot :: (MonadIO m) => m ()
cdRoot = Sh.cd "../../.."

makeFlakeDependencies :: (MonadIO m) => [Dependency] -> m Text
makeFlakeDependencies deps =
  formatFlakeDeps <$> do
    let sourceArgs = ([depLoc, depTag] <*>) . pure <$> deps
    npgOuts <- liftIO $ stripUnlines <$> traverse nixPrefetchGit sourceArgs
    case runParser prefetchGitParser "" npgOuts of
      Left parseErr -> error $ errorBundlePretty parseErr
      Right shas -> pure $ zip deps shas
  where
    formatFlakeDeps =
      stripUnlines
        . map
          ( \(Dependency {..}, sha) ->
              T.unlines
                [ "source-repository-package"
                , "    type: " <> depType
                , "    location: " <> depLoc
                , "    tag: " <> depTag
                , "    --sha256: " <> sha
                ]
                <> maybe "" ("    subdir:\n" <>) depSubdirs
          )
    nixPrefetchGit = fmap (T.strip . snd) . flip (Sh.procStrict "nix-prefetch-git") Sh.empty
    stripUnlines = T.strip . T.unlines
