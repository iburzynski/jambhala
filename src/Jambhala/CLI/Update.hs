module Jambhala.CLI.Update ( updatePlutusApps, getPlutusAppsRev ) where

import Jambhala.CLI.Parsers ( CabalProjectData(..), Dependency(..), cabalProjectParser, prefetchGitParser )

import Prelude hiding (
    Applicative(..), Eq(..), Functor(..), Monoid(..), Semigroup(..), Traversable(..)
  , (<$>), decodeUtf8, elem, error, mconcat, unless )

import Control.Applicative ( Applicative(..) )
import Control.Monad ( filterM, unless, void )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Eq ( Eq(..) )
import Data.Functor ( Functor(..), (<$>), (<&>) )
import Data.List ( break, isPrefixOf )
import Data.Monoid ( Monoid(..) )
import Data.Semigroup ( Semigroup(..) )
import Data.Text ( Text )
import Data.Text.Encoding ( decodeUtf8 )
import Data.Time ( getCurrentTime )
import Data.Time.Format.ISO8601 ( iso8601Show )
import Data.Traversable ( Traversable(..) )
import GHC.Err ( error )
import System.IO ( FilePath, putStrLn )
import Text.Megaparsec ( runParser, errorBundlePretty )
import Turtle ( ExitCode(..) )
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Turtle as Sh
import qualified Control.Foldl as Fold

type Revision = Text

updatePlutusApps :: MonadIO m => m ()
updatePlutusApps = do
  rev  <- getPlutusAppsRev
  fp   <- findPlutusApps
  rev' <- pullPlutusApps fp
  unless (rev == rev') $ do
    liftIO $ putStrLn "Updating plutus-apps...\n"
    cProjConts <- decodeUtf8 <$> liftIO (BS.readFile $ fp ++ "/cabal.project")
    let CabalProjectData deps allOtherContent = runCProjParser rev' cProjConts
    fDeps <- liftIO $ makeFlakeDependencies deps
    (timestamp, _) <- break (== '.') . iso8601Show <$> liftIO getCurrentTime
    let backup = mconcat ["backups/cabal.project.", timestamp, ".backup"]
    Sh.cp "cabal.project" backup
    liftIO . putStrLn $ "cabal.project saved to " ++ backup
    liftIO $ TIO.writeFile "cabal.project" $ allOtherContent <> fDeps
    void $ Sh.proc "direnv" ["allow"] Sh.empty
    liftIO $ putStrLn "Update complete!"
    where
      runCProjParser rv = either (error . errorBundlePretty) id
                         . runParser (cabalProjectParser rv) ""

getPlutusAppsRev :: MonadIO m => m Revision
getPlutusAppsRev = do
  mRev <- fmap Sh.lineToText <$> Sh.fold (Sh.inshell cmd Sh.empty) Fold.head
  maybe (error "plutus-apps source-repository-package not found in cabal.project!") pure mRev
  where
    cmd = "grep -A 1 \"location: https://github.com/input-output-hk/plutus-apps.git\" cabal.project"
       <> " | tail -n 1 | awk '{ sub(/\\r?\\n/, \"\"); print $NF }'"

findPlutusApps :: MonadIO m => m FilePath
findPlutusApps = Sh.fold (Sh.ls src) Fold.list
               >>= filterM (\d -> Sh.testdir d <&> ((src ++ "plutus-ap_") `isPrefixOf` d &&))
               >>= (\case [fp]  -> pure fp
                          _else -> err)
  where
    src = "dist-newstyle/src/"
    err = error "plutus-apps directory not found! Run `cabal build` and try again."

pullPlutusApps :: MonadIO m => FilePath -> m Revision
pullPlutusApps fp = Sh.cd fp >> git ["pull"] >>= runOrDie revParse
  where
    git = flip (Sh.proc "git") Sh.empty
    runOrDie onSuccess code = case code of
      ExitSuccess -> onSuccess
      ExitFailure n -> Sh.cd "../../.." >> Sh.die ("Failed with exit code: " <> Sh.repr n)
    revParse = do
      (_, commitHash) <- Sh.procStrict "git" ["rev-parse", "HEAD"] Sh.empty
      Sh.cd "../../.."
      pure $ T.strip commitHash

makeFlakeDependencies :: MonadIO m => [Dependency] -> m Text
makeFlakeDependencies deps = formatFlakeDeps <$> do
  let sourceArgs = ([depLoc, depTag] <*>) . pure <$> deps
  npgOuts <- liftIO $ stripUnlines <$> traverse nixPrefetchGit sourceArgs
  case runParser prefetchGitParser "" npgOuts of
    Left parseErr -> error $ errorBundlePretty parseErr
    Right shas    -> pure $ zip deps shas
  where
    formatFlakeDeps = stripUnlines . map (\(Dependency {..}, sha) ->
      T.unlines
      [ "source-repository-package"
      , "    type: " <> depType
      , "    location: " <> depLoc
      , "    tag: " <> depTag
      , "    --sha256: " <> sha
      , "    subdir:"
      ] <> depSubdirs)
    nixPrefetchGit = fmap (T.strip . snd) . flip (Sh.procStrict "nix-prefetch-git") Sh.empty
    stripUnlines = T.strip . T.unlines