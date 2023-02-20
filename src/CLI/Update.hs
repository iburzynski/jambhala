module CLI.Update ( updatePlutusApps ) where

import CLI.Parsers ( CabalProjectData(..), Dependency(..), cabalProjectParser, prefetchGitParser )

import Prelude hiding (
    Applicative(..), Eq(..), Functor(..), Monoid(..), Semigroup(..), Traversable(..)
  , (<$>), decodeUtf8, elem, error, mconcat )

import Turtle ( ExitCode(..), (<=<) )
import Control.Applicative ( Applicative(..) )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Eq ( Eq(..) )
import Data.Functor ( Functor(..), (<$>) )
import Data.List ( break )
import Data.Semigroup ( Semigroup(..) )
import Data.Text ( Text )
import Data.Text.Encoding ( decodeUtf8 )
import Data.Time ( getCurrentTime )
import Data.Time.Format.ISO8601 ( iso8601Show )
import Data.Traversable ( Traversable(..) )
import GHC.Err ( error )
import System.IO ( putStrLn )
import Text.Megaparsec ( runParser, errorBundlePretty )
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Turtle as Sh

type Revision = Text

updatePlutusApps :: MonadIO m => m ()
updatePlutusApps = do
    liftIO $ putStrLn "Updating plutus-apps...\n"
    rev <- cloneOrPullPlutusApps
    cabalProjectContents <- liftIO $ decodeUtf8 <$> BS.readFile "plutus-apps/cabal.project"
    case runParser (cabalProjectParser rev) "" cabalProjectContents of
      Right projData -> do
        let CabalProjectData deps allOtherContent = projData
        fDeps <- liftIO $ makeFlakeDependencies deps
        (timestamp, _) <- liftIO $ break (== '.') . iso8601Show <$> getCurrentTime
        _ <- Sh.cp "cabal.project" $ "backups/cabal.project." ++ timestamp ++ ".backup"
        liftIO $ TIO.writeFile "cabal.project" (allOtherContent <> fDeps)
      Left parseErr -> liftIO . putStrLn $ errorBundlePretty parseErr

cloneOrPullPlutusApps :: MonadIO m => m Revision
cloneOrPullPlutusApps = Sh.testdir "plutus-apps" >>= \case
    False -> gitCmd ["clone", "https://github.com/input-output-hk/plutus-apps.git"]
    _else -> gitCmd ["pull"]
  where
    gitCmd = getRevOrDie <=< flip (Sh.proc "git") Sh.empty
    getRevOrDie exitCode = case exitCode of
      ExitSuccess -> do
        Sh.cd "plutus-apps"
        (_, commitHash) <- Sh.procStrict "git" ["rev-parse", "HEAD"] Sh.empty
        Sh.cd ".."
        pure $ T.strip commitHash
      ExitFailure n -> Sh.die ("failed with exit code: " <> Sh.repr n)

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