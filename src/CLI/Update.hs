module CLI.Update ( updatePlutusApps ) where

import CLI.Parsers ( CabalProjectData(..), Dependency(..), cabalProjectParser, prefetchGitParser )

import Prelude hiding (
    Applicative(..), Eq(..), Functor(..), Monoid(..), Semigroup(..), Traversable(..)
  , (<$>), decodeUtf8, elem, error, mconcat )

import Control.Applicative ( Applicative(..) )
import Control.Monad ( filterM )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Eq ( Eq(..) )
import Data.Functor ( Functor(..), (<$>), (<&>) )
import Data.Semigroup ( Semigroup(..) )
import Data.Text ( Text )
import Data.Text.Encoding ( decodeUtf8 )
import Data.Time ( getCurrentTime )
import Data.Time.Format.ISO8601 ( iso8601Show )
import Data.Traversable ( Traversable(..) )
import GHC.Err ( error )
import System.IO ( putStrLn, FilePath )
import Text.Megaparsec ( runParser, errorBundlePretty )
import Turtle ( ExitCode(..) )
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Turtle as Sh
import qualified Control.Foldl as Fold

import Data.List ( break, isPrefixOf )

type Revision = Text

updatePlutusApps :: MonadIO m => m ()
updatePlutusApps = findPlutusApps >>= \case
      Nothing -> Sh.die "Error: plutus-apps directory not found!"
      Just fp -> do
        liftIO $ putStrLn "Updating plutus-apps...\n"
        rev <- pullPlutusApps fp
        cabalProjectContents <- liftIO $ decodeUtf8 <$> BS.readFile (fp ++ "/cabal.project")
        case runParser (cabalProjectParser rev) "" cabalProjectContents of
          Right projData -> do
            let CabalProjectData deps allOtherContent = projData
            fDeps <- liftIO $ makeFlakeDependencies deps
            (timestamp, _) <- liftIO $ break (== '.') . iso8601Show <$> getCurrentTime
            _ <- Sh.cp "cabal.project" $ "backups/cabal.project." ++ timestamp ++ ".backup"
            liftIO $ TIO.writeFile "cabal.project" (allOtherContent <> fDeps)
            allow <- Sh.proc "direnv" ["allow"] Sh.empty
            case allow of
              ExitSuccess -> liftIO $ putStrLn "Update complete!"
              _fail       -> pure ()
          Left parseErr -> liftIO . putStrLn $ errorBundlePretty parseErr

findPlutusApps :: MonadIO m => m (Maybe FilePath)
findPlutusApps = Sh.fold (Sh.ls src) Fold.list
               >>= filterM (\d -> Sh.testdir d <&> ((src ++ "plutus-ap_") `isPrefixOf` d &&))
               >>= (\case [fp] -> pure $ Just fp; _else -> pure Nothing)
  where src = "dist-newstyle/src/"

pullPlutusApps :: MonadIO m => FilePath -> m Revision
pullPlutusApps fp = Sh.cd fp >> git ["pull"] >>= runOrDie revParse
  where
    git = flip (Sh.proc "git") Sh.empty
    runOrDie onSuccess code = case code of
      ExitSuccess -> onSuccess
      ExitFailure n -> Sh.cd "../../.." >> Sh.die ("failed with exit code: " <> Sh.repr n)
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