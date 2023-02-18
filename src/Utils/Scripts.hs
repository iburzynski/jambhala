module Utils.Scripts where

import Utils.Parsers

import Prelude hiding (error, decodeUtf8, Functor(..), Semigroup(..), Monoid(..), Applicative(..), Traversable(..), (<$>), elem, (/=), (<$), (<*), (*>), mconcat)
import GHC.Show (Show)
import Control.Applicative ( (<$>) )
import Data.Text (Text)
import Turtle (ExitCode(..))
import qualified Data.Text as T
import qualified Turtle as Sh
import Text.Megaparsec (runParser, errorBundlePretty)
import qualified Data.ByteString as BS
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (decodeUtf8)
import System.IO (IO, putStrLn)
import GHC.Err (error)
import Data.Traversable
import Data.Monoid

type Location = Text
type Revision = Text
data FlakeDependency = FlakeDependency {
    dependency :: !Dependency
  , sha256 :: !Text
  } deriving Show

cloneOrPullPlutusApps :: IO Revision
cloneOrPullPlutusApps = do
    plutusAppsDirExists <- Sh.testdir "plutus-apps"
    if plutusAppsDirExists
      then Sh.proc "git" ["pull"] Sh.empty >>= getRevOrDie
      else Sh.proc "git" ["clone", repo] Sh.empty >>= getRevOrDie
  where
    getRevOrDie exitCode = case exitCode of
      ExitSuccess -> do
        Sh.cd "plutus-apps"
        (_, commitHash) <- Sh.procStrict "git" ["rev-parse", "HEAD"] Sh.empty
        Sh.cd ".."
        return $ T.strip commitHash
      ExitFailure n -> Sh.die ("failed with exit code: " <> Sh.repr n)
    repo = "https://github.com/input-output-hk/plutus-apps.git"

nixPrefetchGit :: Text -> Text -> IO Text
nixPrefetchGit loc tag = do
  (_, npgOutput) <- Sh.procStrict "nix-prefetch-git" [loc, tag] Sh.empty
  return $ T.strip npgOutput

makeFlakeDependencies :: [Dependency] -> IO [FlakeDependency]
makeFlakeDependencies deps = do
  npgOuts <- T.strip . T.unlines <$> traverse (uncurry nixPrefetchGit) sources
  case runParser shasParser "" npgOuts of
    Left parseErr -> error $ errorBundlePretty parseErr
    Right shas -> return $ zipWith FlakeDependency deps shas
  where
    sources :: [(Location, Tag)]
    sources = map (\d -> (depLoc d, depTag d)) deps

formatFlakeDep :: FlakeDependency -> Text
formatFlakeDep (FlakeDependency (Dependency {..}) sha) = T.unlines [
    "source-repository-package"
  , "    type: " <> depType
  , "    location: " <> depLoc
  , "    tag: " <> depTag
  , "    --sha256: " <> sha
  , "    subdir:"
  ] <> depSubdirs

updatePlutusApps :: IO ()
updatePlutusApps = do
    putStrLn "Updating plutus-apps...\n"
    rev <- cloneOrPullPlutusApps
    cabalProjectContents <- decodeUtf8 <$> BS.readFile "plutus-apps/cabal.project"
    case runParser (projectParser rev) "" cabalProjectContents of
      Right projData -> do
        let ProjectData deps allOtherContent = projData
        fDeps <- T.strip . T.unlines . map formatFlakeDep <$> makeFlakeDependencies deps
        _ <- Sh.cp "plutus-apps/cabal.project" "cabal.project.backup"
        TIO.writeFile "cabal.project" (allOtherContent <> fDeps)
      Left parseErr -> putStrLn $ errorBundlePretty parseErr