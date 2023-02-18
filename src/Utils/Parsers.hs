module Utils.Parsers where


import Prelude hiding (fmap, Functor, Semigroup(..), Monoid(..), Applicative(..), (<$>), elem, (/=), (<$), (<*), (*>), mconcat)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (skipLineComment, charLiteral)
import Data.Text (Text)
import Data.Void (Void)
import qualified Data.Text as T
import Control.Monad
import GHC.Show (Show)
import Data.String
import Control.Applicative hiding (many, some)
import Data.Monoid ( (<>), Monoid(mconcat) )
import Data.Eq

type Tag = Text
type Parser = Parsec Void Text
data Dependency = Dependency {
    depType :: !Text
  , depLoc :: !Text
  , depTag :: !Text
  , depSubdirs :: !Text
  } deriving Show

data ProjectData = ProjectData ![Dependency] !Text deriving Show

shasParser :: Parser [Text]
shasParser = some shaParser <* eof
  where
    shaParser = do
      _ <- manyTill anySingle (string "\"sha256\": \"")
      hash <- manyTill charLiteral (string "\",")
      _ <- manyTill anySingle (lookAhead $ void (char '{') <|> eof)
      return $ T.pack hash

textParser :: Parser String
textParser = choice ["\n" <$ eol, try (lineOrComment <* eol), lineOrComment <* eof]

lineOrComment :: Parser String
lineOrComment = choice ["" <$ try (space *> skipLineComment "--"), some printChar]

pkgsParser :: Parser Text
pkgsParser = T.unlines . map T.strip
           <$> sectionParser (lookAhead $ string "write-ghc-environment-files")

sectionParser :: Parser a -> Parser [Text]
sectionParser = fmap fmtParsedSection . manyTill textParser

fmtParsedSection :: [String] -> [Text]
fmtParsedSection ls = [T.pack $ if l /= "\n" then l else "" | l <- ls, not $ null l]

sect1Parser :: Parser [Text]
sect1Parser =  sectionParser (string "packages:")

sect2Parser :: Parser [Text]
sect2Parser = sectionParser (lookAhead $ void (string "source-repository-package") <|> eof)

sdsParser :: Text -> Parser Text
sdsParser t = fmtSds <$> manyTill textParser (lookAhead $ void (string t) <|> eof)
  where
    fmtSds ls = T.unlines [("      " <>) . T.strip $ T.pack l | l <- ls, not $ null l, l /= "\n"]

srpParser :: Parser Dependency
srpParser = do
  _ <- string "source-repository-package" *> space
  depType <- fieldP "type:"
  depLoc <- fieldP "location:"
  depTag <- fieldP "tag:"
  _ <- space *> string "subdir:" *> hspace *> eol
  depSubdirs <-  sdsParser "source-repository-package"
  return $ Dependency {..}
  where
    fieldP :: Text -> Parser Text
    fieldP field = T.pack <$> (space *> string field *> space *> manyTill anySingle eol)

projectParser :: Tag -> Parser ProjectData
projectParser tag = do
  sect1 <- T.unlines <$> sect1Parser
  plutusAppsPkgs <- sdsParser "write-ghc-environment-files"
  let plutusApps = Dependency {
      depType = "git"
    , depLoc = "https://github.com/input-output-hk/plutus-apps.git"
    , depTag = tag
    , depSubdirs = plutusAppsPkgs
    }
  sect2 <- T.unlines <$> sect2Parser
  deps <- (plutusApps :) <$> many srpParser <* eof
  return $ ProjectData deps $ mconcat [projPkgs, sect1, sect2]
  where
    projPkgs = "packages: ./\n\n"