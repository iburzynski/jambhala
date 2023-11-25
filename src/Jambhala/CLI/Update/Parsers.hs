module Jambhala.CLI.Update.Parsers (
  CabalProjectData (..),
  Dependency (..),
  cabalProjectParser,
  prefetchGitParser,
)
where

import Data.Text qualified as T
import Text.Megaparsec (
  MonadParsec (..),
  Parsec,
  anySingle,
  choice,
  manyTill,
 )
import Text.Megaparsec.Char (char, eol, hspace, printChar, space, string)
import Text.Megaparsec.Char.Lexer (charLiteral, skipLineComment)

type Parser = Parsec Void Text

data Dependency = Dependency
  { depType :: !Text
  , depLoc :: !Text
  , depTag :: !Text
  , depSubdirs :: !(Maybe Text)
  }
  deriving (Show)

data CabalProjectData = CabalProjectData ![Dependency] !Text deriving (Show)

textParser :: Parser String
textParser = choice ["\n" <$ eol, try (lineOrComment <* eol), lineOrComment <* eof]
  where
    lineOrComment = choice ["" <$ try (space *> skipLineComment "--"), some printChar]

cabalProjectParser :: Text -> Parser CabalProjectData
cabalProjectParser tag = do
  sect1 <- sectP $ string "packages:"
  sds <- subdirsParser "write-ghc-environment-files"
  sect2 <- sectP . lookAhead $ void (string "source-repository-package") <|> eof
  let plutusAppsSrp =
        Dependency
          { depType = "git"
          , depLoc = "https://github.com/input-output-hk/plutus-apps.git"
          , depTag = tag
          , depSubdirs = Just sds
          }
  deps <- (plutusAppsSrp :) <$> manyTill srpParser eof
  pure . CabalProjectData deps $ mconcat ["packages: ./\n\n", sect1, sect2]
  where
    sectP = fmap T.unlines . sectionParser

sectionParser :: Parser a -> Parser [Text]
sectionParser = fmap fmtSection . manyTill textParser
  where
    fmtSection ls = [T.pack $ if l /= "\n" then l else "" | l <- ls, not $ null l]

subdirsParser :: Text -> Parser Text
subdirsParser t = fmtSds <$> manyTill textParser (lookAhead $ void (string t) <|> eof)
  where
    fmtSds sds =
      T.unlines $
        [("      " <>) . T.strip $ T.pack sd | sd <- sds, sd `notElem` ["", "\n"]]

srpParser :: Parser Dependency
srpParser = do
  _ <- many (space *> skipLineComment "--" *> eol)
  _ <- try $ lookAhead $ string "source-repository-package"
  let fieldP fld = T.pack <$> (space *> string fld *> space *> manyTill anySingle eol)
  _ <- string "source-repository-package" *> space
  depType <- fieldP "type:"
  depLoc <- fieldP "location:"
  depTag <- fieldP "tag:"
  depSubdirs <- optional . try $ (space *> string "subdir:" *> hspace *> eol *> subdirsParser "source-repository-package")

  pure $ Dependency {..}

prefetchGitParser :: Parser [Text]
prefetchGitParser = some sha256Parser <* eof
  where
    sha256Parser = do
      _ <- manyTill anySingle (string "\"sha256\": \"")
      hash <- manyTill charLiteral (string "\",")
      _ <- manyTill anySingle (lookAhead $ void (char '{') <|> eof)
      pure $ T.pack hash
