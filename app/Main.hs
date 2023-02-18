module Main where

import Utils.CLI ( commandParser, runCommand )
import Options.Applicative ( execParser )

main :: IO ()
main = execParser commandParser >>= runCommand