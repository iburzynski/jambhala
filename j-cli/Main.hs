module Main where

import Contracts (contracts, samples)
import Jambhala.CLI (runJamb)

main :: IO ()
main = runJamb allContracts -- << replace `allContracts` with `contracts` to hide sample contracts
  where
    allContracts = contracts <> samples
