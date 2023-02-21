module Main where

import CLI ( runJamb )
import Contracts ( contracts, samples )

main :: IO ()
main = runJamb allContracts -- << replace `allContracts` with `contracts` to hide sample contracts
  where allContracts = samples <> contracts