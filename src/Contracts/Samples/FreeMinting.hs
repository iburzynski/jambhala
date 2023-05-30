module Contracts.Samples.FreeMinting where

import Jambhala.Plutus
import Jambhala.Utils

freeMinting :: () -> ScriptContext -> Bool
freeMinting _ _ = True
{-# INLINEABLE freeMinting #-}

policy :: MintingPolicy
policy = mkMintingPolicyScript $$(compile [||mkUntypedMintingPolicy freeMinting||])

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

exports :: ContractExports
exports = exportMintingPolicy policy []