{-# LANGUAGE OverloadedLists #-}

{- HLINT ignore "Use empty" -}

module Contracts (contracts, samples) where

-- Sample contracts

-- Import your contracts here, i.e.:
-- import  Contracts.MyContract qualified as MyContract
import Contracts.Samples.Auction qualified as Auction
import Contracts.Samples.CustomTyped qualified as CustomTyped
import Contracts.Samples.ForwardMinting qualified as ForwardMinting
import Contracts.Samples.FreeMinting qualified as FreeMinting
import Contracts.Samples.NFT qualified as NFT
import Contracts.Samples.ParamVesting qualified as ParamVesting
import Contracts.Samples.SignedMinting qualified as SignedMinting
import Contracts.Samples.Simple qualified as Simple
import Contracts.Samples.SimpleGuessing qualified as SimpleGuessing
import Contracts.Samples.SimpleTyped qualified as SimpleTyped
import Contracts.Samples.StateCounter qualified as StateCounter
import Contracts.Samples.Vesting qualified as Vesting
import Jambhala.CLI.Types (JambContracts)

-- Add your contracts to the `contracts` list below, i.e.:
--  [
--    MyContract.exports
--  ]
contracts :: JambContracts
contracts =
  []

samples :: JambContracts
samples =
  [ Simple.giftExports
  , Simple.burnExports
  , SimpleTyped.typedExports
  , SimpleTyped.untypedExports
  , CustomTyped.exports
  , SimpleGuessing.exports
  , Vesting.exports
  , ParamVesting.exports
  , FreeMinting.exports
  , SignedMinting.exports
  , NFT.exports
  , StateCounter.exports
  , ForwardMinting.validatorExports
  , ForwardMinting.policyExports
  , Auction.exports
  ]
