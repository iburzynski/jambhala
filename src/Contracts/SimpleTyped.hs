{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Contracts.SimpleTyped where

import Utils.Utils ( wrap, getSerialised, viewCBOR )

import Data.ByteString ( ByteString )
import Plutus.Script.Utils.V2.Scripts ( validatorHash )
import Plutus.Script.Utils.V2.Typed.Scripts.Validators
    ( ValidatorTypes(..), mkTypedValidator, validatorScript )
import Plutus.V1.Ledger.Address ( scriptHashAddress )
import Plutus.V2.Ledger.Contexts ( ScriptContext )
import Plutus.V2.Ledger.Api ( Validator, ValidatorHash, Address )
import PlutusTx ( compile )

-- Type Families map types to other (like functions but at the type level)
data MyTyped
instance ValidatorTypes MyTyped where
  type instance DatumType MyTyped = () -- DatumType Simple is represented by ()
  type instance RedeemerType MyTyped = Integer -- RedeemerType Simple is represented by Integer

{-# INLINABLE simpleTyped #-}
simpleTyped :: () -> Integer -> ScriptContext -> Bool
simpleTyped _ redeemer _ = traceIfFalse "Sorry, wrong guess!" (redeemer == 42)

validator :: Validator
validator = validatorScript $ mkTypedValidator @MyTyped
  $$(compile [|| simpleTyped ||])
  $$(compile [|| wrap ||])

simpleTypedCBOR :: ByteString
simpleTypedCBOR = viewCBOR $ getSerialised validator

simpleTypedVHash :: ValidatorHash
simpleTypedVHash = validatorHash validator

simpleTypedAddress :: Address
simpleTypedAddress = scriptHashAddress simpleTypedVHash