-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Allows convenient unpacking of record selectors into named variables
{-# LANGUAGE RecordWildCards #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}

module Contracts.Samples.ParamVesting where

import Data.Map qualified as Map
import Jambhala.Plutus
import Jambhala.Utils
import Plutus.Script.Utils.V2.Typed.Scripts (UntypedValidator)

newtype VestingParam = VParam {beneficiary :: PubKeyHash}
  deriving (Generic, ToJSON, FromJSON)

makeLift ''VestingParam

paramVesting :: VestingParam -> POSIXTime -> () -> ScriptContext -> Bool
paramVesting (VParam beneficiary) maturity _ (ScriptContext txInfo _) =
  traceIfFalse "Wrong beneficiary" signedByBeneficiary
    && traceIfFalse "Maturity not reached" maturityReached
  where
    signedByBeneficiary = txSignedBy txInfo beneficiary
    maturityReached = from maturity `contains` txInfoValidRange txInfo
{-# INLINEABLE paramVesting #-}

type VestingContract = ValidatorContract "param-vesting"

contract :: VestingParam -> VestingContract
contract p = mkValidatorContract ($$(compile [||untyped||]) `applyCode` liftCode p)
  where
    untyped :: VestingParam -> UntypedValidator
    untyped p' = mkUntypedValidator $ paramVesting p'

instance ValidatorEndpoints VestingContract where
  data GiveParam VestingContract = Give
    { lovelace :: Integer,
      forBeneficiary :: PubKeyHash,
      afterMaturity :: POSIXTime
    }
    deriving (Generic, ToJSON, FromJSON)
  data GrabParam VestingContract = Grab
    deriving (Generic, ToJSON, FromJSON)

  give :: GiveParam VestingContract -> ContractM VestingContract ()
  give Give {..} = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor contract',
          constraints = mustPayToScriptWithDatum contract' afterMaturity lovelace
        }
    logStr $
      printf
        "Made a gift of %d lovelace to %s with maturity %s"
        lovelace
        (show forBeneficiary)
        (show afterMaturity)
    where
      contract' = contract $ VParam forBeneficiary

  grab :: GrabParam VestingContract -> ContractM VestingContract ()
  grab _ = do
    pkh <- getOwnPkh
    let contract' = contract $ VParam pkh
    utxos <- getUtxosAt contract'
    now <- getCurrentInterval
    let validUtxos = Map.mapMaybe (isEligible now) utxos
    if validUtxos == mempty
      then logStr "No eligible gifts available"
      else do
        submitAndConfirm
          Tx
            { lookups = scriptLookupsFor contract' `andUtxos` validUtxos,
              constraints =
                mconcat
                  [ mustValidateInTimeRange (fromPlutusInterval now),
                    mustSign pkh,
                    validUtxos `mustAllBeSpentWith` ()
                  ]
            }
        logStr "Collected eligible gifts"
    where
      isEligible :: Interval POSIXTime -> DecoratedTxOut -> Maybe DecoratedTxOut
      isEligible now dto = do
        (_, dfq) <- getDecoratedTxOutDatum dto
        Datum d <- getDatumInDatumFromQuery dfq
        maturity <- fromBuiltinData d
        guard (from maturity `contains` now)
        Just dto

test :: EmulatorTest
test =
  initEmulator @VestingContract
    4
    [ Give
        { lovelace = 30_000_000,
          forBeneficiary = pkhForWallet 2,
          afterMaturity = m
        }
        `fromWallet` 1,
      Give
        { lovelace = 30_000_000,
          forBeneficiary = pkhForWallet 4,
          afterMaturity = m
        }
        `fromWallet` 1,
      Grab `toWallet` 2, -- deadline not reached
      waitUntil 20,
      Grab `toWallet` 3, -- wrong beneficiary
      Grab `toWallet` 4 -- collect gift
    ]
  where
    m :: POSIXTime
    m = defaultSlotBeginTime 20

exports :: JambExports
exports =
  export
    (defExports contract')
      { -- 1. With cardano-node running, use the `calc-time` script from cardano-cli-guru to get a POSIX time value
        --    (add the `--plus MINUTES` option, replacing MINUTES with a number of minutes to add).
        -- 2. Replace the placeholder POSIXTime value below with your POSIX time value.
        -- 3. Note the NEW SLOT value for later use in transaction construction.
        dataExports = [(1689950332 :: POSIXTime) `toJSONfile` "maturity"],
        emulatorTest = test
      }
  where
    -- The validator must be applied to some parameter to generate a hash or serialise the script to a file.
    contract' =
      contract
        VParam
          { -- 4. Use the `key-hash` script from cardano-cli-guru to get the pubkey hash for a beneficiary address.
            -- 5. Replace the placeholder hex string below with the beneficiary address pubkey hash.
            beneficiary = "3a5039efcafd4c82c9169b35afb27a17673f6ed785ea087139a65a5d"
          }