-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Contracts.Samples.ParamVesting where

import Jambhala.Plutus
import Jambhala.Utils

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

type ParamVesting = ValidatorContract "param-vesting"

contract :: VestingParam -> ParamVesting
contract p = mkValidatorContract $ $$(compile [||untyped||]) `applyCode` liftCode p
  where
    untyped = mkUntypedValidator . paramVesting

instance ValidatorEndpoints ParamVesting where
  data GiveParam ParamVesting = Give
    { lovelace :: Integer,
      forBeneficiary :: PubKeyHash,
      afterMaturity :: POSIXTime
    }
    deriving (Generic, ToJSON, FromJSON)
  data GrabParam ParamVesting = Grab {withMaturity :: POSIXTime}
    deriving (Generic, ToJSON, FromJSON)

  give :: GiveParam ParamVesting -> ContractM ParamVesting ()
  give Give {..} = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor contract',
          constraints = mustPayToScriptWithDatum contract' () lovelace
        }
    logStr $
      printf
        "Made a gift of %d lovelace to %s with maturity %s"
        lovelace
        (show forBeneficiary)
        (show afterMaturity)
    where
      contract' = contract VParam {beneficiary = forBeneficiary}

  grab :: GrabParam ParamVesting -> ContractM ParamVesting ()
  grab (Grab maturity) = do
    now <- getCurrentInterval
    if from maturity `contains` now
      then do
        pkh <- getOwnPkh
        let p = VParam {beneficiary = pkh}
            contract' = contract p
        validUtxos <- getUtxosAt contract'
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
      else logStr "Maturity not reached"

test :: EmulatorTest
test =
  initEmulator @ParamVesting
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
      Grab {withMaturity = m} `toWallet` 2, -- deadline not reached
      waitUntil 20,
      Grab {withMaturity = m} `toWallet` 3, -- wrong beneficiary
      Grab {withMaturity = m} `toWallet` 4 -- collect gift
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