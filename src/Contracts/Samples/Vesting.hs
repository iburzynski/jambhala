module Contracts.Samples.Vesting where

import Data.Map qualified as Map
import Jambhala.Plutus
import Jambhala.Utils

-- Datum
data VestingDatum = VestingDatum
  { toBeneficiary :: PubKeyHash,
    afterMaturity :: POSIXTime
  }
  deriving (Generic, ToJSON, FromJSON)

makeIsDataIndexed ''VestingDatum [('VestingDatum, 0)]

vesting :: VestingDatum -> () -> ScriptContext -> Bool
vesting (VestingDatum beneficiary maturity) _ (ScriptContext txInfo _) =
  traceIfFalse "Wrong beneficiary" signedByBeneficiary
    && traceIfFalse "Maturity not reached" maturityReached
  where
    signedByBeneficiary = txSignedBy txInfo beneficiary
    maturityReached = contains (from maturity) $ txInfoValidRange txInfo
{-# INLINEABLE vesting #-}

type Vesting = ValidatorContract "vesting"

validator :: Vesting
validator = mkValidatorContract $$(compile [||wrapped||])
  where
    wrapped = mkUntypedValidator vesting

instance ValidatorEndpoints Vesting where
  data GiveParam Vesting = Give
    { lovelace :: Integer,
      withDatum :: VestingDatum
    }
    deriving (Generic, ToJSON, FromJSON)
  data GrabParam Vesting = Grab
    deriving (Generic, ToJSON, FromJSON)

  give :: GiveParam Vesting -> ContractM Vesting ()
  give (Give lovelace datum) = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor validator,
          constraints = mustPayToScriptWithDatum validator datum lovelace
        }
    logStr $
      printf
        "Made a gift of %d lovelace to %s with maturity %s"
        lovelace
        (show $ toBeneficiary datum)
        (show $ afterMaturity datum)

  grab :: GrabParam Vesting -> ContractM Vesting ()
  grab _ = do
    pkh <- getOwnPkh
    now <- getCurrentInterval
    utxos <- getUtxosAt validator
    let validUtxos = Map.mapMaybe (isEligible pkh now) utxos
    if validUtxos == mempty
      then logStr "No eligible gifts available"
      else do
        submitAndConfirm
          Tx
            { lookups = scriptLookupsFor validator `andUtxos` validUtxos,
              constraints =
                mconcat
                  [ mustValidateInTimeRange (fromPlutusInterval now),
                    mustSign pkh,
                    validUtxos `mustAllBeSpentWith` ()
                  ]
            }
        logStr "Collected eligible gifts"
    where
      isEligible :: PubKeyHash -> Interval POSIXTime -> DecoratedTxOut -> Maybe DecoratedTxOut
      isEligible pkh now dto = do
        (_, dfq) <- getDecoratedTxOutDatum dto
        Datum d <- getDatumInDatumFromQuery dfq
        VestingDatum ben maturity <- fromBuiltinData d
        guard (ben == pkh && from maturity `contains` now)
        Just dto

test :: EmulatorTest
test =
  initEmulator @Vesting
    4
    [ Give
        { lovelace = 30_000_000,
          withDatum =
            VestingDatum
              { toBeneficiary = pkhForWallet 2,
                afterMaturity = m
              }
        }
        `fromWallet` 1,
      Give
        { lovelace = 30_000_000,
          withDatum =
            VestingDatum
              { toBeneficiary = pkhForWallet 4,
                afterMaturity = m
              }
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

exports :: JambExports -- Prepare exports for jamb CLI:
exports =
  export
    (defExports validator)
      { dataExports =
          [ VestingDatum
              { -- 1. Use the `key-hash` script from cardano-cli-guru to get the pubkey hash for a beneficiary address.
                -- 2. Replace the placeholder hex string below with the beneficiary address pubkey hash.
                toBeneficiary = "3a5039efcafd4c82c9169b35afb27a17673f6ed785ea087139a65a5d",
                -- 3. With cardano-node running, use the `calc-time` script from cardano-cli-guru to get a POSIX time value
                --    (add the `--plus MINUTES` option, replacing MINUTES with a number of minutes to add).
                -- 4. Replace the placeholder value below with your POSIX time value.
                -- 5. Note the NEW SLOT value for later use in transaction construction.
                afterMaturity = 1689950332
              }
              `toJSONfile` "vdatum"
          ],
        emulatorTest = test
      }