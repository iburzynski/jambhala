-- 0. Create Module & Declare Imports

module Contracts.Samples.Vesting where

import Jambhala.Plutus
import Jambhala.Utils

-- 1. Declare Types

-- | Define a custom data type for the datum.
data VestingDatum = VestingDatum
  { toBeneficiary :: PubKeyHash
  , afterMaturity :: POSIXTime
  }
  deriving (Generic, ToJSON, FromJSON)

unstableMakeIsData ''VestingDatum

-- 2. Define Lambda

-- | Define typed vesting validator lambda.
vestingLambda :: VestingDatum -> () -> ScriptContext -> Bool
vestingLambda (VestingDatum beneficiary maturity) _ (ScriptContext txInfo _) =
  traceIfFalse "Not signed by beneficiary" (txSignedBy txInfo beneficiary)
    && traceIfFalse "Maturity not reached" (from maturity `contains` txInfoValidRange txInfo)
{-# INLINEABLE vestingLambda #-}

-- | Convert lambda into "untyped" form before pre-compilation (:: BuiltinData -> BuiltinData -> BuiltinData -> ())
untypedLambda :: UntypedValidator
untypedLambda = mkUntypedValidator vestingLambda
{-# INLINEABLE untypedLambda #-}

-- 3. Pre-Compile Lambda

-- | Declare contract synonym with unique symbolic identifier.
type Vesting = ValidatorContract "vesting"

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
compiledScript :: Vesting
compiledScript = mkValidatorContract $$(compile [||untypedLambda||])

-- 4. Export Contract to Jambhala

-- | Define `exports` value for use with `j cli`.
exports :: JambExports
exports =
  export
    (defExports compiledScript)
      { dataExports =
          [ VestingDatum
              { -- 1. Use the `key-hash` script from cardano-cli-guru to get the pubkey hash for a beneficiary address.
                -- 2. Replace the placeholder hex string below with the beneficiary address pubkey hash.
                toBeneficiary = "3a5039efcafd4c82c9169b35afb27a17673f6ed785ea087139a65a5d"
              , -- 3. With cardano-node running, use the `calc-time` script from cardano-cli-guru to get a POSIX time value
                --    (add the `--plus MINUTES` option, replacing MINUTES with a number of minutes to add).
                -- 4. Replace the placeholder value below with your POSIX time value.
                -- 5. Note the NEW SLOT value for later use in transaction construction.
                afterMaturity = 1689950332
              }
              `toJSONfile` "vdatum"
          ]
      , emulatorTest = test
      }

-- 5. Define Emulator Component

-- | Define `ValidatorEndpoints` instance for contract synonym.
instance ValidatorEndpoints Vesting where
  data GiveParam Vesting = Give
    { lovelace :: Integer
    , withDatum :: VestingDatum
    }
    deriving (Generic, ToJSON, FromJSON)
  data GrabParam Vesting = Grab
    deriving (Generic, ToJSON, FromJSON)

  give :: GiveParam Vesting -> ContractM Vesting ()
  give (Give lovelace datum) = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor compiledScript
        , constraints = mustPayScriptWithDatum compiledScript datum (lovelaceValueOf lovelace)
        }
    logStr $
      printf
        "Made a gift of %d lovelace to %s with maturity %s"
        lovelace
        (show $ toBeneficiary datum)
        (show $ afterMaturity datum)

  grab :: GrabParam Vesting -> ContractM Vesting ()
  grab _ = do
    pkh <- getOwnPKH
    now <- getCurrentInterval
    utxos <- getUtxosAt compiledScript
    let validUtxos = filterByDatum (isEligible pkh now) utxos
    if validUtxos == mempty
      then logStr "No eligible gifts available"
      else do
        submitAndConfirm
          Tx
            { lookups = scriptLookupsFor compiledScript `andUtxos` validUtxos
            , constraints =
                mconcat
                  [ mustValidateInTimeRange (fromPlutusInterval now)
                  , mustSign pkh
                  , validUtxos `mustAllBeSpentWith` ()
                  ]
            }
        logStr "Collected eligible gifts"
    where
      isEligible :: PubKeyHash -> Interval POSIXTime -> VestingDatum -> Bool
      isEligible pkh now (VestingDatum ben maturity) = ben == pkh && from maturity `contains` now

-- | Define emulator test.
test :: EmulatorTest
test =
  initEmulator @Vesting
    4
    [ Give
        { lovelace = 30_000_000
        , withDatum =
            VestingDatum
              { toBeneficiary = pkhForWallet 2
              , afterMaturity = maturity
              }
        }
        `fromWallet` 1
    , Give
        { lovelace = 30_000_000
        , withDatum =
            VestingDatum
              { toBeneficiary = pkhForWallet 4
              , afterMaturity = maturity
              }
        }
        `fromWallet` 1
    , Grab `toWallet` 2 -- deadline not reached
    , waitUntil 20
    , Grab `toWallet` 3 -- wrong beneficiary
    , Grab `toWallet` 4 -- collect gift
    ]
  where
    maturity :: POSIXTime
    maturity = defaultSlotBeginTime 20
