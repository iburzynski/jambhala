-- Allows convenient unpacking of record selectors into named variables
{-# LANGUAGE RecordWildCards #-}

-- 0. Create Module & Declare Imports
module Contracts.Samples.ParamVesting where

import Jambhala.Plutus
import Jambhala.Utils

-- 1. Declare Types: N/A

-- 2. Define Lambda

paramVestingLambda :: PubKeyHash -> (POSIXTime -> () -> ScriptContext -> Bool)
paramVestingLambda beneficiaryPKH maturity _ (ScriptContext txInfo _) =
  traceIfFalse "Wrong beneficiary" (txSignedBy txInfo beneficiaryPKH)
    && traceIfFalse "Maturity not reached" (from maturity `contains` txInfoValidRange txInfo)
{-# INLINEABLE paramVestingLambda #-}

untypedLambda :: PubKeyHash -> UntypedValidator -- BuiltinData -> BuiltinData -> BuiltinData -> ()
untypedLambda beneficiaryPKH = mkUntypedValidator $ paramVestingLambda beneficiaryPKH
{-# INLINEABLE untypedLambda #-}

-- 3. Pre-Compile Lambda

-- | Declare contract synonym with unique symbolic identifier.
type Vesting = ValidatorContract "param-vesting"

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
compileScript :: PubKeyHash -> Vesting
compileScript beneficiaryPKH =
  mkValidatorContract
    ( $$(compile [||untypedLambda||]) -- compile the untyped parameterized validator
        `applyCode` liftCode beneficiaryPKH -- apply the result to the "lifted" argument
    )

-- 4. Export Contract to Jambhala

-- | Define `exports` value for use with `j cli`.
exports :: JambExports
exports =
  export
    (defExports appliedScript)
      { -- 1. With cardano-node running, use the `calc-time` script from cardano-cli-guru to get a POSIX time value
        --    (add the `--plus MINUTES` option, replacing MINUTES with a number of minutes to add).
        -- 2. Replace the placeholder POSIXTime value below with your POSIX time value.
        -- 3. Note the NEW SLOT value for later use in transaction construction.
        dataExports = [(1689950332 :: POSIXTime) `toJSONfile` "maturity"]
      , emulatorTest = test
      }
  where
    -- The parameterized validator must be applied to a `PubKeyHash` argument before it can be exported.
    appliedScript =
      -- 4. Use the `key-hash` script from cardano-cli-guru to get the pubkey hash for a beneficiary address.
      -- 5. Replace the placeholder hex string below with the beneficiary address pubkey hash.
      compileScript "3a5039efcafd4c82c9169b35afb27a17673f6ed785ea087139a65a5d"

-- 5. Define Emulator Component

instance ValidatorEndpoints Vesting where
  data GiveParam Vesting = Give
    { lovelace :: Integer
    , forBeneficiary :: PubKeyHash
    , availableAfter :: POSIXTime
    }
    deriving (Generic, ToJSON, FromJSON)
  data GrabParam Vesting = Grab
    deriving (Generic, ToJSON, FromJSON)

  give :: GiveParam Vesting -> ContractM Vesting ()
  give Give {..} = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor validator
        , constraints = mustPayScriptWithDatum validator availableAfter (lovelaceValueOf lovelace)
        }
    logStr $
      printf
        "Made a gift of %d lovelace to %s with maturity %s"
        lovelace
        (show forBeneficiary)
        (show availableAfter)
    where
      validator = compileScript forBeneficiary

  grab :: GrabParam Vesting -> ContractM Vesting ()
  grab _ = do
    beneficiaryPKH <- getOwnPKH
    let appliedScript = compileScript beneficiaryPKH
    utxos <- getUtxosAt appliedScript
    now <- getCurrentInterval
    let validUtxos = filterByDatum (\maturity -> from maturity `contains` now) utxos
    if validUtxos == mempty
      then logStr "No eligible gifts available"
      else do
        submitAndConfirm
          Tx
            { lookups = scriptLookupsFor appliedScript `andUtxos` validUtxos
            , constraints =
                mconcat
                  [ mustValidateInTimeRange (fromPlutusInterval now)
                  , mustSign beneficiaryPKH
                  , validUtxos `mustAllBeSpentWith` ()
                  ]
            }
        logStr "Collected eligible gifts"

-- | Define emulator test.
test :: EmulatorTest
test =
  initEmulator @Vesting
    4
    [ Give
        { lovelace = 30_000_000
        , forBeneficiary = pkhForWallet 2
        , availableAfter = maturity
        }
        `fromWallet` 1
    , Give
        { lovelace = 30_000_000
        , forBeneficiary = pkhForWallet 4
        , availableAfter = maturity
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
