# **`cardano-cli` Exercise 04: Multi-signature Policy Script**

In this exercise we'll submit a more complex transaction in which funds from a multi-signature policy script are transferred to a fourth user, `dan`, when given authorization from `alice`, `bob` and `charlie`.

1. **Generate a keypair and address for `dan`.**
2. **[Create a multi-signature policy script](#script) with placeholder values.**
3. **[Get key-hashes](#keyhashes) for `alice`, `bob`, and `charlie` and replace the placeholder values in the script file.**
4. **[Generate a script address](#address) using the script file.**
5. **[Fund the script address](#fund) with Test ADA.**
6. **[Build the transaction](#build) using the `--witness-override` option.**
7. **Witness the transaction for `alice`, `bob`, and `charlie`.**
8. **[Assemble](#assemble) the transaction.**
9. **Submit the transaction.**
10. **Extra: [verify the script](#verify).**

## <a id="script"></a> **Create a multi-signature policy script**
Create a file called `multisig.script` in the `assets/scripts/native` directory, and paste the following:

```json
{
  "type": "all",
  "scripts":
  [
    {
      "type": "sig",
      "keyHash": "<KEY-HASH1>"
    },
    {
      "type": "sig",
      "keyHash": "<KEY-HASH2>"
    },
    {
      "type": "sig",
      "keyHash": "<KEY-HASH3>"
    }
  ]
}
```

## <a id="keyhashes"></a> **Get key-hashes**
The `<KEY-HASH#>` placeholder values in `multisig.script` must be replaced with the key-hashes of the users who will authorize the transaction (`alice`, `bob` and `charlie`).

Jambhala provides a `key-hash` script to conveniently display the key-hash of a user. It contains the following `cardano-cli` command:

```sh
cardano-cli address key-hash \
--payment-verification-key-file "$KEYS_PATH/$1.vkey"
```

Run the script with a user name argument to display the user's keyhash:

```sh
$ key-hash alice
3a5039efcafd4c82c9169b35afb27a17673f6ed785ea087139a65a5d
```

Do this for `alice`, `bob` and `charlie` and paste their key-hashes into `multisig.script`, replacing the placeholder values, i.e.:

```json
    {
      "type": "sig",
      "keyHash": "3a5039efcafd4c82c9169b35afb27a17673f6ed785ea087139a65a5d"
    },
```

## <a id="address"></a> **Generate a script address**
Jambhala provides a script called `script-addr`, which contains the following:

```sh
# cardano-cli/script-addr

cardano-cli address build \
--payment-script-file $NATIVE_SCRIPTS_PATH/$1.script \
--out-file $addr \
$NET
```

When run with a script name as argument, it does the following:
 * Locates the corresponding `.script` file at the filepath specified by the `NATIVE_SCRIPTS_PATH` variable in the `.envrc` file
 * Provides this as argument to the `--payment-script-file` option of the `address build` command
 * Generates a Cardano address for the script and saves it to a `.addr` file at the filepath specified by the `addr` variable.

Run the script to generate the script address:

```sh
$ script-addr multisig
wrote address to: assets/addr/multisig.addr
```

Now run the `addr` script to view the script address:

```sh
$ addr multisig
```

## <a id="fund"></a> **Fund the script address**
Copy the script address from the previous step and fund it with 10,000 Test ADA from the [Testnet Faucet](https://docs.cardano.org/cardano-testnet/tools/faucet).

>Make sure to select the correct `Environment` at the Testnet Faucet. By default Jambhala uses the `Preview Testnet`.

Wait a few minutes, and then run the `utxos` script to confirm the receipt of funds at the script:

```sh
$ utxos multisig
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
3f2a8e3292c7d34b54d4020aea2a4dc993334ab50376dab2fd4295bbdf48dad3     0        10000000000 lovelace + TxOutDatumNone
```

Copy the `TxHash` and `TxIx` values from your terminal output and assign the UTXO to a temporary variable `U`, as we've done in previous exercises.

## <a id="build"></a> **Build the transaction**
We'll build the transaction as usual with the `transaction build` command, but with two additional options:

1. Building a transaction involving a script requires providing the script file as an argument to the **`--tx-in-script-file`** option.
2. A **witness override** to correctly calculate the transaction fee.

### **`witness-override`**
The `--witness-override` option allows us to override the default number of witnesses (`1`) if our transaction requires signatures from multiple keys. Adding additional witnesses makes a transaction larger and thus more expensive: without including this option, `cardano-cli` will calculate the fee based on a single signature and the transaction will fail due to an insufficient fee. The option is provided to the `transaction build` command with a numeric argument corresponding to the number of necessary signatures.

Our transaction will be built as follows:

```sh
$ cardano-cli transaction build \
--tx-in $U \
--change-address $(addr dan) \
--tx-in-script-file "$NATIVE_SCRIPTS_PATH/multisig.script" \
--out-file "$TX_PATH/multisig.raw" \
--witness-override 3 \
$NET
```

## <a id="assemble"></a> **Assemble the transaction**
After witnessing the transaction for `alice`, `bob`, and `charlie` (i.e. `$ tx-witness alice multisig`), assemble it as follows:

```sh
cardano-cli transaction assemble \
--tx-body-file "$TX_PATH/multisig.raw" \
--witness-file "$TX_PATH/multisig-alice.witness" \
--witness-file "$TX_PATH/multisig-bob.witness" \
--witness-file "$TX_PATH/multisig-charlie.witness" \
--out-file "$TX_PATH/multisig.signed"
```

You can now submit the transaction and view the results.

## <a id="verify"></a> **Extra: verify script**
You can confirm the fidelity of a local copy of a Cardano native script to an address on-chain using `cardanoscan`.

Search for the script address at [preview.cardanoscan.io](https://preview.cardanoscan.io) (use `preprod.cardanoscan.io` instead if using the `preprod` testnet).

You should see a **Contract** heading for the address, and a `Verify` button next to it. Click the button and select `Native script` from the dropdown. Paste the entire JSON contents of the `multisig.script` file into the box and click on `Verify`. This will confirm that the contents accord with the script address.