# **`cardano-cli` Exercise 02: Building Transactions with `build-raw`**
**Note**: you'll need your local node to be running and fully synced to complete this exercise. If your node is correctly installed and configured for use with Jambhala, you can use  the appropriate alias in your terminal to start `cardano-node`:
- `preprod-node` for preprod testnet
- `preview-node` for preview testnet

See the **[Starting and syncing the node](./00-installation.md#starting-and-syncing-the-node)** section of the installation guide if you face any difficulty.

We'll also be reusing the addresses and key pairs created during the previous exercise, so you'll need to have completed at least **Steps 1 & 2** of **[Exercise 1](./01-simple-transfer.md)** before proceeding further.

***
In this exercise we'll take a deeper look at how Cardano transactions are constructed and practice using `cardano-cli`'s lower-level `transaction build-raw` command. We'll also create a more complex transaction by specifying a **validity interval** inside of which it must be submitted.

Unlike the `transaction build` command, transactions constructed with `build-raw` aren't automatically balanced, so we'll need to calculate the fee and deduct it from the transaction's outputs manually. We'll also manually compute the **validity interval**, which specifies a window of time (measured in slots) during which the transaction can be processed.

The process for this exercise will be as follows:

1. **[Get the protocol parameters](#params)**
2. **[Draft the transaction](#draft)**
3. **[Calculate the fee and change](#fee)**
4. **[Define the validity interval](#validity)**
5. **[Build the transaction](#build)**
6. **[Sign the transaction](#sign)**
7. **[Submit the transaction](#submit)**

## <a id="params"></a> **1. Get the protocol parameters**
**Protocol parameters** refer to the various settings that determine how the Cardano network functions, including things like transaction fees, block sizes, rewards for validators, and other important aspects of the system.

Since `build-raw` requires us to calculate transaction fees manually, we need a copy of the parameters on hand; this way we can use the fee information contained within to arrive at the correct amount for the fee.

Jambhala provides a helper script called `params` to query and save the protocol parameters in the `assets` directory:

```sh
# cardano-cli/params

cardano-cli query protocol-parameters \
--out-file $PARAMS_PATH
```

The script runs the `query protocol-parameters` command with the network option and the filepath configured in the `PARAMS_PATH` environment variable for the `out-file` option.

Run the script to save the protocol parameters:

```sh
$ params
saved protocol parameters to 'assets/params.json'
```

Inspect the parameters output at `assets/params.json`, particularly the `txFee` attributes at the bottom of the file:

```json
{
  ...

    "txFeeFixed": 155381,
    "txFeePerByte": 44,

  ...
}
```

We'll use this file in **Step 3** to calculate the transaction fee.

## **2. <a id="draft"></a> Draft the transaction**
Like in the previous exericse, we need to find a UTXO at `alice`'s address for the transaction to consume:

```sh
utxos alice
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
215650738a6a1c3fd8495075091a5853d9d48f9c818d791bdf464e6b8ae674c7     1        9749834279 lovelace + TxOutDatumNone
```

Copy the `TxHash` value and create a temporary variable called `U` to store it, followed by a `#` symbol and the `TxIx` value:

```sh
U=215650738a6a1c3fd8495075091a5853d9d48f9c818d791bdf464e6b8ae674c7#1
```

Copy the `Amount` value and create a temporary variable called `INPUT_AMT` to store it:

```sh
INPUT_AMT=9749834279
```

We'll use this value in our calculations later.

Now we'll draft the transaction as follows, using the `build-raw` command:

```sh
cardano-cli transaction build-raw \
--tx-in $U \
--tx-out $(addr bob)+250000000 \
--tx-out $(addr alice)+0 \
--invalid-hereafter 0 \
--fee 0 \
--out-file $TX_PATH/transfer-raw.draft
```

In the draft, the `tx-out` for Alice's change, `invalid-hereafter` and `fee` can all be set to zero initially. Later when we build the final transaction we'll use the draft's `out-file` (`transfer-raw.draft`) to calculate the `fee` and Alice's change, and set the `invalid-hereafter` value based on the current slot.

## **3. <a id="fee"></a> Calculate the fee and change**

Jambhala provides a `min-fee` script, which uses `cardano-cli`'s `transaction calculate-min-fee` command to determine the transaction fee.

The script takes four arguments, in the following format:

```sh
min-fee <tx_name> -i <in_count> -o <out_count> -w <witness_count>
```
>The `-i`, `-o`, and `-w` arguments can be provided in any order.

The `calculate-min-fee` subcommand takes the transaction draft, the input, output and witness counts, the network option and the protocol parameters (which we saved in **Step 1**), and determines the minimum fee required by the transaction. The script then trims the result to return the lovelace value as an integer:

```sh
cardano-cli transaction calculate-min-fee \
--tx-body-file $TX_PATH/$tx_name.draft \
--tx-in-count $in_count \
--tx-out-count $out_count \
--witness-count $witness_count \
--protocol-params-file $PARAMS_PATH | cut -d' ' -f1
```

We'll run the script for our `transfer-raw` transaction (which has `1` input, `2` outputs and requires `1` witness) and save its result to a temporary variable so we can reference it later:

```sh
FEE=$(min-fee transfer-raw -i 1 -o 2 -w 1)

echo $FEE
174433
```

### **Calculating the change**
Now we need to calculate the return balance (Alice's change), which we'll also save to a temporary variable. The return balance should be understood as `(SUM OF INPUT UTXOs) - (SUM OF OUTPUT UTXOs) - FEE` (in this case: `9749834279 - 250000000 - 174433`).

We can use the `expr` command with the variables we assigned earlier and the output amount, and save it to another variable like this:

```sh
BALANCE=$(expr $INPUT_AMT - 250000000 - $FEE)
```

## **4. <a id="validity"></a> Determine the validity interval**
When building transactions, we can specify a time interval during which the transaction is valid (called the **validity interval**). Validity intervals are defined using **slot numbers**, and are specified as absolute values rather than relative ones. This is done using two associated options:

* **`--invalid-before`**: represents a slot that the transaction is valid from. We include this option if we want to set a lower boundary for the validity interval (i.e. prevent a transaction from processing before some desired time). A transaction remains invalid until the `--invalid-before` slot is reached.
* **`--invalid-hereafter`**: represents a deadline slot before which a transaction must be submitted. The `--invalid-hereafter` value should be greater than the current slot number. A transaction becomes invalid when the `--invalid-hereafter` slot is reached.

In this exercise we'll set the validity interval using just the `--invalid-hereafter` option.

We'll need to query the current slot (the "tip") of the blockchain and add some number of slots to it to provide a window of time in which the transaction can be submitted. For example, if the current tip is slot `12000000`, you should set the `--invalid-hereafter` to `12000000 + N`, where `N` is some number of slots providing sufficient time to build and submit the transaction. Submitting a transaction with an `--invalid-hereafter` value in the past will result in a transaction error.

The provided `tip` script queries the blockchain and provides information about the current slot:

```sh
tip
```

>Make sure `syncProgress` is `100.00`% before proceeding further. If not, allow your node time to sync and then run `tip` again to verify completion.

Look for the value of `slot` in the output:

```sh
{
    "block": 546553,
    "epoch": 141,
    "era": "Babbage",
    "hash": "ef2790faf28b6faa0edc86afec665d9d7a8c263a9b49039fbbedb18c964d4160",
    "slot": 12235226,
    "syncProgress": "100.00"
}
```

If we want to specify a validity interval of 10 minutes (600 slots), we add this value to the current slot. Use the `expr` command to compute the `--invalid-hereafter` value and save it to a variable:

```sh
VALIDTILL=$(expr 12235226 + 600)

echo $VALIDTILL
12235196
```

## **5. <a id="build"></a> Build the transaction**
We're now ready to build the transaction with all of the arguments (replacing the dummy zero values we used when building the draft):

```sh
cardano-cli transaction build-raw \
--tx-in $U \
--tx-out $(addr bob)+250000000 \
--tx-out $(addr alice)+$BALANCE \
--invalid-hereafter $VALIDTILL \
--fee $FEE \
--out-file $TX_PATH/transfer-raw.raw
```

## **6. <a id="sign"></a> Sign the transaction**
We can now sign the transaction using the `tx-sign` script:

```sh
tx-sign transfer-raw alice
```

>Review the contents of the `tx-sign` script at `cardano-cli/tx-sign` if needed, to make sure you understand the `cardano-cli` command for signing transactions.

## **7. <a id="submit"></a> Submit the transaction**
Now we submit the transaction with the `tx-submit` script:

```sh
tx-submit transfer-raw
```

>Review the contents of the `tx-submit` script at `cardano-cli/tx-submit` if needed, to make sure you understand the `cardano-cli` command for submitting transactions.

As in the previous exercise, you can run the `utxos` script for `bob` and `alice` to confirm the transfer of funds. The provided `tx-hash` script can also be used to get the hash of a transaction to look up transaction details on **[cardanoscan.io](cardanoscan.io)**:

```
tx-hash transfer-raw
a4579dec4246dfd8cc26b373fdfb6a4ab614777e6b9d444250994314081e32d2
```

## **Challenges**
Try to calculate the transaction fee using the information contained in `params.json`, using the formula `txFeeFixed + (txFeePerByte * TxSizeInBytes)`, and compare this to the fee calculated by `cardano-cli` in **Step 3**.
>**Hint:** look at the CBOR Hex attribute of `transfer-raw.signed` and calculate the size in bytes given that one hex digit is 4 bits (two hex digits = 1 byte).

Try building a "vesting" transaction that can only be submitted after a certain time (specify the validity interval using the `--invalid-before` option).