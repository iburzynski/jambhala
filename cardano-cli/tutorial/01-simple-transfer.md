# `cardano-cli` Exercise 01: Simple Transfer
In this exercise we'll practice generating wallet addresses and submitting a simple transfer transaction.

## **1. Generate addresses**
Inspect the contents of the `keygen` script at `cardano-cli/keygen`.

Then run the script twice to create keypairs and addresses for `alice` and `bob`:

```sh
$ keygen alice
wrote verification key to: assets/keys/alice.vkey
wrote signing key to: assets/keys/alice.skey
wrote address to: assets/keys/alice.addr

$ keygen bob
wrote verification key to: assets/keys/bob.vkey
wrote signing key to: assets/keys/bob.skey
wrote address to: assets/keys/bob.addr
```

## **2. Fund wallet from faucet and select UTXO**
Inspect the contents of the `addr` script at `cardano-cli/addr`.

Then run the script for `alice` to see her address:

```sh
$ addr alice
addr_test1vqa9qw00et75eqkfz6dnttaj0gtkw0mw67z75zr38xn95hgvh8v80
```

Copy Alice's wallet address and fund the wallet with 10,000 Test Ada from the [Testnet Faucet](https://docs.cardano.org/cardano-testnet/tools/faucet).

Inspect the contents of the `utxos` script at `cardano-cli/utxos`.

Then confirm the receipt of test Ada from the faucet by running the script with `alice` as argument:

```sh
$ utxos alice
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9c1203e214524e72f0e686b09821dc4c46a2b3cf0c6ee312f52a97cde00d1765     0        10000000000 lovelace + TxOutDatumNone
```

Copy the `TxHash` value and create a temporary variable called `U` to store this value, followed by a `#` symbol and the `TxIx` value:

```sh
$ U=9c1203e214524e72f0e686b09821dc4c46a2b3cf0c6ee312f52a97cde00d1765#0
```

## **3. Draft the transaction**
Now draft a transaction to transfer 250 ADA from Alice to Bob:

```sh
$ cardano-cli transaction build \
--babbage-era \
--tx-in $U \
--tx-out $(addr bob)+250000000 \
--change-address $(addr alice) \
$TESTNET \
--out-file $TX_PATH/transfer.raw
Estimated transaction fee: Lovelace 165721
```

## **4. Sign and submit the transaction**
Inspect the contents of the `tx-sign` script at `cardano-cli/tx-sign`.

Then run the script by providing the signer name (`alice`) and transaction name (`transfer`) as arguments:

```sh
$ tx-sign alice transfer
```

Inspect the contents of the `tx-submit` script at `cardano-cli/tx-submit`.

Then run the script by providing the transaction name (`transfer`) as argument:

```sh
$ tx-submit transfer
Transaction successfully submitted.
```

## **5. Check the result**
Run the `utxos` script with `bob` as argument to confirm the funds were received:

```sh
$ utxos bob
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
215650738a6a1c3fd8495075091a5853d9d48f9c818d791bdf464e6b8ae674c7     0        250000000 lovelace + TxOutDatumNone
```

You can copy the `TxHash` value and search for it on [cexplorer](https://preview.cexplorer.io/) to view the transaction details.