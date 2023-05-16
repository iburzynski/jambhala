# **`cardano-cli` Exercise 03: Multi-witness Transaction**
In this exercise we'll submit a more complex transaction involving two input UTXOs from different users (`alice` and `bob`) to transfer funds to a third user (`charlie`).

A transaction **witness** consists of a user's verification (public) key and a signature of the transaction body hash using the user's signing (secret) key. The witness serves as a proof that the transaction has been signed by a user's secret key, which allows a transaction between multiple parties to be signed by all of them without sharing their secret keys.

Submitting a multi-witness transaction requires use of the `witness` and `assemble` subcommands of the `transaction` command, which are explained below. Helper scripts using these subcommands are provided for convenience. The rest of the exercise is left to you to test your understanding of the UTXO model and what you've learned so far about `cardano-cli`, although the high-level steps are provided to guide you.

***
## **Steps for building a multi-witness transaction**
1. Create a key-pair and address for `charlie`.
2. Find one UTXO to consume from `alice` and save it to a temporary variable `U1` (with its value assigned in the format `TxHash#TxIx`). Do the same for a UTXO from `bob`, saving to a temporary variable `U2`.
3. Build the transaction body using the `transaction build` command, including the following:
  * `U1` and `U2` as inputs
  * `charlie`'s address and the transfer amount as the output
  * `alice`'s address as the change address
4. Create witness files for both senders.
5. Assemble the transaction.
6. Submit the transaction.

***
### **`witness` subcommand**
The `transaction witness` command signs a `tx-body-file` with a user's secret key and produces an output file containing a proof of signature (by convention this file has a `.witness` extension).

Jambhala provides a helper script called `tx-witness`, which contains the following:

```sh
# cardano-cli/tx-witness

for user in "${@:2}"; do
  out="$TX_PATH/$1-$user.witness"

  cardano-cli transaction witness \
  --tx-body-file "$TX_PATH/$1.raw" \
  --signing-key-file "$KEYS_PATH/$user.skey" \
  --out-file $out

  ...
```

The script takes a transaction name as its first argument (`$1`), followed by one or more user names separated by spaces. It loops over each user argument, assigns an `out` variable to the filepath/name where the witness file will be saved, then runs `cardano-cli`'s `transaction witness` command, providing options for the body-file, the user's signing key, the network, and the out-file location.

Run the script for a transaction and its required witnesses as follows:

```sh
tx-witness multiwit alice bob
```

### **`assemble` subcommand**
The `transaction assemble` command creates a transaction by combining its witness(es) with the transaction body. It serves as an equivalent to `transaction sign` for transactions requiring multiple signatures that allows signers to witness the transaction securely.

Jambhala provides a helper script called `tx-assemble`, which contains the following:

```sh
# cardano-cli/tx-assemble

tx="$TX_PATH/$1"
witfiles=""

for user in "${@:2}"; do
  witfiles+="--witness-file $TX_PATH/$1-$user.witness "
done

cardano-cli transaction assemble \
--tx-body-file $tx.raw \
$witfiles \
--out-file $tx.signed
```

Like the `tx-witness` script, this script takes a transaction name as its first argument (`$1`), followed by one or more user names separated by spaces. It loops over each user argument and appends a `--witness-file` option with the filepath/name where the user's witness file is located, then runs `cardano-cli`'s `transaction assembles` command, providing the body-file, `witness-file` options for all the users, and the out-file location.

Run the `tx-assemble` script to assemble the signed transaction:

```sh
tx-assemble multiwit alice bob
```

***
## **Additional exercise**
Demonstrate how this feature could be used to conduct an **atomic swap**.
  >**Atomic** means the transaction either succeeds completely, or fails completely. The swap transaction should exchange two values between two addresses in a single transaction.

Since we haven't yet studied native tokens, we'll simply swap value in ADA (later we'll be able to exchange tokens using the same logic).