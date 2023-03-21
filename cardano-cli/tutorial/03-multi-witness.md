# **`cardano-cli` Exercise 03: Multi-witness Transactions**
In this exercise we'll submit a more complex transaction involving two input UTXOs from different users (`alice` and `bob`) to transfer funds to a third user (`charlie`).

This transaction requires use of the `witness` and `assemble` subcommands of the `transaction` command, which are explained below. The rest of the exercise is left to you to test your understanding of the UTXO model and what you've learned so far about `cardano-cli`, although the high-level steps are provided to guide you.

### **`witness` subcommand**
Jambhala provides a helper script called `tx-witness`, which contains the following command:

```sh
# cardano-cli/tx-witness

$ cardano-cli transaction witness \
--signing-key-file "$KEYS_PATH/$1.skey" \
--tx-body-file "$TX_PATH/$2.raw" \
--out-file "$TX_PATH/$2-$1.witness" \
$NET
```

Run it by providing a signer and transaction name, i.e.:

```
$ tx-witness alice multiwit
```

### **`assemble` subcommand**
The `transaction assemble` command creates a transaction by combining its witness(es) with the transaction body. It serves as the equivalent to `transaction sign` for transactions requiring multiple signatures.

```sh
$ cardano-cli transaction assemble \
--tx-body-file $TX_PATH/multiwit.raw \
--witness-file $TX_PATH/multiwit-alice.witness \
--witness-file $TX_PATH/multiwit-bob.witness \
--out-file $TX_PATH/multiwit.signed
```

## **Steps for building a multi-witness transaction**
1. Create a key-pair and address for `charlie`.
2. Find one UTXO to consume from `alice` and save it to a temporary variable `U1` (with its value assigned in the format `TxHash#TxIx`). Do the same for a UTXO from `bob`, saving to a temporary variable `U2`.
3. Build the transaction body using the `transaction build` command, including the following:
  * `U1` and `U2` as inputs
  * `charlie`'s address and the transfer amount as the output
  * `alice`'s address as the change address
  * A witness override
4. Create witness files for both senders.
5. Assemble the transaction.
6. Submit the transaction.

## **Additional exercise**
Demonstrate how this feature could be used to conduct an **atomic swap**.
  >**Atomic** means the transaction either succeeds completely, or fails completely. The swap transaction should exchange two values between two addresses in a single transaction.

Since we haven't yet studied native tokens, we'll simply swap value in ADA (later we'll be able to exchange tokens using the same logic).