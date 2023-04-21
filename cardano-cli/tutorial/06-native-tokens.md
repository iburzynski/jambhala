# **`cardano-cli` Exercise 06: Native Tokens**
In this exercise we'll learn how to **[mint](#🪙-mint-native-tokens)**, **[send](#📨-send-native-tokens)**, and **[burn](#🔥-burn-tokens)** native tokens on Cardano.

***
## **🪙 Mint native tokens**
Minting native tokens with `cardano-cli` involves the following steps:

1. **[Create minting policy](#create-minting-policy)**
2. **[Prepare token representation](#prepare-token-representation)**
3. **[Draft the minting transaction](#draft-the-minting-transaction)**
4. **[Build the minting transaction](#build-the-minting-transaction)**
5. **[Sign and submit the minting transaction](#sign-and-submit-the-minting-transaction)**

***
### **Create minting policy**
A minting policy determines the conditions under which new tokens can be minted. Only those in possession of the key(s) specified in the policy can mint or burn the token.

In this example, we'll create a new address for our minting policy, but it's possible to use an existing payment address as well.

Use the `key-gen` script to generate a new key-pair/address for our minting policy:

```sh
key-gen native-policy
```

Now create a file called `native.script` in the `assets/scripts/native` directory and paste the following:

```json
{
  "keyHash": "<KEY-HASH>",
  "type": "sig"
}
```

Now use the `key-hash` script to get the key-hash for `native-policy`:

```sh
key-hash native-policy
```

Copy the key-hash value from the terminal and replace the `<KEY-HASH>` placeholder in `native.script`.

We now have a simple script file that defines the policy verification key as a witness to sign the minting transaction. This simple policy includes no further constraints, such as token locking or requiring additional signatures to successfully submit a transaction.

***

### **Prepare token representation**
A token representation consists of the following: `<POLICY_ID>.<TOKEN_NAME>`.

The **policy ID** for a Cardano asset is computed by applying a hash function to the policy script.

**Token names** must be `base16` (hexadecimal) encoded.

Jambhala provides a `token` helper script to easily prepare a token representation, which we can save to a variable for later use:

```sh
T=$(token native jambcoin)

echo $T
d1298d07e8fefa6f65c8f01fb8a43e94e086729583dc3b1121dc75cf.6a616d62636f696e
```

The script takes the name of an existing policy script (`$1`) and the desired name for the token (`$2`). It performs two steps: **getting the Policy ID** of the script, and **encoding the token name**. This is achieved via two subscripts, `policyid` and `token-name`, whose results are then concatenated with a period.

```sh
# cardano-cli/token

echo -n $(policyid $1).$(token-name $2)
```

The two steps and their respective helper subscripts are explained below in greater detail.

#### **Get policy ID**
The `policyid` subscript takes the policy name as an argument and runs the `transaction policyid` command, which hashes the policy script and outputs its ID:

```sh
# cardano-cli/policyid

cardano-cli transaction policyid \
--script-file $NATIVE_SCRIPTS_PATH/$1.script
```

#### **Encode token name**
To produce the desired token name in hexadecimal format, Jambhala provides a `token-name` script, which uses the `xxd` tool to encode a token name provided as argument:

```sh
# cardano-cli/token-name

echo -n $1 | xxd -ps | tr -d '\n'
```

Try running the script on a fictional token name to see the hexadecimal result:

```sh
token-name jambcoin
6a616d62636f696e
```

***

### **Draft the minting transaction**
Query `alice`'s UTXOs and save a UTXO to a temporary variable `U` in the format `<TXHash>#<TxIX>` as in previous exercises.

Assign a variable `INPUT_AMT` to the amount of the UTXO.

Assign a variable `Q` to the quantity of native tokens the transaction will mint (here 1 million):

```sh
Q=1000000
```

```sh
cardano-cli transaction build-raw \
--tx-in $U \
--tx-out "$(addr alice)+0+$Q $T" \
--mint "$Q $T" \
--mint-script-file $NATIVE_SCRIPTS_PATH/native.script \
--fee 0 \
--out-file $TX_PATH/native-mint.draft
```

***

### **Build the minting transaction**
Calculate the transaction fee, storing it in a temporary variable `FEE`:

```sh
FEE=$(min-fee native-mint -i 1 -o 1 -w 2)
```

>Note that the transaction has a `witness-count` of 2: one signature will be provided by the `native-policy` and the other by the user who is minting the tokens (`alice`).

Then calculate the remaining balance after paying the fee by subtracting `FEE` from `INPUT_AMT`, and assign it to a variable `BALANCE`:

```sh
BALANCE=$(expr $INPUT_AMT - $FEE)
```

We now have all the information needed to build the transaction, replacing the placeholder values from the draft with the variables:

```sh
cardano-cli transaction build-raw \
--tx-in $U \
--tx-out "$(addr alice)+$BALANCE+$Q $T" \
--mint "$Q $T" \
--mint-script-file $NATIVE_SCRIPTS_PATH/native.script \
--fee $FEE \
--out-file $TX_PATH/native-mint.raw
```

***

### **Sign and submit the minting transaction**
For this transaction we won't create witness files and use `transaction assemble` to produce the signed transaction: we'll assume the person minting the tokens (`alice`) is in possession of the `native-policy` keys, and simply use the `tx-sign` script with multiple user arguments:

```sh
tx-sign native-mint native-policy alice
```

Now submit the minting transaction. After a few minutes, you can query the UTXOs of the recipient to confirm the receipt of the minted tokens.

***

## **📨 Send native tokens**
We'll now build a transaction to send one of the newly minted tokens from `alice` to `bob`.

The process for sending native tokens is:
1. **[Calculate the minimum amount of accompanying ADA](#calculate-the-minimum-amount-of-accompanying-ada)**
2. **[Draft the token transfer transaction](#draft-the-token-transfer-transaction)**
3. **[Build, sign and submit the token transfer transaction](#build-sign-and-submit-the-token-transfer-transaction)**

***

### **Calculate the minimum amount of accompanying ADA**
In order to prevent spam, Cardano transactions cannot consist solely of native assets: each transaction must also include a minimum amount of ADA. The minimum ADA amount varies based on the specifics of the transaction, but is typically `1 - 2` ADA.

>Read the official docs on **[Minimum ada value requirement](https://docs.cardano.org/native-tokens/minimum-ada-value-requirement)** for more information

We'll need to determine how much ADA our transaction requires and add this to our transaction outputs when we draft the transaction.

Jambhala provides a `min-ada` script that takes a user name (`$1`), an asset quantity (`$2`) and an asset (`$3`) and calculates the minimum ADA required for the transaction output:

```sh
# cardano-cli/min-ada

cardano-cli transaction calculate-min-required-utxo \
--protocol-params-file $PARAMS_PATH \
--tx-out $(addr $1)+"$2 $3" | cut -d' ' -f2
```

The script runs the `calculate-min-required-utxo` subcommand with the three input arguments and trims the result to return the lovelace value as an integer.

We'll calculate the minimum amount of ADA required for `alice` to send `1` unit of our asset `$T`. Assign the output of this command to a temporary variable `MIN_ADA`:

```sh
MIN_ADA=$(min-ada alice 1 $T)
```

>Run `echo $MIN_ADA` to see how much ADA your transaction requires.

***

### **Draft the token transfer transaction**
To draft the transaction, we start by locating the UTXO output from the minting transaction, which contains both ADA and our native token:

```sh
utxos alice
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
8580677d9f4589998dce7d7c2c5a0509518640a6eeae67280853df44cd73e445     0        8748621168 lovelace + 1000000 866a902c6237c0c6fb472d258a1ad036c59b756066bf8de5df29aaa3.6a616d62636f696e + TxOutDatumNone
```

Assign the UTXO's `TxHash` and `TxIx` as usual to a variable `U` in the format `<TxHash>#<TxIx>`.

```sh
cardano-cli transaction build-raw \
--tx-in $U \
--tx-out $(addr bob)+$MIN_ADA+"1 $T" \
--tx-out $(addr alice)+0+"999999 $T" \
--fee 0 \
--out-file $TX_PATH/native-transfer.draft
```

Calculate the minimum fee for the token transfer. The transaction has `1` input, `2` outputs, and `1` witness:

```sh
FEE=$(min-fee native-transfer -i 1 -o 2 -w 1)
```

Now calculate the change balance by subtracting the `MIN_ADA` quantity and `FEE` from the `INPUT_AMT`:

```sh
BALANCE=$(expr $INPUT_AMT - $MIN_ADA - $FEE)
```

***

### **Build, sign and submit the token transfer transaction**
Now build the final transaction:

```sh
cardano-cli transaction build-raw \
--tx-in $U \
--tx-out $(addr bob)+$MIN_ADA+"1 $T" \
--tx-out $(addr alice)+$BALANCE+"999999 $T" \
--fee $FEE \
--out-file $TX_PATH/native-transfer.raw
```

Sign the transaction for `alice` and submit it.

***

## **🔥 Burn tokens**
In this transaction we'll have `alice` burn `10%` (`100000`) of the native tokens she minted.

Start by locating `alice`'s UTXO from the `native-transfer` transaction and saving its `TxHash` and `TxIx` to the variable `U`. Save its ADA amount to the variable `INPUT_AMT`.

***

### **Draft the burn transaction**
A transaction that burns native tokens is constructed similarly to one that mints tokens, but with a negative quantity provided to the `--mint` option.

If `alice` minted `1000000` tokens, sent `1` token to `bob`, and now intends to burn `100000` tokens, she should receive `899999` leftover tokens in the `tx-out`.

We create the draft for our `native-burn` transaction accordingly using `build-raw`, providing the burn amount and remaining token quantities (with zeros used as placeholders for the change in ADA and fee).

```sh
cardano-cli transaction build-raw \
--tx-in $U \
--tx-out "$(addr alice)+0+899999 $T" \
--mint "-100000 $T" \
--mint-script-file $NATIVE_SCRIPTS_PATH/native.script \
--fee 0 \
--out-file $TX_PATH/native-burn.draft
```

***

### **Build, sign and submit the burn transaction**
Calculate the minimum fee for the transaction (refer to how we calculated the fee for the minting transaction above if needed), assigning it to `FEE`. Then compute `alice`'s change as usual, assigning the amount to `BALANCE`.

Then you can build the final transaction:

```sh
cardano-cli transaction build-raw \
--tx-in $U \
--tx-out "$(addr alice)+$BALANCE+899999 $T" \
--mint "-100000 $T" \
--mint-script-file $NATIVE_SCRIPTS_PATH/native.script \
--fee $FEE \
--out-file $TX_PATH/native-burn.raw
```

Sign the transaction for `native-policy` and `alice` and submit.

Wait a few minutes, then query Alice's UTXOs to confirm that the tokens were burned.