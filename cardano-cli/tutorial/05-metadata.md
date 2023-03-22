Since the Shelley era, Cardano allows user-defined metadata to be associated with transactions.

## **Metadata structure
Transaction metadata is prepared in JSON format. The top level is a map from **metadata keys** to **metadata values**.

**Metadata keys** are **integers** in the range `0` to `2^64 - 1`.

**Metadata values** can be one of three simple types or two compound types.

**Simple types:**
* Integers in the range `-(2^64 - 1)` to `2^64 - 1`
* Strings (UTF-8 encoded)
* Bytestrings (hex-encoded)

>Strings and bytestrings have a maximum length of 64 bytes. Try **[this tool](https://ethproductions.github.io/bytes/?e=utf-8)** to count the bytes in a string.

**Compound types:**
* Lists of **metadata values**
* Mappings from **metadata values** to **metadata values**

>Lists and maps need not necessarily contain the same type of metadata value in each element.

## **Create metadata**
We'll now construct a simple piece of metadata to store on-chain. Create a file called `metadata.json` in the `assets/tx` directory.

Create a file with the following format, replacing `"20230321"` with today's date, and the values of the `"name"` and `"message"` keys with your name and a message of your choice:

```json
{
  "20230321": {
    "name": "Alice",
    "message": "Om Dzambhala Dzalentraye Svaha"
  }
}
```

Create a temporary `LABEL` variable for the date key you selected, so you can refer to it later:

```sh
LABEL=20230321
```

## **Build a transaction with metadata**
To build a transaction that includes metadata, we'll use the `transaction build-raw` command that we practiced in **[Exercise 2](./02-build-raw.md)**.

Start by drafting the transaction. Instead of transfering funds between two users as we've done in previous examples, we can simply construct a single-user transaction that pays the transaction fee to publish the metadata on-chain, and returns the change to the original address:

```sh
$ cardano-cli transaction build-raw \
--tx-in $U \
--tx-out $(addr alice)+0 \
--fee 0 \
--metadata-json-file $TX_PATH/metadata.json \
--out-file $TX_PATH/metadata.draft
```

Note the inclusion of the `--metadata-json-file` option with the filepath to the metadata file we created.

Follow the remaining steps from **[Exercise 2](./02-build-raw.md)** to submit the transaction.

>Note that our transaction has only one `tx-out`: you'll need to adjust the `--tx-out-count` option to the `transaction calculate-min-fee` command accordingly.

## **Creating and storing a Blockfrost Project ID
Visit **[blockfrost.io](https://blockfrost.io/)** and click the blue **`BUILD APPS NOW`** button in the top right corner to create a free account.

Once your account is created, you'll be taken to the **`DASHBOARD`**. Click **`+ADD PROJECT`** to create a new project.

Enter anything you like in the **`Project name`** field (i.e. "jambhala").

In the **`Network`** dropdown, select the Cardano testnet your project is using (**`Cardano preview`** or **`Cardano preprod`**). By default Jambhala is configured to use **`Cardano preview`**. You can run the following command at any time to confirm which testnet your project is using:

```sh
$ echo $TESTNET_NAME
```

Once your project is created, you'll be taken to the project's page. Find the **`PROJECT ID`** field and click the copy icon to copy your

Open the `.env` file in the root directory of your project and type the following line:

```sh
BLOCKFROST_ID=
```

Then paste your Blockfrost project ID immediately after the equals sign. Save the file and run `direnv allow` in your terminal session to make the new variable available.

Run the following command to confirm that the variable is available:

```sh
$ echo $BLOCKFROST_ID
```

## **Querying transaction metadata with Blockfrost

```sh
$ curl -H "project_id: $BLOCKFROST_ID" "https://cardano-$TESTNET_NAME.blockfrost.io/api/v0/metadata/txs/labels/$LABEL" | jq
```