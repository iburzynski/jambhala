# **`cardano-cli` Exercise 05: Transaction Metadata**
Since the Shelley era, Cardano allows user-defined metadata to be associated with transactions. In this exercise we'll practice building transactions containing arbitrary metadata and store a message on the blockchain forever. We'll also learn how to use Blockfrost's API to view transactions containing related metadata.

## **Metadata structure**
Transaction metadata is prepared in JSON format. The top level is a map from **metadata keys** to **metadata values**.

* **Metadata keys** are **integers** in the range `0` to `2^64 - 1`. Metadata keys are also referred to as **labels**.

* **Metadata values** can be one of three simple types or two compound types.

  * **Simple types:**
    * **Integers** in the range `-(2^64 - 1)` to `2^64 - 1`
    * **Strings** (UTF-8 encoded)
    * **Bytestrings** (hex-encoded)

    >Strings and bytestrings have a maximum length of 64 bytes. Try **[this tool](https://ethproductions.github.io/bytes/?e=utf-8)** to count the bytes in a string.

  * **Compound types:**
    * **Lists** of **metadata values**
    * **Mappings** from **metadata values** to **metadata values**

    >Lists and maps need not necessarily contain the same type of metadata value in each element.

## **Create metadata**
We'll now construct a simple piece of metadata to store on-chain. Create a file called `metadata.json` in the `assets/tx` directory.

Create a file with the following format, replacing the label `"20230321"` with today's date, and the values of the `"name"` and `"message"` keys with your name and a message of your choice:

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
cardano-cli transaction build-raw \
--tx-in $U \
--tx-out $(addr alice)+0 \
--fee 0 \
--metadata-json-file $TX_PATH/metadata.json \
--out-file $TX_PATH/metadata.draft
```

Note the inclusion of the `--metadata-json-file` option with the filepath to the metadata file we created.

Follow the remaining steps from **[Exercise 2](./02-build-raw.md)** to submit the transaction.

>Note that our transaction has only one `tx-out`: you'll need to adjust the `--tx-out-count` option to the `transaction calculate-min-fee` command accordingly. Our transaction also doesn't include a **validity interval**, so you don't need to calculate an interval and include the `--invalid-hereafter` option.

## **View your transaction's metadata**
You can run the `tx-hash` script for your `metadata` transaction and search for it on **[preview.cardanoscan.io](https://preview.cardanoscan.io/)** to view the transaction metadata (use **[preprod.cardanoscan.io](https://preprod.cardanoscan.io)** instead if using the `preprod` testnet).

```sh
tx-hash metadata
```

On `cardanoscan.io`, beneath the **Transaction Details** section you'll see a section with three options: `Summary`, `UTXOs`, and `Metadata`. Click the `Metadata` tab to view your transaction's metadata.

## **View transactions by metadata label**
We can query **[Blockfrost](https://blockfrost.io/)**'s API to view transactions submitted with a particular label. You'll need a Blockfrost account and **Project ID** (API key) to submit requests.

### **Create and store a Blockfrost Project ID**
If you haven't done so already, visit **[blockfrost.io](https://blockfrost.io/)** and click the blue **`BUILD APPS NOW`** button in the top right corner to create a free account.

Once your account is created, you'll be taken to the **`DASHBOARD`**. Click **`+ADD PROJECT`** to create a new project.

Enter anything you like in the **`Project name`** field (i.e. "jambhala").

In the **`Network`** dropdown, select the Cardano testnet your project is using (**`Cardano preview`** or **`Cardano preprod`**). By default Jambhala is configured to use **`Cardano preview`**. You can run the following command at any time to confirm which testnet your project is using:

```sh
echo $TESTNET_NAME
```

Once your project is created, you'll be taken to the project's page. Find the **`PROJECT ID`** field and click the copy icon to copy your ID.

Open the `.env` file in the root directory of your project and replace the placeholder value for the corresponding variable with your project ID (paste it immediately after the equals sign):

```sh
BLOCKFROST_PROJECT_ID_PREVIEW=previewProjectId
```

or...

```sh
BLOCKFROST_PROJECT_ID_PREPROD=preprodProjectId
```

Save the file and run `direnv allow` in your terminal session to make the new variable available.

Run the following command to confirm that the variable is available:

```sh
echo $BLOCKFROST_PROJECT_ID_PREVIEW
```

or...

```sh
echo $BLOCKFROST_PROJECT_ID_PREPROD
```

### **Query transaction metadata by label**
You can now view transactions containing a particular metadata label by running the command below:

```sh
curl -H "project_id: $BLOCKFROST_PROJECT_ID_PREVIEW" "https://cardano-$TESTNET_NAME.blockfrost.io/api/v0/metadata/txs/labels/$LABEL" | jq
```

You should see your transaction and its metadata appear in the results, as well as any other transactions submitted with the same label. You can replace the `$LABEL` variable in the URL to view transactions with a different label.