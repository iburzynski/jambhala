# **Installing `cardano-node` & `cardano-cli`**

The Jambhala setup wizard (`jsetup`) prompts you to install `cardano-node` and `cardano-cli` with the **[Cardano EZ-Installer](https://github.com/iburzynski/cardano-ez-installer)**. If you did not accept this prompt during the setup process, you can run the `install-node` script at any time to install and configure the node/cli:

```sh
install-node
```

* If you haven't already installed `cardano-node`, this is the quickest and easiest way to get a node running on your system.

* If you encounter any errors during installation, refer to the **Troubleshooting** section of the **[Cardano EZ-Installer README](/cardano-ez-installer/README.md#troubleshooting)** for help.

**To configure an existing installation of `cardano-node`, see [Configuring an existing installation](#existing)**.

***
## **<a id="syncing"></a> Starting and syncing the node**
***
Use the appropriate alias in your terminal to start `cardano-node`, i.e.:
- `preprod-node` for preprod testnet
- `preview-node` for preview testnet

>If you're using an existing installation of `cardano-node`, see **[Configuring an existing installation](#existing)** for an example alias definition that you can add to your `.bashrc` file.

It's normal for the node to encounter occasional errors, which it will recover from and continue running. To tell if your node is working properly, look for `Chain extended` log entries with the following format:

```sh
Chain extended, new tip: c472036b83c119b875e3fc230435b741598677ffa45ea3ad8ad9cda3f70a872d at slot 12227931
```

Your node must be `100%` synced before you can proceed. Use the `tip` script to query the sync progress:

```sh
tip
{
    "block": 546242,
    "epoch": 141,
    "era": "Babbage",
    "hash": "7ee471e26ed927ae463d386cdd322fd7f3afb18d0fef462255ce2a2f221d7112",
    "slot": 12227857,
    "syncProgress": "100.00"
}
```
***
## **`cardano-cli` tutorial**
***
Jambhala provides a tutorial with guided exercises for building/submitting various types of transactions using `cardano-cli`. The tutorial begins with the simplest possible transaction and progresses through increasingly complex examples.

Some prewritten shell scripts have been provided to make the process less laborious, but you should always inspect the contents of these scripts before using them to understand the structure of the `cardano-cli` commands.

For example, if we inspect the contents of the `tip` script at `cardano-cli/tip` we'll see the following:

```sh
# cardano-cli/tip

cardano-cli query tip $NET
```

This script uses `cardano-cli`'s `query` command to query information about the `tip` of the chain.

`NET` is an environment variable set in the `.envrc` file, which is used to specify the network we're using. By default this is set to the `preview` testnet.

***
## **<a id="existing"></a> Configuring an existing installation**
***
If you've already installed `cardano-node` and `cardano-cli`, you'll need to modify certain environment variables in the `.env` file and adjust your directory structure so Jambhala can communicate with your node:

* `NETWORK`: the value should correspond to the type of network your node is running on (`testnet` or `mainnet`).
* `NETWORK_MAGIC`: make sure this value matches the testnet your node is configured to use (`1` for `preprod`, `2` for `preview`).
* `CARDANO_PATH`: replace this value with the base path to the directory where your node files (database, socket, etc.) are stored. To work correctly with Jambhala:
    * Your `CARDANO_PATH` directory must contain a subdirectory with a name corresponding to the `NETWORK` your node is running on (`testnet` or `mainnet`). For `testnet`, this subdirectory must contain an additional subdirectory with a name corresponding to the specific testnet your node is using (`preprod` or `preview`).
    * Your database should be located in a subdirectory of this directory called `db` (i.e. `/$CARDANO_PATH/testnet/preview/db`).
    * Your configuration `.json` files should be in a subdirectory of this directory called `config` (i.e. `/$CARDANO_PATH/testnet/preview/config`)).

You can rename/move files in your `CARDANO_PATH` directory as needed to conform with the requirements above.

>Run `direnv allow` in your terminal session before proceeding if you make any changes to `.env`.

If you have any alias inside your `~/.bashrc` file that you use to start your node, you'll also need to update this to conform with the requirements. An example `.bashrc` alias to run the node on the `preview` testnet:

```sh
# ~/.bashrc sample

alias preview-node='cardano-node run --topology /home/ian/cardano/testnet/preview/config/topology.json --database-path /home/ian/cardano/testnet/preview/db --socket-path /home/ian/cardano/testnet/preview/node.socket --port 1337 --config /home/ian/cardano/testnet/preview/config/config.json'
```

You may also need to update your `CARDANO_NODE_SOCKET_PATH` variable if you have one defined in your `.bashrc` file:

```sh
# ~/.bashrc sample

export CARDANO_NODE_SOCKET_PATH='/home/ian/cardano/testnet/preview/node.socket'

```

This variable is set for you dynamically when you're inside of the Jambhala environment, but it must also be set in your `.bashrc` file to use `cardano-node` and `cardano-cli` outside of the environment.

>Run `source ~/.bashrc` in your terminal session before proceeding if you make any changes to `.bashrc`.