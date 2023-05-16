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

>If you're using an existing installation of `cardano-node`, see **[Configuring an existing installation](#existing)** for an example alias definition that you can add to your shell dotfile.

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

cardano-cli query tip
```

This script uses `cardano-cli`'s `query` command to query information about the `tip` of the chain.

***
## **<a id="existing"></a> Configuring an existing installation**
***
If you've already installed `cardano-node` and `cardano-cli`, and want to use your existing installation with the Jambhala scripts and `cardano-cli` tutorial, simply modify the `.env` file in your project's root directory as follows.

* `CARDANO_NODE_NETWORK_ID`: set this value to correspond to the network you're currently using (`1` for `preprod`, `2` for `preview`, `mainnet` for `mainnet`).

>Run `direnv allow` in your terminal session before proceeding if you make any changes to `.env`.

By default the integrated terminal in Jambhala's editor uses `bash`, which means to use your pre-installed `cardano-node`/`cardano-cli` you must also export the `CARDANO_NODE_SOCKET_PATH` environment variable in your `~/.bashrc` file with the location where your `node.socket` file is located.

An example export:

```sh
# ~/.bashrc sample

export CARDANO_NODE_SOCKET_PATH=/home/ian/cardano/node.socket
```


Additionally, if you use any custom alias to start your node, you may also want to add this to your `.bashrc` file so you can use it within the integrated terminal in Jambhala's editor. An example `.bashrc` alias to run the node on the `preview` testnet:

```sh
# ~/.bashrc sample

alias preview-node='cardano-node run --topology /home/ian/cardano/preview/config/topology.json --database-path /home/ian/cardano/preview/db --socket-path $CARDANO_NODE_SOCKET_PATH --port 1337 --config /home/ian/cardano/preview/config/config.json'
```

>Open a fresh terminal session to source your changes before proceeding.