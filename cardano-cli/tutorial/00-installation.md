# **Installing `cardano-node` & `cardano-cli`**

Jambhala provides an easy installation script called `install-node`, which installs `cardano-node` and `cardano-cli` using Nix and configures the node so it's ready for use. If you haven't already installed `cardano-node`, this is the quickest and easiest way to get a node running on your system.

```sh
$ install-node
```

>You can safely ignore any Nix-related warnings that occur during the installation process.

* To configure an existing installation of `cardano-node`, see **[Configuring an existing installation](#existing)**.
* To customize the installation process, see **[Custom installation](#custom)**.

***
## **<a id="syncing"></a> Starting and syncing the node**
***
Use the `run-node` script to start `cardano-node`:

```sh
$ run-node
```

It's normal for the node to encounter occasional errors, which it will recover from and continue running. To tell if your node is working properly, look for `Chain extended` log entries with the following format:

```sh
Chain extended, new tip: c472036b83c119b875e3fc230435b741598677ffa45ea3ad8ad9cda3f70a872d at slot 12227931
```

Your node must be `100%` synced before you can proceed. Use the `tip` script to query the sync progress:

```sh
$ tip
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
cardano-cli query tip $NET
```

This script uses `cardano-cli`'s `query` command to query information about the `tip` of the chain.

`NET` is an environment variable set in the `.envrc` file, which is used to specify the network we're using. By default this is set to the `preview` testnet.

***
## **<a id="existing"></a> Configuring an existing installation**
***
If you've already installed `cardano-node` and `cardano-cli`, you'll need to modify certain environment variables in the `.envrc` file so Jambhala can communicate with your node:

* `CARDANO_PATH`: replace this value with the path to the directory where your node files (database, socket, etc.) are stored.
* `DB_PATH`: make sure this value is correct (by default it's a directory called `db` inside whichever directory `CARDANO_PATH`is set to).
* `CONFIG_PATH`: replace this value with the path to the directory where your node configuration `.json` files are stored (`config.json`, `topology.json`, etc.).
* `CARDANO_NODE_SOCKET_PATH`: make sure this value is correct (by default it's a file `node.socket` inside whichever directory `CARDANO_PATH`is set to).
* `NETWORK_MAGIC`: make sure this value matches the testnet you are using (`1` for `preprod`, `2` for `preview`).

>Run `direnv allow` in your terminal session before proceeding if you make any changes to `.envrc`.

***
## **<a id="custom"></a> Custom installation**
***
The`.envrc` file provides several environment variables, which can be changed to modify the installation process.

* By default, `cardano-node` and `cardano-cli` will be installed at `$HOME/cardano-src`. To use a different directory, change the `CARDANO_SRC_PATH` variable in `.envrc`.

* The default directory where the node database and configuration files are created is `$HOME/cardano/testnet`. To use a different directory, change the `CARDANO_PATH` variable in `.envrc`.

* By default, the node and CLI will be configured to use the `preview` testnet. To use the `preprod` testnet instead, change the `NETWORK_MAGIC` variable in `.envrc` to `1`.

>Run `direnv allow` in your terminal session before proceeding if you make any changes to `.envrc`.