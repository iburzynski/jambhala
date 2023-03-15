# Installing `cardano-node` & `cardano-cli`

Jambhala provides an easy installation script called `install-node`, which installs `cardano-node` and `cardano-cli` using Nix and configures the node so it's ready for use:

```sh
$ install-node
```

>You can safely ignore any Nix-related warnings that occur during the installation process.

## Custom Installation
The `.envrc` file provides several environment variables, which can be changed to modify the installation process:

* By default, `cardano-node` and `cardano-cli` will be installed at `$HOME/cardano-src`. To use a different directory, change the `CARDANO_SRC_PATH` variable in `.envrc`.

* The default directory where the node database and configuration files are created is `$HOME/cardano/testnet`. To use a different directory, change the `CARDANO_PATH` variable in `.envrc`.

* By default, the node and CLI will be configured to use the `preview` testnet. To use the `preprod` testnet instead, change the `NETWORK_MAGIC` variable in `.envrc` to `1`.

>Run `direnv allow` in your terminal session before proceeding if you make any changes to `.envrc`.

## Syncing the Node
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

## `cardano-cli` Tutorial
Jambhala provides a tutorial with guided exercises for building/submitting various types of transactions using `cardano-cli`. The tutorial begins with the simplest possible transaction and progresses through increasingly complex examples.

Some prewritten shell scripts have been provided to make the process less laborious, but you should always inspect the contents of these scripts before using them to understand the structure of the `cardano-cli` commands.

For example, if we inspect the contents of the `tip` script at `cardano-cli/tip` we'll see the following:

```sh
cardano-cli query tip $TESTNET
```

This script uses `cardano-cli`'s `query` command to query information about the `tip` of the chain.

`$TESTNET` is an environment variable set in the `.envrc` file. It has a value of `"--testnet-magic ${NETWORK_MAGIC}"`, where `${NETWORK_MAGIC}` is a number indicating which testnet we're using (`1` for `preprod`, `2` for `preview`). By default this is set to `2`.