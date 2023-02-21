# Jambhala: A Plutus Starter Kit
Jambhala brings a state of Zen to Plutus smart-contract development.
* **Keep your project in sync with the `plutus-apps` library:**
  * With Jambhala, we don't need to maintain a central clone of the [plutus-apps](https://github.com/input-output-hk/plutus-apps) repository and use its associated Nix shell as the entry point for development of every project.
  * **Why this matters:** relying on a central `plutus-apps` instance forces us to use the same revision for every project we develop. If we update our `plutus-apps` to use a more recent revision:
    * we need to adjust all of our individual projects' `cabal.project` files by hand to reflect any changes in dependencies
    * we risk breaking our older projects if the API of `plutus-apps` has changed
  * Even setting up a single project with this approach is painful, because in the absence of an up-to-date template, we must manually copy and adjust boilerplate from `plutus-apps` into our `cabal.project` file. This in turn quickly becomes outdated given the pace of Plutus development.
  * Jhambala uses [haskell.nix]() to provide a self-reliant development environment for each of your Plutus projects, which can be brought up to date with the current state of `plutus-apps` using a single command. No more wrangling of dependency boilerplate: just build your project environment and get to work, then bump `plutus-apps` for a specific project whenever you like.
  * Serve Haddock documentation for the specific `plutus-apps` revision your project is using with the `serve-docs` command.
* **Perform common Plutus tasks with simple commands:**
  * Serialize your contracts to `.plutus` files.
  * Compute validator hashes.
* **Minimize boilerplate in your contract files:**
  * `PlutusTx.Prelude` is enabled as prelude by default
  * Certain common language extensions are enabled by default

# Installation

**Requirements:**
  * This project uses the Nix package manager, Nix flakes, and IOG's `haskell.nix` infrastructure to build a fully-functioning and reproducible Plutus development environment.
  * Nix is only compatible with Unix-like operating systems, so you must be using a Linux distribution, MacOS, or WSL2 (Windows Subsystem for Linux) to install this project locally.
  * This project also assumes the use of `VS Code` as editor and `bash` as shell. Other tools will require alternative workflows that are not covered here.
  * This project is storage-intensive. We suggest you have at least `30GB` of free disk space before proceeding further.
  * **NOTE for MacOS users:** MacOS may ship with versions of `bash` and `grep` that are incompatible with this workflow. You should install `bash`/`grep` using Homebrew first before proceeding.
  * You'll need a fully-synced Cardano Node and the `cardano-cli` binary in order to submit example transactions to the blockchain.

1. **[Install Nix package manager](https://nixos.org/download.html)**
  * Follow the instructions for **multi-user installation** for your OS at the link above
  * Then follow the prompts in your terminal
  * When you are finished installing, close the terminal session and open a fresh one.

2. **Enable flakes & configure binary cache**
  * Edit `/etc/nix/nix.conf`: this requires root access to edit. Use a terminal-based editor like `nano` (i.e.):

      ```bash
      $ sudo nano /etc/nix/nix.conf
      ```

  * Modify the file following the instructions below:

    ```
    # Sample /etc/nix/nix.conf

    # Leave this line alone (may appear differently in your file)
    build-users-group = nixbld

    # Step 2a: Add this line to enable Flakes
    experimental-features = nix-command flakes

    # Step 2b: Set up binary cache (replace existing substituters and trusted-public-keys lines if present)

    substituters = https://cache.iog.io https://cache.nixos.org https://digitallyinduced.cachix.org https://all-hies.cachix.org https://cache.zw3rk.com
    trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE= all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k= loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk=

    # Step 2c: Avoid unwanted garbage collection with nix-direnv

    keep-outputs = true
    keep-derivations = true
    ```

  * **IMPORTANT!** You must restart the `nix-daemon` to apply the changes

    **Linux:**

      ```bash
      $ sudo systemctl restart nix-daemon
      ```

    **MacOS:** \
    Find the name of the `nix-daemon` service

      ```bash
      $ sudo launchctl list | grep nix
      ```

      Then stop and restart the service

      ```bash
      $ sudo launchctl stop <NAME>
      $ sudo launchctl start <NAME>
      ```

3. **Set up direnv**
  * This setup uses `direnv` to provide seamless loading of the Nix environment whenever you navigate into the project directory tree.
  * The `direnv` extension for VS Code integrates the environment with your editor, providing full IDE support for Plutus development.
  * [Install direnv](https://direnv.net/docs/installation.html) and follow the instructions to hook it into your shell.
  * When you load the project in VS Code for the first time, you will be prompted to install the [direnv extension](https://marketplace.visualstudio.com/items?itemName=cab404.vscode-direnv&ssr=false#review-details).

4. **Create your repository**
  * On this repository's Github page, select the green `Use this template` button and select `Create a new repository` to fork the template.
  * Clone your new repository, open the root directory in VS Code, and open a `bash` terminal session.
  * When you open the project in VS Code, you should be prompted to install some recommended extensions: `haskell`, `direnv` and `Nix IDE`.

5. **Build development environment**
  * Open the project root directory in a terminal window. You should see the following message:

    ```sh
    $ direnv: error /home/.../jambhala/.envrc is blocked. Run `direnv allow` to approve its content
    ```

    This is a security measure, since `.envrc` files can contain run arbitrary shell commands. Make sure you always trust the author of a project and inspect the contents of its `.envrc` file before running `direnv allow`.
    Enter `direnv allow` to approve the content.
  * It will take some time to set up the environment the first time.
  * Warning messages about "No index state specified" can be disregarded.
  * Some dependencies will need to be built from source, but if you see "building" for certain packages that should be downloadable from a binary cache (particularly GHC, the Linux kernel, and other non-Haskell related dependencies) or if you see any warning such as `warning: ignoring substitute`, this means your binary cache was not set up correctly and Nix is attempting to build packages from source that it should be fetching from a cache. Exit with `CTRL+c` and repeat **Step 2**, then try again. Make sure to restart the `nix-daemon`!
  * If you see any HTTP-related errors, it means the IOG binary cache is non-responsive. Wait a bit and try again later.
  * Once the build process completes, run `cabal build` to build the project dependencies.

    ```sh
    $ cabal build
    ```

    This will take some time to complete.

  * You can now start VS Code and use the `File > Open Folder...` menu option to load the starter kit.

# Usage

## Using the `jamb` CLI
Jambhala includes a simple command-line utility called `jamb`, which reduces boilerplate and provides a simple API for the following uses:

### **Listing Contracts**
You can run the following command to view the names of available contracts in your project, for use with other commands:

  ```sh
   $ jamb -l
  ```

### **Hashing Validators**
You can calculate the validator hash for any available contract like this:

  ```sh
  $ jamb -s CONTRACT
  ```

where `CONTRACT` is the name of the contract to hash.

### **Compiling Contracts to `.plutus` Files**
you can run the following command from the workspace terminal to write a contract to a `.plutus` file:

  ```sh
  $ jamb -w CONTRACT [FILENAME]
  ```

  where `CONTRACT` is the name of the contract to compile, and `[FILENAME]` is an optional file name (the contract name is used as the filename by default if no argument is given). When the command finishes, you should get a `compiled/CONTRACT.plutus` file that contains a JSON envelope of the UPLC code.

  ```json
  {
      "type": "PlutusScriptV2",
      "description": "",
      "cborHex": "5907c05907bd0100003232323232323232323..."
  }
  ```

  This file can be used to submit transactions on-chain.

### **Updating Plutus Dependencies:**
The non-Hackage dependencies in the `cabal.project` file are following those of the [plutus-apps](https://github.com/input-output-hk/plutus-apps) library, with `sha256` hashes calculated for each `source-repository-package` entry.

Since Nix flakes require pure inputs to guarantee reproducibility, and the content associated with a particular Git repository/tag can change, we need to hash any repositories we include in `cabal.project`. This means if we need to change any dependencies or add additional ones, we'll need to calculate new hashes and replace the existing ones.

`jamb` provides a utility to easily update `plutus-apps` to the most recent revision and adjust all related dependencies. Run the `jamb -u` command to pull the latest revision and generate a new `cabal.project` file.

  ```sh
  jamb -u
  ```

A backup of your existing `cabal.project` file will be created in the `backups/` directory in case you need to roll back the update.

**Manually updating dependencies:** while not recommended, if you need to change the revision of Plutus dependencies manually, you will need to calculate sha256 hashes for them. You can do this using the `nix-prefetch-git` utility, which has been provided with this project's Nix development environment.
  * Use the following command to calculate a  hash:

    ```
    $ nix-prefetch-git LOCATION TAG
    ```

  * Here is an example of how we'd calculate a hash for the `plutus-apps` dependency with tag `5dda0323ef30c92bfebd520ac8d4bc5a46580c5c`:

    ```bash
    $ nix-prefetch-git https://github.com/input-output-hk/plutus-apps.git 5dda0323ef30c92bfebd520ac8d4bc5a46580c5c

    ...

    git revision is 5dda0323ef30c92bfebd520ac8d4bc5a46580c5c
    path is /nix/store/mzjqwvfc2qmmvg9llskjyvkdph8hv4i4-plutus-apps-5dda032
    git human-readable version is -- none --
    Commit date is 2023-01-19 17:41:18 +0000
    hash is 05ggi69w2n0cnhfyifpa83aphq6avk0fd9zvxywn1scwxza85r1a
    {
      "url": "https://github.com/input-output-hk/plutus-apps.git",
      "rev": "5dda0323ef30c92bfebd520ac8d4bc5a46580c5c",
      "date": "2023-01-19T17:41:18+00:00",
      "path": "/nix/store/mzjqwvfc2qmmvg9llskjyvkdph8hv4i4-plutus-apps-5dda032",
      "sha256": "05ggi69w2n0cnhfyifpa83aphq6avk0fd9zvxywn1scwxza85r1a",
      "fetchLFS": false,
      "fetchSubmodules": false,
      "deepClone": false,
      "leaveDotGit": false
    }
    ```
  * The hash string must now be added as a comment prexied with `--sha256:` anywhere inside the `source-repository-package` stanza like so:

    ```
    source-repository-package
      type: git
      location: https://github.com/input-output-hk/plutus-apps.git
      tag: 5dda0323ef30c92bfebd520ac8d4bc5a46580c5c
      --sha256: 05ggi69w2n0cnhfyifpa83aphq6avk0fd9zvxywn1scwxza85r1a
    ```

## Writing Contracts

### **Read This First!**
Jambhala makes certain opinionated decisions in order to reduce boilerplate in writing Plutus contracts.

#### **Prelude & imports from `base`:**
  * Jambhala is configured to use `PlutusTx.Prelude` as its default prelude via a `mixin` in the `.cabal` file.
  * This eliminates the need to include both the `{#- LANGUAGE NoImplicitPrelude #-}` extension and `import PlutusTx.Prelude` in your contract files.
  * If you need to use regular Haskell functions that would normally be imported in the standard Prelude, import them from the specific `base` modules they reside in
  * For example, if you need to use the ***IO*** type in signatures and the `putStrLn` and `print` functions, import them from `System.IO` like so:

      ```haskell
      import System.IO ( IO, print, putStrLn )
      ```

#### **Language extensions:**
The following language extensions are enabled project-wide by Jambhala using the `default-extensions` setting in the `library` stanza of the `.cabal` file:

  ```
    default-extensions:
      -- Allows promotion of types to "kinds", enabling more expressive type-level programming (required for all Plutus contracts):
        DataKinds

      -- Allows construction of Text and other string-like values as string literals:
      , OverloadedStrings

      -- A syntactic convenience for working with record values (used by Jambhala's utilities):
      , RecordWildCards

      -- Required for all Plutus contracts to translate between Plutus and Haskell:
      , TemplateHaskell
  ```

Beyond these, the sample contracts include only the specific language extensions needed to compile their code. Keep in mind that Haskell language extensions are experimental modifications to compiler behavior, and should only be used when necessary with clear understanding of their purpose. It is better to add extensions incrementally as they become needed than to add a multitude of modifications to the compiler as boilerplate in every file.

#### **Sample contracts:**
The source code for the sample Plutus contracts live in the `src/Contracts` folder. `Contracts.hs` contains a ***Map*** data structure with the names and validators for the included sample contracts.

### **Creating a New Contract**
To create a new contract, create a new `.hs` file in the `src/Contracts` directory, and write a module declaration, i.e.:

  ```haskell
  module Contracts.MyContract where
  ```

**IMPORTANT:** you must stage any new contract files you create to git, to make them visible to Nix for compilation.

In the `jambhala.cabal` file, add your module name (i.e. `Contracts.MyContract`) to the `other-modules` section of the `library` stanza.

You're now ready to write your contract, which should contain a ***Validator*** value (by convention in the samples this is called `validator`). You can then make this the sole exported item in your module declaration (to avoid unnecessary exports) like so:

  ```haskell
  module Contracts.MyContract ( validator ) where
  ```

In `src/Contracts/Contracts.hs`, import your contract module as a qualified import, i.e.:

  ```haskell
  import qualified Contracts.MyContract as MyContract
  ```

Then add a new tuple entry to the `contracts` ***Map***, containing a name string for your contract and reference to its validator like so:

  ```haskell
    contracts :: Map String Validator
    contracts = M.fromList [
      ("simple", Simple.validator)
    , ("simple-typed", SimpleTyped.validator)

    ...

    , ("my-contract", MyContract.validator) -- example entry for new contract
    ]
  ```

Once your contract has been added to the map, it can now be written to a `.plutus` file or hashed using the `jamb` CLI.

## Serving `plutus-apps` docs
To serve docs for the specific revision of `plutus-apps` this project is using, open a new bash terminal from the project root directory and run the following command:

  ```sh
  $ serve-docs
  ```

The script will look up the specific `plutus-apps` revision hash from the `cabal.project` file, clone the `plutus-apps` repository (if it doesn't already exist) and checkout this revision, then launch a new `nix develop` shell and serve the docs at `http://0.0.0.0:8002/`.

To view the correct Haddock documentation for the revision you are using, open http://0.0.0.0:8002/haddock in your browser.

## TODO: Troubleshooting

For assistance or bug reporting, file an Issue or email `ian.burzynski@emurgo.io`.