![image](jamb_logo.png)

***

# **A Full-Featured Cardano Development Suite**

* **[Features](#features)**
* **[Installation](#installation)**
  * **[Requirements](#0-requirements)**
  * **[Install Nix](#1-install-nix)**
  * **[Configure `nix.conf`](#2-configure-nixconf)**
  * **[Create your repository](#3-create-your-repository)**
  * **[Clone repository & test readiness](#4-clone-repository--test-readiness)**
  * **[Build environment & set up project](#5-build-environment--set-up-project)**
  * **[Start coding](#6-start-coding)**
* **[Using the `jamb` CLI](#using-the-jamb-cli)**
* **[Writing contracts](#writing-contracts)**
  * **[Creating a contract](#creating-a-contract)**
    * **[Writing emulator tests](#writing-emulator-tests)**
    * **[Using the `jamb` CLI](#using-the-jamb-cli)**
  * **[Using GHCi](#using-ghci)**
  * **[Serving `plutus-apps` docs](#serving-plutus-apps-docs)**
* **[Updating Jambhala](#updating-jambhala)**
* **[Updating Plutus dependencies](#updating-plutus-dependencies)**
  * **[Set `plutus-apps` to a specific commit/tag](#set-plutus-apps-to-a-specific-committag)**
  * **[Restoring a previous version](#restoring-a-previous-version)**
  * **[Manually updating dependencies](#manually-updating-dependencies)**
* **[Troubleshooting](#troubleshooting)**

***
# **Features**
Jambhala brings Cardano development nirvana by presenting five jewels:

💎 #1: **Painless installation of Cardano tooling**
  - Only `git`, and **[`nix`](#1-install-nix)** are required to get started.
  - Jambhala's **Readiness Test** confirms system readiness before proceeding with installation, preventing wastage of time and resources on failed builds.
  - Jambhala's setup wizard handles everything else, including easy installation of `cardano-node` and `cardano-cli` using **[Cardano EZ-Installer](https://github.com/iburzynski/cardano-ez-installer)**.
  - **[plutus-apps](https://github.com/input-output-hk/plutus-apps)** is installed internally to your project, so you don't need to maintain a central clone and use its associated Nix shell as the entry point for your projects (See **Jewel #4** below).
  - A preconfigured VS Codium editor is included, allowing you to start coding immediately (you can still use your editor of choice if preferred).

💎 #2: **Minimize contract boilerplate**
  - `PlutusTx.Prelude` is enabled as prelude project-wide by default via mixin
  - Certain common Haskell language extensions are enabled by default
  - Common Plutus and Haskell types and functions are re-exported from their respective modules by `Jambhala.Plutus` and `Jambhala.Haskell`, so you don't need to keep track of messy import boilerplate. You can always import Plutus or Haskell modules explicitly if we prefer.
  - `Jambhala.Utils` provides common utility functions which are consumed by the `jamb` CLI, to avoid contract clutter.

💎 #3: **Perform common `cardano-cli` and Plutus tasks with simple commands**
  - Jambhala includes **[Cardano CLI Guru](https://github.com/iburzynski/cardano-cli-guru)**, which provides utility scripts for common `cardano-cli` tasks that can be run as terminal commands.
  - A Haskell-based executable CLI called `jamb` lets you easily perform common tasks with your Plutus contracts, including:
    - Computing validator hashes and script addresses
    - Running emulator tests
    - Serializing contracts and their associated input data to JSON files

💎 #4: **Keep projects in sync with `plutus-apps`**
  - Jhambala uses **[haskell.nix](https://input-output-hk.github.io/haskell.nix/)** to provide a fully self-reliant Plutus development environment for each of your projects, which can be brought up to date with the current state of `plutus-apps` using a single command.
  - No wrangling of dependency boilerplate: just build your project environment and get to work, then bump `plutus-apps` for a specific project whenever you like.
  - Serve Haddock documentation for the specific `plutus-apps` revision your project uses with the `serve-docs` command.

💎 #5: **Learn from a wealth of high-quality tutorials and samples**
  - **[Cardano CLI Guru](https://github.com/iburzynski/cardano-cli-guru)** provides a series of easy-to-follow `cardano-cli` tutorials that teach you how to build increasingly complex transactions and native scripts.
  - Numerous sample contracts are included to help you learn Plutus quickly.
  - **[Jambhalucid](https://github.com/iburzynski/jambhalucid)** provides an example frontend user interface for sample contracts built with Next.js, TypeScript and Lucid/`use-cardano` (UNDER CONSTRUCTION).

# **Installation**

### 0. **Requirements**

  * This project uses the Nix package manager to build a fully-functioning and reproducible Cardano development environment.
  * Nix is only compatible with Unix-like operating systems, so you must be using a Linux distribution, MacOS, or WSL2 (Windows Subsystem for Linux) to install Jambhala.
  * Your system must have `bash` and `git` installed. Run `bash --version` and `git -v` in a terminal window to confirm.
    * **NOTE for MacOS users:** MacOS may ship with versions of `bash` and `grep` that are incompatible with this workflow. You should install `bash`/`grep` using Homebrew first before proceeding.
  * This project assumes the use of `VS Codium` (a preconfigured installation is provided) or `VS Code` as editor and `bash` as shell (also provided in the Nix development environment). Other tools will require alternative workflows that are not covered here.
  * Jambhala and `cardano-node` are storage-intensive. We suggest you have at least `50GB` of free disk space before proceeding further.

***
### 1. **Install `nix`**
***
  - If you're setting up Nix on your system for the first time, try Determinate Systems' **[Zero-to-Nix](https://zero-to-nix.com)** in lieu of the official installer, as it provides an easier tool for **[installing](https://zero-to-nix.com/start/install)** and **[uninstalling](https://zero-to-nix.com/start/uninstall)** Nix.
  - Alternatively, you may follow the instructions for **multi-user installation** for your OS at **[nixos.org](https://nixos.org/download.html)**. This approach will require some additional configuration and it will be harder to uninstall Nix should you need to. It is only recommended if you've previously installed Nix on your system, as it will detect and repair a previous installation as needed.
  - When you are finished installing Nix, close the terminal session and open a fresh one.
***
### 2. **Configure `nix.conf`**
***
  * Edit `/etc/nix/nix.conf`: this requires root access to edit. Use a terminal-based editor like `nano` (i.e.):

      ```sh
      sudo nano /etc/nix/nix.conf
      ```

  * Modify the file following the instructions below:

    ```
    # Sample /etc/nix/nix.conf

    # Step 2a: Add this line to enable Flakes if missing (if you used the Zero-to-Nix installer this should already be added)
    experimental-features = nix-command flakes

    # Step 2b: Add your username to trusted-users (also include 'root' to prevent overriding default setting)
    trusted-users = root your-username

    # Step 2c: Avoid unwanted garbage collection with nix-direnv
    keep-outputs = true
    ```

  * **🚨 IMPORTANT!** You must restart the `nix-daemon` to apply the changes

    **Linux:**

      ```sh
      sudo systemctl restart nix-daemon
      ```

    **MacOS:**

      ```sh
      sudo launchctl stop org.nixos.nix-daemon
      sudo launchctl start org.nixos.nix-daemon
      ```


***
### 3. **Create your repository**
***
There are two "modes" you can choose from to use Jambhala, depending on your use case:

### **Learning Mode**
**Learning Mode** is recommended if you are currently learning Cardano/Plutus or just experimenting with Jambhala, as opposed to developing a full-fledged project.

Jambhala is under active development, with new features and learning materials being added on a continuous basis. To take advantage of the latest additions, it's better to work inside a fork of the repository. Your fork will maintain a historical link with the source repository upstream, which makes it easier to fetch and merge upstream changes into your local Jambhala setup (see **[Updating Jambhala](#📥-updating-jambhala)** for instructions).

**Cons:**
  * Github only allows one fork of a repository at a time
  * Contributions to a fork aren't reflected in your activity on Github

For these reasons, **Learning Mode** isn't well-suited for developing your own projects using Jambhala.

To use Jambhala in **Learning Mode**, just click the **`Fork`** button at the top of this repository's page on Github to create your fork. Then proceed to **Step 4**.

### **Development Mode**
**Development Mode** allows you to generate a completely independent repository, which can be personalized for your project. You can generate as many repositories from the template as you like, and your contributions to them will be reflected in your Github activity.

**Cons:**
  * Generating from a Github template creates a new repository with no historical link to the upstream template.
  * This makes it difficult to incorporate updates to Jambhala released after your repository is generated.

It's still possible to update Jambhala in **Development Mode**, although it requires more manual labor to resolve merge conflicts (see **[Updating Jambhala](#📥-updating-jambhala)** for instructions).

To use Jambhala in **Development Mode**, select the green **`Use this template`** button on this repository's Github page, and select **`Create a new repository`** to generate a new repository from the Jambhala template.

***
### 4. **Clone repository & test readiness**
***

Clone your new repository in a terminal session:

  ```sh
  git clone https://github.com/PATH-TO/YOUR-REPO.git --recurse-submodules
  ```

  >**Note:** Jambhala uses git submodules to incorporate companion projects (like **[Cardano EZ-Installer](https://github.com/iburzynski/cardano-ez-installer)**, **[Cardano CLI Guru](https://github.com/iburzynski/cardano-cli-guru)**, and **[Jambhalucid](https://github.com/iburzynski/jambhalucid)**). These projects are maintained as separate repositories. To include them when you clone your fork, you must use the `--recurse-submodules` flag.

Before proceeding to Step 5, navigate to your project directory in a terminal window and run the **Jambhala Readiness Test** to confirm whether your system is ready to install the Jambhala environment:

```sh
./ready?
```

The script will prepare your system to use Jambhala and check for any issues with your Nix configuration.

#### **Installing `direnv`**
Jambhala uses the `direnv` utility to provide seamless loading of the Nix environment whenever you navigate into the project directory tree. The **Readiness Test** will prompt you to install `direnv` using Nix and configure it to work with your shell if you don't already have a compatible version installed and configured.

##### **Installing `direnv` manually**
  Jambhala requires `direnv` version `>= 2.30`, which may not be available in the packaging systems for certain older operating systems (for instance, any Ubuntu system below version `22.10`). For convenience, the **Readiness Test** allows you to install a compatible version using Nix.
  
  While not recommended, if you prefer to install `direnv` through a different method you may do the following:
  
  - Visit the **[direnv installation page](https://direnv.net/docs/installation.html)** and check which version is available for your OS in the `Packaging status` section. If `direnv` version `2.30` or higher is available for your machine, follow the instructions to install it. Otherwise use the **Readiness Test** to install a compatible version using Nix.
  - The final step is to hook `direnv` into your shell. Running the **Readiness Test** (`./ready?`) will complete this step for you, but if you prefer to do it manually you can **[follow the instructions](https://direnv.net/docs/hook.html)** for your preferred shell here.

  >**Note:** MacOS has two types of shell sessions: login and interactive. If using login sessions, you'll need to add the appropriate hook to your `.zprofile` or `.bash_profile` file (depending on which shell you use). `.zshrc` and `.bashrc` are used for interactive sessions. For convenience, the **Readiness Test** adds hooks to all four of these files for Mac users.

After the `./ready?` script completes, correct any issues and re-run it until all tests pass.

***
### 5. **Build environment & set up project**
***

  * Open a new terminal window and navigate to your project directory:

    ```sh
    cd path-to-your-project
    ```

    You should now see the following message (if not, return to **[Step 4](#4-clone-repository--test-readiness)** and complete the **Readiness Test**):

    ```sh
    direnv: error /home/path-to-your-project/.envrc is blocked. Run `direnv allow` to approve its content
    ```

    This is a security measure, since `.envrc` files can run arbitrary shell commands. Make sure you always trust the author of a project and inspect the contents of its `.envrc` file before running `direnv allow`.

    When you're ready, enter `direnv allow` to approve the content:

    ```sh
    direnv allow
    ```

  * You can ignore the following warning: `direnv: ([/nix/store/.../bin/direnv export bash]) is taking a while to execute. Use CTRL-C to give up.`

  * It will take significant time (~2 hours) to set up the environment the first time. You can recite the **[Yellow Dzambhala Mantra](https://mantrasmeditation.com/buddhist-mantras/yellow-dzambhala-mantra/)** while you wait:

    ```
                  _=_
                q(-_-)p
                '_) (_`
                /__/  \
              _(<_   / )_
    _________(__\_\_|_/__)_________

    Om Dzambhala Dzalentraye Svaha!
    ```

  - Some dependencies will need to be built from source, but if you see "building" for certain packages that should be downloadable from a binary cache (particularly GHC) or if you see any warning such as `warning: ignoring substitute`, this means your system is not configured correctly and Nix is attempting to build packages from source that it should be fetching from a cache. Exit with `CTRL + c` and repeat **[Step 4](#2-configure-nixconf)**, then try again. Make sure to restart the `nix-daemon`!
  - **If you see any HTTP-related errors**, it means the IOG binary cache is non-responsive. Wait a bit and try again later.

  - Once the Nix environment build process completes, run `jsetup` to launch the Jambhala setup wizard:

    ```sh
    jsetup
    ```

  - The setup wizard runs differently depending on whether you're using Jambhala in **Learning** or **Development** mode:
    - If you created your repository by forking (**Learning Mode**), the wizard will simply build the project without any personalization.
    - If you created your repository by generating from the template (**Development Mode**), the wizard will provide a series of prompts to personalize your project.
      - Your `.cabal` and `LICENSE` files will be customized using your answers to the prompts.
      - The Jambhala `README` file will be moved to the `docs` directory, and a new `README` for your project will be created

  - The project will then be built using `cabal`. While seemingly redundant (since the project's dependencies have already been built using Nix), a `cabal build` is necessary for proper Haskell Language Server support in VS Codium/Code. This step will also take some time to complete:

    ```
                  _=_
                q(-_-)p
                '_) (_`
                /__/  \
              _(<_   / )_
    _________(__\_\_|_/__)_________

    Om Dzambhala Dzalentraye Svaha!
    ```

  - **Installing `cardano-node` & `cardano-cli`**
    - Once the project has built, the setup wizard will prompt you to install `cardano-node` and `cardano-cli`.
    - You'll need a fully-synced `cardano-node` with `cardano-cli` to submit example transactions to the blockchain. The setup wizard runs **[Cardano EZ-Installer](https://github.com/iburzynski/cardano-ez-installer)** to easily install and configure these tools in a single step.
    - This installation method also makes it easy to update your node/cli to a new version later.
    - If you've already installed `cardano-node` and `cardano-cli` through other means, you can also configure your existing installation to work with Jambhala.
    - See the **[Cardano EZ-Installer README](./cardano-ez-installer/README.md)** for more information.
    - A tutorial with guided exercises for learning to use `cardano-cli` is provided in the `cardano-cli-guru/tutorial` directory.

***
### 6. **Start coding**
***
Jambhala's development environment includes a preconfigured instance of VS Codium (a community-driven, freely-licensed distribution of VS Code without Microsoft branding or telemetry). This "Jambhala Editor" comes with all the required extensions for Cardano development already installed via Nix. 

Simply use the `jcode` command from your project's root directory in your terminal to open the Jambhala editor and start coding!

>**Note:** when you open a Haskell (`*.hs`) file for the first time in your editor, you may see the following popup appear:
  ```
    How do you want the extension to manage/discover HLS and the relevant toolchain?

    Manually via PATH   Cancel    Automatically via GHCup
  ```
  Select **`Manually via PATH`**. Our project is using the Haskell tooling installed via Nix in the development environment, *not* a system-wide installation via GHCup. If you select GHCup the Haskell Language Server (HLS) won't work properly in the editor.

#### **Adding Extensions to Jambhala Editor**
Because the Jambhala Editor is installed via Nix, it isn't possible to install additional extensions in the usual manner from within the application. Instead, they can be added to the `flake.nix` file and installed via Nix when you load the development environment using `direnv`. To add an extension:
- Click the `Extensions` icon in the left menu panel and look up the extension in the marketplace. 
- Click on the extension you wish to install and click the **gear icon** next to the `Install` button. 
- Select `Copy Extension ID`. 
- Visit **[https://search.nixos.org/packages?channel=unstable](https://search.nixos.org/packages?channel=unstable)** and paste the Extension ID into the search. If a matching result is returned, your extension is available in the `nixpkgs` repository and can be added to VS Codium.*
  >**Note:** in some rare cases, extensions are proprietary and thus aren't compatible with VS Codium (only VS Code). 
- Open `flake.nix` and find the following section:

  ```
    # flake.nix
      ...
      vscodeExtensions = with pkgs.vscode-extensions; [
        asvetliakov.vscode-neovim
        dracula-theme.theme-dracula
        haskell.haskell
        jnoortheen.nix-ide
        justusadam.language-haskell
        mkhl.direnv
        ms-python.python
        ms-python.vscode-pylance
      ];
      ...
  ```
- Paste the Extension ID into the list of extensions on a new line and save the changes.
- Close VS Codium, and run `direnv reload` in your terminal (inside your project root directory).
- Run the `jcode` command to relaunch VS Codium. Your extension should now be installed and ready for use.

\* If your desired extension isn't available in `nixpkgs`, it is still possible to add it to `flake.nix`, but the process is more complex and will not be covered here. You can **file an issue** to request a particular extension be added if you feel it will be beneficial to Jambhala users, and I will consider adding it to the flake upstream.

#### **Vim Mode**
Jambhala's VS Codium editor comes with the `neovim` extension installed, but it's disabled by default when you load the editor using `jcode`. If you prefer to use Vim keybindings, you can enable `neovim` by changing the `VIM_MODE` environment variable in the `.env` file:

```sh
VIM_MODE=true
```

#### **Use Jambhala with your own VS Code/VS Codium**
While the Jambhala Editor provides the most rapid route to start coding, you can also use Jambhala with your existing VS Code/Codium installation.

Loading the editor from within the Nix environment provides the most reliable experience, as it prevents errors that can be encountered when extensions (particularly the Haskell extension) load before the Nix environment has finished loading via `direnv`.

Open the project root directory in your terminal and run one of the following, depending on your preferred editor:


```sh
code .
```

or...

```sh
codium .
```

Alternatively, you can simply start VS Code/Codium and use the `File > Open Folder...` menu option to load your project directory. This method is more convenient, but occasionally results in errors indicating the Haskell extension is unable to determine the project's version of GHC. This is caused by the issue explained above, and may require occasionally reloading the project or require you to `Restart Haskell LSP Server` from the command palette (`CTRL + SHIFT + P`). For the best experience, launch your editor from your terminal inside the project directory.

The first time you open the project, you'll be prompted to install some recommended extensions if you don't have them already: `haskell`, `direnv` and `Nix IDE`. Follow the prompt to install these, and close/relaunch your editor before continuing. 

Accept any pop-up prompts from the `direnv` extension when you encounter them.

***
# **Using the `jamb` CLI**
Jambhala includes a simple command-line utility called `jamb`, which reduces boilerplate and provides a simple API for the following uses:

### **Listing Contracts**
You can run the following command to view the names of available contracts in your project, for use with other commands:

```sh
  jamb -l
```

### **Hashing Validators**
You can calculate the validator hash for any available contract like this:

```sh
jamb -s CONTRACT
```

where `CONTRACT` is the name of the contract to hash.

### **Testing Contracts with Emulator Trace**
You can run the emulator test defined for a contract with the following command:

```sh
jamb -t CONTRACT
```

where `CONTRACT` is the name of the contract to test.

### **Compiling Contracts to `.plutus` Files**
You can run the following command from the workspace terminal to write a contract to a `.plutus` file:

```sh
jamb -w CONTRACT [FILENAME]
```

where `CONTRACT` is the name of the contract to compile, and `[FILENAME]` is an optional file name (the contract name is used as the filename by default if no argument is given). Contracts are saved in the `assets` directory of the `cardano-cli-guru` submodule, where they can be used to easily submit transactions via `cardano-cli`, assisted by the various utility scripts provided by `cardano-cli-guru`. When the command finishes, you'll get a `CONTRACT.plutus` file at `cardano-cli-guru/assets/scripts/plutus` that contains a JSON envelope of the UPLC code:

```json
{
    "type": "PlutusScriptV2",
    "description": "",
    "cborHex": "5907c05907bd0100003232323232323232323..."
}
```

This file can now be used in on-chain transactions. 

***

# **Writing Contracts**

### **🚨 Read This First!**
Jambhala makes certain opinionated decisions in order to vastly reduce the boilerplate required to write Plutus contracts.

#### **Prelude**
  - Jambhala is configured to use `PlutusTx.Prelude` as its default prelude via a `mixin` in the `.cabal` file.
  - This eliminates the need to include both the `{#- LANGUAGE NoImplicitPrelude #-}` extension and `import PlutusTx.Prelude` in your contract files.

#### **Plutus & Haskell imports**
  - Many common Plutus types and functions are available via a single import from `Jambhala.Plutus`.
  - If you need to use regular Haskell functions that would normally be imported via the standard Prelude or from other modules in `base`, import them from `Jambhala.Haskell` or from the specific `base` modules they reside in
  - For example, if you need to use the ***IO*** type in signatures and the `putStrLn` and `print` functions, import them from `Jambhala.Haskell` like so:

      ```haskell
      import Jambhala.Haskell ( IO, print, putStrLn )
      ```

  - See the sample contracts in `src/Contracts/Samples` for more examples of handling imports with Jambhala.

#### **Language extensions**
The following language extensions are enabled project-wide by Jambhala using the `default-extensions` setting in the `library` stanza of the `.cabal` file:

```
  default-extensions:
    -- Allows promotion of types to "kinds", enabling more expressive type-level programming (required for all Plutus contracts):
      DataKinds

    -- A syntactic convenience for writing single-argument lambdas containing case expressions (used in Jambhala's utilities)
    , LambdaCase

    -- Allows construction of Text and other string-like values as string literals:
    , OverloadedStrings

    -- A syntactic convenience for working with record values (used in Jambhala's utilities):
    , RecordWildCards

    -- Required for all Plutus contracts to translate between Plutus and Haskell:
    , TemplateHaskell
```

Beyond these, the sample contracts include only the specific language extensions needed to compile their code. Keep in mind that Haskell language extensions are experimental modifications to compiler behavior: they should be used only when necessary and with clear understanding of their purpose. It is better to add extensions incrementally as they become needed than to add a multitude of modifications to the compiler as boilerplate in every file.

#### **Sample contracts**
The source code for the sample Plutus contracts live in the `src/Contracts/Samples` folder.

If you want to hide the sample contracts from the `jamb` utility and only serve your own contracts, you can modify the `main` action in `app/Main.hs` accordingly:

```haskell
main :: IO ()
main = runJamb contracts -- << replace `allContracts` with `contracts` to hide sample contracts
  where allContracts = samples <> contracts
```

***
## **Creating a Contract**
***
To create a new contract, create a new `.hs` file in the `src/Contracts` directory, and write a module declaration, i.e.:

```haskell
module Contracts.MyContract where
```

In the `jambhala.cabal` file, add your module name (i.e. `Contracts.MyContract`) to the `exposed-modules` section of the `library` stanza:

```
library
  import: common
  exposed-modules:
    Contracts
    Jambhala.CLI
    Jambhala.Haskell
    Jambhala.Plutus
    Jambhala.Utils

-- Add new contracts here, i.e.:
    Contracts.MyContract

...
```

**🚨 IMPORTANT:** you must stage any new contract files you create to git before they are visible to Nix for compilation. Use the `Source Control` option in the left sidebar of VS Code or stage changes from the command line with `git add`.

You're now ready to write your contract, which should contain a ***Validator*** value (by convention in the samples this is called `validator`). See the contracts in `src/Contracts/Samples` for example validators.

### **Writing Emulator Tests**
Jambhala provides an enhanced variant of the Plutus emulator with some additional conveniences.

To define an emulator trace that is compatible with the `jamb` CLI, use the type `JambEmulatorTrace`.

>`JambEmulatorTrace` is similar to the `EmulatorTrace ()` type used with the standard emulator, but subsumes the unit (`()`) type and includes an additional effect for more convenient management of mock wallets.

All code used in the standard `EmulatorTrace` context is compatible with `JambEmulatorTrace`, but some additional emulator utilities are provided by `Jambhala.Utils` for convenience:

- `activateWallets`: takes endpoints and activates all mock wallets in the test, returning their `ContractHandle` values in an `IntMap` with keys corresponding to the wallet numbers. Activated wallet handles can be referenced in the test via the (`!`) operator.

  ```haskell
  hs <- activateWallets endpoints
  callEndpoint @"give" (hs ! 1) 33_000_000
  ```

- `wait1`: waits one slot in an emulator test. This is equivalent to `waitNSlots 1` but discards the resulting `Slot` value, making it more convenient to use.

### **Using the `jamb` CLI**
The `jamb` CLI can perform various operations on your contracts, including calculating its validator hash, testing it using a blockchain emulator, and compiling it into a `.plutus` file. To do this we need to prepare a ***ContractExports*** value in each of our contracts.

***ContractExports*** are constructed from the following components:
1. A ***Validator*** or ***MintingPolicy***
2. A list of ***DataExport*** values
  - This includes any additional data required by your contract (i.e. datums and redeemers) that you wish to serialize
  - Can be empty
3. **Optional:** a ***JambEmulatorTrace*** test and desired number of mock wallets

**Data Exports**  
Any value with a ***ToData*** instance can be converted to a serializable ***DataExport*** value by applying the `DataExport` constructor to a filename `String` and the value. The following example will be serialized into a file `unit.json` containing `{"constructor":0,"fields":[]}`:

```haskell
unitExport :: DataExport
unitExport = DataExport "unit" ()
```

You can prepare any pieces of data you wish to serialize with your contract this way, and aggregate them into a list when you produce your ***ContractExports***.

**Generating Contract Exports**  
For a contract without an emulator test, apply the `exportValidator` function (from `Jambhala.Utils`) to your validator and list of data exports to prepare it for use with the `jamb` CLI:

```haskell
exports :: ContractExports -- Prepare exports for jamb CLI
exports = exportValidator validator dataExports
```

To export a contract with an emulator test, apply the `exportValidatorWithTest` function to:
1. the validator
2. data exports list
3. emulator test
4. desired number of mock wallets

```haskell
exports :: ContractExports -- Prepare exports for jamb CLI:
exports = exportValidatorWithTest validator dataExports test 4
```

(where `validator`, `dataExports`, and `test` are values defined in our contract of types ***Validator***, **[*DataExport*]**, and ***JambEmulatorTrace***, respectively)


To make your contract visible to the CLI, in `src/Contracts/Contracts.hs` import your contract module as a qualified import, i.e.:

```haskell
import qualified Contracts.MyContract as MyContract
```

Then add a new tuple entry to the `contracts` ***Map***, containing a name string for your contract and reference to its exports like so:

```haskell
contracts :: Contracts
contracts = M.fromList [
    ("my-contract", MyContract.exports)
  ]
```

Once your contract has been added to the map, it can now be operated on by the `jamb` CLI:

```sh
jamb -s my-contract
jamb -t my-contract
jamb -w my-contract
```

***
## **Using GHCi**
***
To start a GHCi REPL session, run `jrepl` and then load your contract:

```sh
jrepl

Prelude Contracts λ > :m Contracts.MyContract
Prelude Contracts.MyContract λ >
```

***
## **Serving `plutus-apps` docs**
***
To serve docs for the specific revision of `plutus-apps` this project is using, open a new bash terminal from the project root directory and run the following command:

```sh
serve-docs
```

**Note:** This will require significant additional build time and storage space the first time the docs are served.

The script will look up the specific `plutus-apps` revision hash from the `cabal.project` file, clone the `plutus-apps` repository (if it doesn't already exist) and checkout this revision, then launch a new `nix develop` shell and serve the docs at `http://0.0.0.0:8002/`.

To view the correct Haddock documentation for the revision you are using, open http://0.0.0.0:8002/haddock in your browser.

***
# **Updating Jambhala**
#### **🛠️ This section is under construction...**
Since Jambhala is under active development and is closely tracking the progress of `plutus-apps`, its codebase changes frequently.

## **Learning Mode**
If you created your repository by forking Jambhala (**Learning Mode**), you can easily update Jambhala using `git fetch`/`git merge` (or `git pull`).

If you've made changes to any core Jambhala files, you may encounter a merge conflict that you'll need to resolve. You may find a VS Code extension like **[Git Merger](https://marketplace.visualstudio.com/items?itemName=shaharkazaz.git-merger)** to be helpful with this.

## **Development Mode**
Unlike forks, Github repositories generated from templates have unique histories, so they aren't able to fetch and merge upstream changes as smoothly. However it's still possible to merge updates from an upstream template into your project with a little manual effort.

The `jsetup` wizard added the upstream template as a remote source. You can now run the `jupdate` command to fetch any changes to the template and attempt to merge them:

```sh
jupdate
```
> *Note that this command is distinct from the `jamb -u` command discussed below, which updates only the `plutus-apps` dependency in `cabal.project`, not Jambhala itself.*

You will need to manually resolve the resulting merge conflicts. You may find a VS Code extension like **[Git Merger](https://marketplace.visualstudio.com/items?itemName=shaharkazaz.git-merger)** to be helpful with this.

***
# **Updating Plutus Dependencies**
The non-Hackage dependencies in the `cabal.project` file are following the **[plutus-apps](https://github.com/input-output-hk/plutus-apps)** library, with `sha256` hashes calculated for each `source-repository-package` entry.

`jamb` provides a utility to easily update `plutus-apps` to the most recent revision and adjust all related dependencies. Run the `jamb -u` command to pull the latest revision and generate a new `cabal.project` file.

```sh
jamb -u
```

🚨 **WARNING!** This operation rewrites your `cabal.project` file according to the most recent `plutus-apps` commit, and may cause your environment and/or contracts to break. You should use at your own risk, but you can also easily restore a previous `cabal.project` file by following the instructions for **Restoring a previous version** below.
***
## **Set `plutus-apps` to a specific commit/tag**
***
You can also use the `jamb -u` command with an additional argument to set `plutus-apps` to a specific commit hash or tag:

```sh
jamb -u 38979da68816ab84faf6bfec6d0e7b6d47af651a
```

You can run the `pa-history` command to view the full commit history for `plutus-apps`:

```sh
pa-history
```

Use the up/down keys to navigate or type `q` to quit.

🚨 **WARNING!** The code in the sample contracts and `Jambhala.Plutus` module have been designed for compatibility with very recent commits of `plutus-apps` - this means pointing `plutus-apps` to older tags/commits is much more likely to result in breakage. *Use this feature at your own risk!*

***
## **Restoring a previous version**
***
Before the `jamb -u` command rewrites your `cabal.project` file, a backup of your existing `cabal.project` will be created in the `backups/` directory in case you need to roll back the update. Just delete the current `cabal.project` file, copy the backup file and rename it to `cabal.project`. Then run `direnv allow` or reload the project in VS Code and your previous project state will be restored.

***
## **Manually updating dependencies**
***
Since Nix flakes require pure inputs to guarantee reproducibility, and the content associated with a particular Git repository/tag can change, we need to hash any repositories we include in `cabal.project`. This means if we need to manually change any dependencies or add additional ones, we'll need to calculate new hashes and replace the existing ones.

While not recommended, if you need to change the revision of Plutus dependencies manually, you can calculate sha256 hashes for them using the `nix-prefetch-git` utility, which has been provided with this project's Nix development environment.

Use the following command to calculate a hash:

```
nix-prefetch-git LOCATION TAG
```

Here is an example of how we'd calculate a hash for the `plutus-apps` dependency with tag `5dda0323ef30c92bfebd520ac8d4bc5a46580c5c`:

```sh
nix-prefetch-git https://github.com/input-output-hk/plutus-apps.git 5dda0323ef30c92bfebd520ac8d4bc5a46580c5c

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

The hash string must now be added as a comment prexied with `--sha256:` anywhere inside the `source-repository-package` stanza like so:

```
source-repository-package
  type: git
  location: https://github.com/input-output-hk/plutus-apps.git
  tag: 5dda0323ef30c92bfebd520ac8d4bc5a46580c5c
  --sha256: 05ggi69w2n0cnhfyifpa83aphq6avk0fd9zvxywn1scwxza85r1a
```

***
# **Troubleshooting**

#### **This section is under construction...**

**`'hs-source-dirs: app' specifies a directory which does not exist.`**
```
  > Warning: 'hs-source-dirs: app' specifies a directory which does not exist.
  > building
  > Preprocessing library for jambhala-0.1.0.0..
  > Error: Setup: can't find source for Contracts/... in src,
```
You need to **stage** your new module file in `git` so it becomes visible to the Nix environment. Use the `Source Control` option in the left sidebar of VS Code or stage changes from the command line with `git add`. Then try the operation that caused the error again.

**VS Codium/Code doesn't provide Haskell IDE support: hangs with `Processing: 1/x`**: 

It's possible the project dependencies haven't been properly built via `cabal`, which is a requirement for Haskell Language Server support in VS Codium/Code. Run `cabal build` to build the dependencies.

### **General Troubleshooting Techniques**
- **Restart Haskell LSP Server:** restarting `haskell-language-server` often fixes common issues with Haskell in VS Code. Open the command palette (`Ctrl + Shift + p`) and begin typing `Restart Haskell LSP Server` until you see this option, and select it. In the future it will be pinned to the top of the command palette options and easier to find.
- **`jrebuild`:** cleaning the `dist-newstyle` directory of all build artifacts and rebuilding the project may resolve certain issues

  ```sh
  jrebuild
  ```

  Note that it will be time-consuming to `cabal build` the project from scratch, so be sure to exhaust all other troubleshooting options before attempting.

**For assistance or bug reporting, file an issue or email `ian.burzynski@emurgo.io`.**