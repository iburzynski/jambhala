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
  * **[Build environment](#5-build-environment)**
  * **[Getting started: j commands and setup wizard](#6-getting-started-j-commands-and-setup-wizard)**
  * **[Start coding](#7-start-coding)**
* **[Writing contracts](#writing-contracts)**
  * **[Creating a contract](#creating-a-contract)**
  * **[Writing emulator tests](#writing-emulator-tests)**
  * **[Using `j cli`](#using-j-cli)**
  * **[Using GHCi](#using-ghci)**
  * **[Serving docs](#serving-docs)**
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
  - Only `git` and **[`nix`](#1-install-nix)** are required to get started.
  - Jambhala's **Readiness Test** confirms system readiness before proceeding with installation, preventing wastage of time and resources on failed builds.
  - Jambhala's setup wizard handles everything else, including easy installation of `cardano-node` and `cardano-cli` using **[Cardano EZ-Installer](https://github.com/iburzynski/cardano-ez-installer)**.
  - **[plutus-apps](https://github.com/input-output-hk/plutus-apps)** is installed internally to your project, so you don't need to maintain a central clone and use its associated Nix shell as the entry point for your projects (See **Jewel #4** below).
  - A preconfigured VS Codium editor is included, allowing you to start coding immediately (you can still use your editor of choice if preferred).

💎 #2: **Minimize contract boilerplate**
  - Jambhala uses a custom `Prelude` module which includes both the `PlutusTx.Prelude` and common Haskell items .
    - No need to use `NoImplicitPrelude` pragma and manually import `PlutusTx.Prelude` or Haskell's prelude
    - No need to use `hiding` clauses or qualified imports to avoid nameclashes: `PlutusTx` versions of functions that clash with their Haskell counterparts are prefixed with `p` (for prefix functions) and `#` (for infix operators)
  - The amount of required language extensions for Plutus development are significantly reduced, and commonly required extensions are enabled by default.
  - Common Plutus types and functions are re-exported from their respective modules by `Jambhala.Plutus`, so you don't need to keep track of messy import boilerplate. You can always import Plutus modules explicitly if you prefer.
  - `Jambhala.Utils` provides common utility functions to avoid contract clutter.

💎 #3: **Perform common `cardano-cli` and Plutus tasks with simple commands**
  - Jambhala includes **[Cardano CLI Guru](https://github.com/iburzynski/cardano-cli-guru)**, which provides utility scripts for common `cardano-cli` tasks that can be run as terminal commands.
  - A Haskell-based executable CLI (`j cli`) lets you easily perform common tasks with your Plutus contracts, including:
    - Computing validator hashes and script addresses
    - Running emulator tests
    - Serializing contracts and their associated input data to JSON files

💎 #4: **Keep projects in sync with `plutus-apps`**
  - Jhambala uses **[haskell.nix](https://input-output-hk.github.io/haskell.nix/)** to provide a fully self-reliant Plutus development environment for each of your projects, which can be brought up to date with the current state of `plutus-apps` using a single command.
  - No wrangling of dependency boilerplate: just build your project environment and get to work, then bump `plutus-apps` for a specific project whenever you like.

💎 #5: **Learn from a wealth of high-quality tutorials and samples**
  - **[Cardano CLI Guru](https://github.com/iburzynski/cardano-cli-guru)** provides a series of easy-to-follow `cardano-cli` tutorials that teach you how to build increasingly complex transactions and native scripts.
  - Numerous sample contracts are included to help you learn Plutus quickly.
  - **[Jambhalucid](https://github.com/iburzynski/jambhalucid)** provides an example frontend user interface for sample contracts built with Next.js, TypeScript and Lucid/`use-cardano` (UNDER CONSTRUCTION).

# **Installation**

### 0. **Requirements**

  * This project uses the Nix package manager to build a fully-functioning and reproducible Cardano development environment.
  * Nix is only compatible with Unix-like operating systems, so you must be using a Linux distribution, MacOS, or WSL2 (Windows Subsystem for Linux) to install Jambhala.
  * Your system must have `git` installed. Run `git -v` in a terminal window to confirm.
  * This project assumes the use of `VS Codium` (a preconfigured installation is provided) or `VS Code` as editor. Other editors will require alternative workflows that are not covered here.
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

    >**Note:** if no configuration file exists at `/etc/nix/nix.conf` it's possible the file is located elsewhere, depending on your OS. Run `find / -name "nix.conf" 2>/dev/null` to find the location of the file (this may take several minutes).

  * Modify the file following the instructions below:

    ```
    # Sample /etc/nix/nix.conf

    # Step 2a: Add this line to enable Flakes if missing (if you used the Zero-to-Nix installer this should already be added)
    experimental-features = nix-command flakes ca-derivations

    # Step 2b: Add your username to trusted-users (also include 'root' to prevent overriding default setting)
    trusted-users = root your-username

    # Step 2c: Avoid unwanted garbage collection with nix-direnv
    keep-outputs = true
    ```
  
  * Mac users with Apple silicon hardware (M1/M2 chip) also need to add the following, as `plutus-apps` currently doesn't build successfully on `aarch64` architecture:

    ```
    # Step 2d: Adjust system and platforms for aarch64 compatibility:
    system = x86_64-darwin
    extra-platforms = x86_64-darwin aarch64-darwin
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

Jambhala is under active development, with new features and learning materials being added on a continuous basis. To take advantage of the latest additions, it's better to work inside a fork of the repository. Your fork will maintain a historical link with the source repository upstream, which makes it easier to fetch and merge upstream changes into your local Jambhala setup (see **[Updating Jambhala](#updating-jambhala)** for instructions).

**Cons:**
  * Github only allows one fork of a repository at a time
  * Contributions to a fork aren't reflected in your activity on Github

For these reasons, **Learning Mode** isn't well-suited for developing your own projects using Jambhala.

To use Jambhala in **Learning Mode**, just click the **`Fork`** button at the top of this repository's page on Github to create your fork. Then proceed to **Step 4**.

### **Development Mode**
**Development Mode** allows you to generate a completely independent repository, which can be personalized for your project. You can generate as many repositories from the template as you like, and your contributions to them will be reflected in your Github activity.

**Cons:**
  * Generating from a Github template creates a new repository with no historical link to the upstream template.
  * This makes it more difficult to incorporate updates to Jambhala released after your repository is generated.

It's still possible to update Jambhala in **Development Mode**, although it requires more manual labor to resolve merge conflicts (see **[Updating Jambhala](#updating-jambhala)** for instructions).

To use Jambhala in **Development Mode**, select the green **`Use this template`** button on this repository's Github page, and select **`Create a new repository`** to generate a new repository from the Jambhala template.

***
### 4. **Clone repository & test readiness**
***

Clone your new repository in a terminal session:

  ```sh
  git clone https://github.com/PATH-TO/YOUR-REPO.git --recurse-submodules
  ```

  >**Note:** Jambhala uses git submodules to incorporate companion projects (like **[Cardano EZ-Installer](https://github.com/iburzynski/cardano-ez-installer)**, **[Cardano CLI Guru](https://github.com/iburzynski/cardano-cli-guru)**, and **[Jambhalucid](https://github.com/iburzynski/jambhalucid)**). These projects are maintained as separate repositories. To include them when you clone your fork, you must use the `--recurse-submodules` flag.

Before proceeding to Step 5, navigate to your project directory in a terminal window and run the **Jambhala Readiness Test** to confirm that your system is ready to install the Jambhala environment:

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
### 5. **Build environment**
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

***
### 6. **Getting started: `j` commands and setup wizard**
***

Once the environment loads successfully, you can get started using Jambhala's `j` commands (also called "recipes"). These provide concise commands to run common tasks from within the Jambhala environment.

To see available "recipes", run `j --list`. You'll see a list of commands displayed along with brief descriptions of what they do. You can run any of these by adding `j ` in front of the command. Specific `j` commands will be explained in later sections. 

We'll run our first `j` command to launch the Jambhala setup wizard (`j setup`), which is the final step of the installation process. It helps set up required Cardano tooling like `cardano-node` and `cardano-cli`, and personalizes your project if you're using **Developer Mode**.

```sh
j setup
```

If you created your repository by generating from the template (**Development Mode**), the wizard will begin by providing a series of prompts allowing you to personalize your project:
  - Your `.cabal` and `LICENSE` files will be customized using your answers to the prompts.
  - You can optionally move the Jambhala `README` file to the `docs` directory, and create a new `README` for your project.

#### **Installing `cardano-node` & `cardano-cli`**
Next, the setup wizard will prompt you to install `cardano-node` and `cardano-cli`.
  - You'll need a fully-synced `cardano-node` with `cardano-cli` to submit example transactions to the blockchain. The setup wizard runs **[Cardano EZ-Installer](https://github.com/iburzynski/cardano-ez-installer)** to easily install and configure these tools in a single step.
  - This installation method also makes it easy to update your node/cli to a new version later.
  - If you've already installed `cardano-node` and `cardano-cli` through other means, you can also configure your existing installation to work with Jambhala.
  - See the **[Cardano EZ-Installer README](./cardano-ez-installer/README.md)** for more information.
  - A tutorial with guided exercises for learning to use `cardano-cli` is provided in the `cardano-cli-guru/tutorial` directory.
  - The EZ-Installer will also ask if you want to install **[Ogmios](https://ogmios.dev/)**. Choose `y` if you want to use the optional `cardano-devnet` add-on utility, which required Ogmios.

***
### 7. **Start coding**
***
Jambhala's development environment includes a preconfigured instance of VS Codium (a community-driven, freely-licensed distribution of VS Code without Microsoft branding or telemetry). This "Jambhala Editor" comes with all the required extensions for Cardano development already installed via Nix. 

Simply use the `j code` command from your project's root directory in your terminal to open the Jambhala editor and start coding!

>**Note:** when you open a Haskell (`*.hs`) file for the first time in your editor, you may see the following popup appear:
  ```
    How do you want the extension to manage/discover HLS and the relevant toolchain?

    Manually via PATH   Cancel    Automatically via GHCup
  ```
  Select **`Manually via PATH`**. Our project is using the Haskell tooling installed via Nix in the development environment, *not* a system-wide installation via GHCup. If you select GHCup the Haskell Language Server (HLS) may not work properly in the editor.

#### **Adding extensions to `j code` editor**
Because the `j code` editor is installed via Nix, it isn't possible to update VS Codium or install additional extensions in the usual manner from within the application. Extensions can instead be added to the `flake.nix` file and installed via Nix the next time you load the Jambhala environment. To add an extension:
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
- Run the `j code` command to relaunch VS Codium. Your extension should now be installed and ready for use.

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

# **Writing contracts**

### **🚨 Read This First!**
Jambhala makes certain opinionated decisions in order to vastly reduce the boilerplate required to write Plutus contracts.

#### **Prelude**
  - Jambhala uses a custom `Prelude` module which includes both the `PlutusTx.Prelude` and common Haskell items.
    - No need to use `NoImplicitPrelude` pragma and manually import `PlutusTx.Prelude` and Haskell's prelude
    - No need to use `hiding` clauses or qualified imports to avoid nameclashes: `PlutusTx` versions of functions that clash with their Haskell counterparts are prefixed with `p` (for prefix functions) and `#` (for infix operators)

#### **Plutus imports**
  - Many common Plutus types and functions are available via a single import from `Jambhala.Plutus`, which aggregates and re-exports items from the various Plutus modules.
  - See the sample contracts in `src/Contracts/Samples` for more examples of handling imports with Jambhala.
  - You can still import Plutus modules directly if you prefer, or if you need something from a Plutus module that isn't included in `Jambhala.Plutus`. Qualified or restricted imports may be required if you want to combine both approaches, due to overlapping exports.

#### **Language extensions**
The following language extensions are enabled project-wide by Jambhala using the `default-extensions` setting in the `library` stanza of the `.cabal` file:
```
  default-extensions:
    -- Allows promotion of types to "kinds", enabling more expressive type-level programming (required for all Plutus contracts):
      DataKinds

    -- Allows automatic derivation of certain typeclasses (like FromJSON/ToJSON):
    , DeriveAnyClass
    , DeriveGeneric

    -- Allows defining typeclass instances for type synonyms:
    , FlexibleInstances

    -- Allows post-fix style qualified import declarations
    , ImportQualifiedPost

    -- Allows writing type signatures for methods in typeclass instances:
    , InstanceSigs
    
    -- A syntactic convenience for writing single-argument lambdas containing case expressions (used by Jambhala's utilities):
    , LambdaCase

    -- Allows more than one type parameter in class and instance declarations (required to lift parameters in parameterized validators):
    , MultiParamTypeClasses

    -- A syntactic convenience for constructing record values:
    , NamedFieldPuns

    -- Allows more readable representations of large integers (i.e. 1_000_000), useful for lovelace quantities
    , NumericUnderscores

    -- Allows construction of Text and other string-like values as string literals:
    , OverloadedStrings

    -- A syntactic convenience for destructuring record values:
    , RecordWildCards

    -- Allows referencing type variables in multiple scopes (required to lift parameters in parameterized validators):
    , ScopedTypeVariables

    -- Required for all Plutus contracts to translate between Plutus and Haskell:
    , TemplateHaskell

    -- Provides a convenient way to disambiguate type variables inline
    , TypeApplications

    -- Allows type-level functions (used in Jambhala's ValidatorEndpoints & MintingEndpoint classes):
    , TypeFamilies
```

Beyond these, the sample contracts include only the specific language extensions needed to compile their code. Extensions enabled by default are still declared explicitly in the sample contracts when they are introduced for the first time, in order to explain their use.

Keep in mind that Haskell language extensions are experimental modifications to compiler behavior: they should be used only when they provide a concrete benefit and with clear understanding of their purpose. It is better to add extensions incrementally as they become needed than to add a multitude of modifications to the compiler as boilerplate in every file.

#### **Sample contracts**
The source code for the sample Plutus contracts live in the `src/Contracts/Samples` folder.

If you want to hide the sample contracts from the `j cli` utility and only serve your own contracts, you can modify the `main` action in `j-cli/Main.hs` accordingly:

```haskell
main :: IO ()
main = runJamb contracts -- << replace `allContracts` with `contracts` to hide sample contracts
  where allContracts = contracts <> samples
```

***
## **Creating a contract**

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
    Jambhala.Plutus
    Jambhala.Utils
    Prelude

-- Add new contracts here, i.e.:
    Contracts.MyContract
    Contracts.MyOtherContract

...
```

We're now ready to write our contract. We begin by defining a predicate function to express the validator or minting policy logic. 

We then define a custom type synonym for our contract, using either *`ValidatorContract`* or *`MintingContract`* with a type-level string as an identifier:

```haskell
type MyValidator = ValidatorContract "my-validator"
```

or...

```haskell
type MyMintingPolicy = MintingContract "my-minting-policy"
```

The string identifier will be used to reference the contract in `j cli` commands.

Then we compile the predicate code into Plutus (using Template Haskell) and apply the appropriate Jambhala constructor function based on the type of our contract. We must provide a type signature for this value with the type synonym chosen above:

```haskell
contract :: MyValidator
contract = mkValidatorContract $$(compile [||validator||])
```

or...

```haskell
contract :: MyMintingPolicy
contract = mkMintingContract $$(compile [||policy||])
```

>See the contracts in `src/Contracts/Samples` for examples of predicate definition and compilation.

## **Writing emulator tests**
Jambhala provides an enhanced variant of the `plutus-apps` blockchain emulator with a simpler and more intuitive API. This tool (and the associated utilities imported from `Jambhala.Utils`) allows us to write simple and readable off-chain code in Haskell and conduct simple tests of our contracts.

The emulator environment's behavior doesn't always perfectly match the way contracts behave on-chain (for instance, fee amounts may vary and should not be used for predictive purposes). For rigorous testing of contracts intended for production, more robust tools like `plutus-simple-model` should be used. However, the emulator provides a way to confirm the expected behavior of on-chain scripts, as well as a good way to practice Haskell fundamentals.

### ***`ValidatorEndpoints`* & *`MintingEndpoint`* Classes**
After defining a custom type synonym for our contract using either *`ValidatorContract`* or *`MintingContract`* and a type-level string identifier, we must instantiate one of two corresponding typeclasses to implement emulation endpoints. 

### ***`ValidatorEndpoints`***
The *`ValidatorEndpoints`* class consists of the following:
  * Two associated data types: *`GiveParam`* and *`GrabParam`*. These represent the types of the inputs our off-chain endpoint actions will accept.
  * Two methods, `give` and `grab`, which define the off-chain endpoint actions through which we can lock and unlock UTxOs at our contract's script address in an emulated blockchain environment.

*`GiveParam`* and *`GrabParam`* are *associated data types*, which are used to declare custom data types associated with an instance of a particular typeclass:

```haskell
instance ValidatorEndpoints MyContract where
  newtype GiveParam MyContract = Give {lovelace :: Integer}
  data GrabParam MyContract = Grab
```

Here we've declared two new data types associated with *`MyContract`*: *`GiveParam MyContract`* and *`GrabParam MyContract`*. We've used the more efficient `newtype` keyword to declare our *`GiveParam`* type, since it has a single constructor and a single field. Our *`GrabParam`* type contains no fields (in this simple hypothetical example, we don't need to provide any information to unlock UTxOs from the contract address). We can think of this type as equivalent to the Unit type (*`()`*), but we'll use the value `Grab` to construct it, rather than `()`.

>**Note:** We can call the constructors whatever we like, but the sample contracts use `Give` and `Grab`, which provide semantic clarity in the context of our emulator tests.

In order for the emulator to work properly, we also need to be able to encode and decode parameter values to/from JSON format. This necessitates a bit of boilerplate `deriving` code for each of our parameter types:

```haskell
instance ValidatorEndpoints MyContract where
  newtype GiveParam MyContract = Give {lovelace :: Integer}
    deriving (Generic, FromJSON, ToJSON)
  data GrabParam MyContract = Grab
    deriving (Generic, FromJSON, ToJSON)
```

We're now ready to implement the `give` and `grab` endpoint methods, which have the following signatures:

```haskell
give :: GiveParam MyContract -> ContractM MyContract ()
grab :: GrabParam MyContract -> ContractM MyContract ()
```

They accept values of our newly-created *`GiveParam`* and *`GrabParam`* types, and return a unit value inside a monadic context called *`ContractM`*, which is parameterized by our *`MyContract`* type. 

>**Note:** *`ContractM`* is a type synonym for a more polymorphic *`Contract`* monad defined in the `plutus-apps` libraries.

The `give` and `grab` endpoint actions can contain arbitrary Haskell code depending on the nature of the contract. Their role is to construct and submit transactions, ideally conducting some preliminary validation mirroring the logic of the contract's on-chain script. The purpose of this preliminary off-chain validation is to prevent unnecessary submission of invalid transactions. 

Ultimately, `give` and `grab` need to submit a transaction to the script address and await confirmation, using the `submitAndConfirm` function. This function takes a *`Transaction`* value constructed via the `Tx` constructor and two fields: `lookups` (which define which information is visible to the transaction) and `constraints` (which define the conditions under which the transaction succeeds or fails):

```haskell
submitAndConfirm
      Tx
        { lookups = scriptLookupsFor contract,
          constraints = mustPayToScriptWithDatum contract () lovelace
        }
```

### ***`MintingEndpoint`***
The *`MintingEndpoint`* class is similar to *`ValidatorEndpoints`*, but is simpler due to the comparative simplicity of minting policies vs. validators. It consists of the following:
  * One associated data type: *`MintParam`*
  * One method, `mint`, which defines the off-chain endpoint action through which we can mint assets with the policy.

```haskell
instance MintingEndpoint MyMinting where
  data MintParam MyMinting = Mint
    { tokenName :: !TokenName,
      tokenQuantity :: !Integer
    }
    deriving (Generic, FromJSON, ToJSON)
  mint :: MintParam MyMinting -> ContractM MyMinting ()
  mint (Mint tokenName tokenQuantity)  = do
    submitAndConfirm
      Tx
        { lookups = scriptLookupsFor contract,
          constraints = mustMint contract tokenName tokenQuantity
        }
```

>**Note:** Off-chain endpoint code is more complex than on-chain predicate functions and is beyond the scope of our tutorial at this time: refer to the sample contracts containing off-chain emulation for more examples.

### **Define the test**
Now that we've implemented our endpoint actions, we're ready to define our emulator test. We begin by declaring a variable for our test (i.e. `test :: EmulatorTest`).

The test is constructed by calling the `initEmulator` function (imported from `Jambhala.Utils`). This function requires a type application (i.e. `@MyContract`) to disambiguate the type of the contract being emulated. It then receives two arguments: 
  1. The number of mock wallets the test requires (expressed as an integer literal)
  2. A comma-separated list of *`EmulatorAction`* values. 

*`EmulatorAction`* values primarily consist of (indirect) calls to the endpoints we defined in the *`ValidatorEndpoints`* or *`MintingEndpoint`* instance for our contract. These can be conveniently expressed using `fromWallet` and `toWallet` (for validator contracts) or `forWallet` (for minting contracts), which provide a pseudo-code like semantics when used with infix notation:

```haskell
test :: EmulatorTest
test =
  initEmulator @MyContract
    2
    [ Give {lovelace = 3_000_000} `fromWallet` 1,
      Grab `toWallet` 2
    ]
```

This test is initialized with 2 mock wallets. Wallet 1 gives 3 ADA to the script address, then Wallet 2 claims the gift.

*`EmulatorAction`* values can also be included in the list to simulate the passage of time. The `waitUntil` action takes a slot number and advances the emulated blockchain to that slot, which is useful for testing contracts involving deadlines (see `Vesting.hs` and `ParamVesting.hs`)

## **Using `j cli`**
Jambhala includes a simple command-line utility (`j cli`), which reduces boilerplate and provides a simple API for common contract-related tasks.

### **Listing contracts**
You can run the following command to view the names of available contracts in your project, for use with other commands:

```sh
j cli -l
```

### **Exporting contracts**
`j cli` can perform various operations on your contracts, including calculating its script hash, testing it using a blockchain emulator, and compiling it into a `.plutus` file. To do this we need to prepare a *`JambExports`* value in each of our contracts. Start by declaring a value of this type (i.e. `exports ::` *`JambExports`*).

Construct the exported contract using the `defExports` and `export` utility functions (imported from `Jambhala.Utils`):

```haskell
exports :: JambExports
exports = export (defExports contract)
```

The `defExports` (default exports) function takes a contract value and produces an *`ExportTemplate`* value containing the contract's name and its script. The result of this function is then passed to `export`, which constructs an export package compatible with the CLI.

#### **Adding data exports**
The *`JambExports`* can optionally include a list of *`DataExport`* values. These are sample values to supply as inputs during transaction construction with `cardano-cli`. Any value of a type with a *`ToData`* instance can be exported.

If data exports are included, `j cli` will produce serialised JSON versions of them along with your contract script when you use the `j cli -w` command. These optional exports are included as additional input to `defExports` using record update syntax and the `dataExports` attribute:

```haskell
exports :: JambExports
exports = 
  export 
    (defExports contract) -- Parentheses are required when using record update syntax
      { dataExports = 
        [
          () `toJSONfile` "unit",
          42 `toJSONfile` "forty-two"
        ]
      }
```
>**Note:** the value provided for `dataExports` must be a list, even if you are only exporting a single value.

In this example, running `j cli -w my-contract` will serialise `contract` into a `.plutus` file and save it to `cardano-cli-guru/assets/scripts/plutus/my-contract.plutus`. It will also produce a serialised JSON representation of a unit value (saved to `cardano-cli-guru/assets/data/unit.json`) and the integer 42 (saved to `cardano-cli-guru/assets/data/forty-two.json`).

#### **Adding emulator test**
An emulator test value (`::` *`EmulatorTest`*) can also be optionally included in the record input, using the `emulatorTest` attribute:

```haskell
exports :: JambExports
exports = 
  export 
    (defExports contract) 
      { dataExports = 
        [
          () `toJSONfile` "unit",
          42 `toJSONfile` "forty-two"
        ],
        emulatorTest = test
      }
```

#### **Adding to the contract map**  
Once we've completed our `exports` value, the final step is to make our contract visible to the CLI. We go to `src/Contracts/Contracts.hs` and import our contract's module as a qualified import, i.e.:

```haskell
import Contracts.MyContract qualified as MyContract
```

Then we add a new entry to the `contracts` list containing the `exports` value:

```haskell
contracts :: Contracts
contracts = [
    MyContract.exports
  ]
```

Once our contract has been added to the list, it can now be operated on by `j cli`.

### **Hashing scripts**
You can calculate the script hash for any available contract like this:

```sh
j cli -s CONTRACT
```

where `CONTRACT` is the name of the contract to hash.

### **Testing contracts with emulator trace**
You can run the emulator test defined for a contract with the following command:

```sh
j cli -t CONTRACT
```

where `CONTRACT` is the name of the contract to test.

### **Compiling contracts to `.plutus` files**
You can run the following command from the workspace terminal to write a contract to a `.plutus` file:

```sh
j cli -w CONTRACT [FILENAME]
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
## **Using GHCi**

To start a GHCi REPL session, run `j repl` and then load your contract:

```sh
j repl

Prelude Contracts λ > :m Contracts.MyContract
Prelude Contracts.MyContract λ >
```

***
## **Serving docs**

To launch a Hoogle server with docs for the Haskell packages this project is using, open a new bash terminal from the project root directory and run the following command:

```sh
j hoogle
```

The script will look up the specific `plutus-apps` revision hash from the `cabal.project` file, clone the `plutus-apps` repository (if it doesn't already exist) and checkout this revision, then launch a new `nix develop` shell and serve the docs at `http://0.0.0.0:8002/`.

To view the correct Haddock documentation for the revision you are using, open http://0.0.0.0:8002/haddock in your browser.

***
# **Updating Jambhala**
#### **🛠️ This section is under construction...**
Since Jambhala is under active development and is closely tracking the progress of `plutus-apps`, its codebase changes frequently.

## **Learning Mode**
If you created your repository by forking Jambhala (**Learning Mode**), you can update Jambhala using the `j update` command:

```sh
j update
```

This will fetch and merge changes from the upstream Jambhala repository, bringing your fork in sync while preserving any local changes you've made.

If you've made changes to any core Jambhala files (specifically `jambhala.cabal` and/or `src/Contracts.hs`), you may encounter a merge conflict that you'll need to resolve.

If you've made changes but not committed them, you can try `j update -f` to discard any uncommitted changes to `jambhala.cabal` and `src/Contracts.hs`. After this, you'll need to re-add your contract modules to `jambhala.cabal` and your contract imports/exports to `src/Contracts.hs`.

If you've committed changes to these files, you'll need to resolve the conflict yourself. The simplest way to do this is to copy/paste the latest contents from **[https://github.com/iburzynski/jambhala](https://github.com/iburzynski/jambhala)** for the conflicting files, commit your changes, and run `j update` again. After this, you'll need to re-add your contract modules to `jambhala.cabal` and your contract imports/exports to `src/Contracts.hs`.

## **Development Mode**
Unlike forks, Github repositories generated from templates have unique histories, so they aren't able to fetch and merge upstream changes as smoothly. However it's still possible to merge updates from an upstream template into your project with a little manual effort.

The Jambhala environment automatically adds the upstream template as a remote source. You can run the `j update` command to fetch any changes to the template and attempt to merge them:

```sh
j update
```
> *Note that this command is distinct from the `j cli -u` command discussed below, which updates only the `plutus-apps` dependency in `cabal.project`, not Jambhala itself.*

You will need to manually resolve any resulting merge conflicts.

***
# **Updating Plutus Dependencies**
The non-Hackage dependencies in the `cabal.project` file are following the **[plutus-apps](https://github.com/input-output-hk/plutus-apps)** library, with `sha256` hashes calculated for each `source-repository-package` entry.

`j cli` provides a utility to easily update `plutus-apps` to the most recent revision and adjust all related dependencies. Run the `j cli -u` command to pull the latest revision and generate a new `cabal.project` file.

```sh
j cli -u
```

🚨 **WARNING!** This operation rewrites your `cabal.project` file according to the most recent `plutus-apps` commit, and may cause your environment and/or contracts to break. You should use at your own risk, but you can also easily restore a previous `cabal.project` file by following the instructions for **Restoring a previous version** below.
***
## **Set `plutus-apps` to a specific commit/tag**
You can also use the `j cli -u` command with an additional argument to set `plutus-apps` to a specific commit hash or tag:

```sh
j cli -u 38979da68816ab84faf6bfec6d0e7b6d47af651a
```

You can run the `j pa-history` command to view the full commit history for `plutus-apps`:

```sh
j pa-history
```

Use the up/down keys to navigate or type `q` to quit.

🚨 **WARNING!** The code in the sample contracts and `Jambhala.Plutus` module have been designed for compatibility with very recent commits of `plutus-apps` - this means pointing `plutus-apps` to older tags/commits is much more likely to result in breakage. *Use this feature at your own risk!*

***
## **Restoring a previous version**
Before the `j cli -u` command rewrites your `cabal.project` file, a backup of your existing `cabal.project` will be created in the `backups/` directory in case you need to roll back the update. Just delete the current `cabal.project` file, copy the backup file and rename it to `cabal.project`. Then run `direnv allow` or reload the project in VS Code and your previous project state will be restored.

***
## **Manually updating dependencies**
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
Additional issues and solutions will be documented here as they're encountered during public testing.

**Nix stops working after updating Mac OS**  
Nix installation on Macs appends a code snippet to `/etc/zshrc` and `/etc/bashrc`, which is required for Nix to work properly with `zsh` and `bash` shells. Unfortunately one or both of these additions may be erased after updating your Mac.

This will break:
 * all Nix commands
 * any Jambhala commands that use Nix under the hood
 * if you installed `cardano-node`/`cardano-cli` using Nix (i.e. via **Cardano EZ-Installer**), the `cardano-node`/`cardano-cli` commands and associated aliases to start the node

To resolve the issue, edit `~/.zprofile`, `~/.zshrc`, `~/.bash_profile`, and `~/.bashrc`, adding the following snippet to the top of each file:

```sh
# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix
```

>**NOTE:** make sure this snippet is above the `direnv` hook in these files. You must then restart the shell for the changes to take effect.

**VS Codium/Code doesn't provide Haskell IDE support: hangs with `Processing: 1/x`**: 

It's possible the project dependencies haven't been properly built via `cabal`, which is a requirement for Haskell Language Server support in VS Codium/Code. Run `cabal build` to build the dependencies.

### **General Troubleshooting Techniques**
- **Restart Haskell LSP Server:** restarting `haskell-language-server` often fixes common issues with Haskell in VS Code. Open the command palette (`Ctrl + Shift + p`) and begin typing `Restart Haskell LSP Server` until you see this option, and select it. In the future it will be pinned to the top of the command palette options and easier to find.
- **`j rebuild`:** cleaning the `.direnv`, `.cabal`, and `dist-newstyle` directories of all build artifacts and rebuilding the project may resolve certain issues

  ```sh
  j rebuild
  ```

  Note that it will be time-consuming to rebuild the project from scratch, so be sure to exhaust all other troubleshooting options before attempting.
  
- **Delete `~/.local/state/cabal/store`:** if you have pre-existing Haskell tooling installed system-wide (i.e. via GHCup, or the Haskell extension in VS Codium/Code), it's possible that build artifacts from Cabal can conflict with the Jambhala environment, resulting in Cabal errors or preventing IDE features from Haskell Language Server to work correctly in the editor. Removing `~/.local/state/cabal/store` can resolve such errors (the contents of this directory are always safe to remove and will not break any functionality). After removing the directory, run the `jrebuild` command to rebuild the project with Cabal before trying again.

**For assistance or bug reporting, file an issue or email `ian.burzynski@emurgo.io`.**