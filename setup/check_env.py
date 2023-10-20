import os

CWD = os.getcwd()
DEVNET_ENVRC = os.path.join(CWD, "cardano-devnet", ".envrc")
JAMB_ENV = os.path.join(CWD, ".env")
JAMB_ENVRC = os.path.join(CWD, ".envrc")
GURU_ENV = os.path.join(CWD, "cardano-cli-guru", ".env")
GURU_ENVRC = os.path.join(CWD, "cardano-cli-guru", ".envrc")


env_variables = {
    "ADDR_PATH": GURU_ENV,
    "BLOCKFROST_PROJECT_ID_MAINNET": GURU_ENV,
    "BLOCKFROST_PROJECT_ID_PREPROD": GURU_ENV,
    "BLOCKFROST_PROJECT_ID_PREVIEW": GURU_ENV,
    "CARDANO_ASSETS_PATH": GURU_ENV,
    "CARDANO_NODE_NETWORK_ID": GURU_ENV,
    "CARDANO_PATH": JAMB_ENV,
    "CARDANO_SRC_PATH": JAMB_ENV,
    "DATA_PATH": GURU_ENV,
    "DEVNET_ENV_LOADED": DEVNET_ENVRC,
    "GITHUB_ID": JAMB_ENV,
    "GURU_ENV_LOADED": GURU_ENVRC,
    "JAMB_ENV_LOADED": JAMB_ENVRC,
    "KEYS_PATH": GURU_ENV,
    "NATIVE_SCRIPTS_PATH": GURU_ENV,
    "NODE_RELEASE": JAMB_ENV,
    "OGMIOS_RELEASE": JAMB_ENV,
    "PARAMS_PATH": GURU_ENV,
    "PLUTUS_SCRIPTS_PATH": GURU_ENV,
    "PROJECT_ROOT": JAMB_ENVRC,
    "TX_PATH": GURU_ENV,
    "VIM_MODE": JAMB_ENV,
}


def get_env_values():
    print("  ***** \033[35mJAMBHALA ENVIRONMENT VARIABLES\033[0m *****\n")
    print(
        "  \033[91mNOTE:\033[0m Only variables in .env files should be set manually.")
    print("        Contents of .envrc files should not be modifed!")
    print("        Run `refresh` script in terminal to refresh environment after changing variables.\n")
    for var_name, var_src in env_variables.items():
        if var_name in os.environ.keys():
            print(
                f"  * \033[35m{var_name}\033[0m={os.environ[var_name]} ('{var_src}')")
        else:
            print(f"  * \033[91m{var_name}\033[0m ('{var_src}')")


get_env_values()
