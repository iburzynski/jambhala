#!/usr/bin/env python3

import getpass
import json
import os
import platform
import subprocess
import sys
from functools import partial
from typing import Any, Callable, TypedDict

# Check if Nix is installed
try:
    subprocess.check_output(["nix", "--version"])
except OSError:
    print("Error: Nix is not installed on this system.")
    sys.exit(1)

RequiredAttrs = TypedDict('RequiredAttrs', {
    "flags": list[str],
    "experimental-features": list[str],
    "substituters": list[str],
    "trusted-public-keys": list[str]
})


def get_required_attributes() -> RequiredAttrs:
    return {
        "flags": ['keep-derivations', 'keep-outputs'],
        "experimental-features": ['nix-command', 'flakes'],
        "substituters": [
            "https://cache.nixos.org/",
            "https://cache.zw3rk.com",
        ],
        "trusted-public-keys": [
            "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=",
            "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
        ]
    }


# Formatting helpers
def ind(txt: str, n: int = 1) -> str: return "  " * n + txt
def ind2(txt: str) -> str: return ind(txt, 2)
def mk_color_text(
    code: int, txt: str) -> str: return f"\033[{code}m{txt}\033[0m"


def mk_neutral_text(txt: str) -> str: return mk_color_text(93, txt)
def print_success(txt: str) -> None: return print(mk_color_text(92, txt))
def print_neutral(txt: str) -> None: return print(mk_neutral_text(txt))
def print_fail(txt: str) -> None: return print(mk_color_text(91, txt))


def print_report(attr: str, passed: bool) -> None:
    printer = print_success if passed else print_fail
    printer(ind(f"> {attr}: {'PASSED' if passed else 'FAILED'}"))


# Direnv check/install
DirenvStatus = TypedDict('DirenvStatus', {
                         'installed': bool, 'install?': bool})


def prompt_install_direnv() -> DirenvStatus:
    response = input(ind2(mk_neutral_text("Install direnv with Nix? (Y/n): ")))
    return {
        "installed": False,
        "install?": True if not response.strip() else response.lower() == 'y'}


def get_direnv_status() -> DirenvStatus:
    try:
        result = subprocess.run(['direnv', '--version'],
                                capture_output=True, text=True, check=True)
        output = result.stdout.strip()
        version = tuple(map(int, output.split('.')))
        if version >= (2, 30):
            print_success(ind2(f"direnv version: {output}"))
            return {
                "installed": True,
                "install?": False
            }
        else:
            print_fail(
                ind2(f"direnv {output} is below the required version (2.30+)."))
            return prompt_install_direnv()
    except FileNotFoundError:
        print_fail(ind2(f"direnv is not installed."))
        return prompt_install_direnv()


def check_direnv() -> bool:
    print_neutral(ind("> Checking direnv..."))
    direnv_status = get_direnv_status()
    installed = direnv_status["installed"]
    installing = direnv_status["install?"]
    ready: bool = True

    if not installed and not installing:
        ready = False

    if installing:
        result = subprocess.run(
            ['nix-env', '-iA', 'nixpkgs.direnv'], stderr=subprocess.PIPE)

        if result.returncode == 0:
            print_neutral(ind2("direnv installed successfully."))
        else:
            err = result.stderr.decode('utf-8')
            print_fail(
                ind2(f"direnv installation failed with {err}"))
            ready = False

    if installed or installing:
        dotfiles = ['.bash_profile', '.bashrc', '.zprofile',
                    '.zshrc'] if platform.system() == 'Darwin' else ['.bashrc']
        bash_hook = 'eval "$(direnv hook bash)"\n'
        zsh_hook = 'eval "$(direnv hook zsh)"\n'
        hooks = {
            '.bashrc': bash_hook,
            '.bash_profile': bash_hook,
            '.zshrc': zsh_hook,
            '.zprofile': zsh_hook
        }
        for dotfile in dotfiles:
            try:
                file_path = os.path.expanduser(f"~/{dotfile}")
                if not os.path.exists(file_path):
                    print_neutral(ind2(f"> Creating '{file_path}' file..."))
                    open(file_path, 'a').close()

                print_neutral(
                    ind(f"> Adding direnv hook to '{file_path}'"))

                with open(file_path, 'r') as f:
                    lines = f.readlines()

                    hook = hooks[dotfile]
                    dotfile_contents = [
                        line for line in lines if
                        not line.startswith('eval "$(direnv hook')] + [hook]

                with open(file_path, 'w') as f:
                    f.writelines(dotfile_contents)
            except:
                print_fail(ind2("Unable to configure .bashrc file"))
                ready = False

    print_report("direnv", ready)

    return ready


# Check Nix config
def check_attr(
        nix_conf_json: dict[str, Any],
        attribute: str, pred: Callable[[Any],
                                       tuple[bool, Any]],
        error_details: str):
    attr_val = nix_conf_json[attribute]["value"]
    passed, extra_data = pred(attr_val)
    print_report(attribute, passed)
    if not passed:
        print_fail(ind2(error_details))

    return (passed, extra_data)


def check_trusted_user(nix_conf_json: dict[str, Any]) -> bool:
    user = getpass.getuser()
    err = f"'trusted-users = root {user}' is missing in nix.conf."
    passed, _ = check_attr(
        nix_conf_json,
        "trusted-users",
        lambda users: ("root" in users and user in users, None),
        err)

    return passed


def check_flag_attr(nix_conf_json: dict[str, Any], attribute: str) -> bool:
    err = f"'{attribute} = true' missing in nix.conf."
    passed, _ = check_attr(nix_conf_json, attribute, lambda v: (v, None), err)

    return passed


def check_set_attr(
        nix_conf_json: dict[str, Any],
        required_attrs: RequiredAttrs, attribute: str) -> bool:

    def pred(attr_val: list[str]) -> tuple[bool, set[str]]:
        cur_set = set(attr_val)
        req_set = set(required_attrs[attribute])
        return (req_set.issubset(cur_set), req_set.difference(cur_set))

    err = f"The following {attribute} are missing in nix.conf:\n"
    passed, missing = check_attr(nix_conf_json, attribute, pred, err)

    if not passed:
        for val in missing:
            print_fail(ind(f"{val}", 3))
        print("")

    return passed


def check_nix_conf() -> bool:
    print_neutral(f'\n{ind("> Checking nix.conf...")}')
    nix_conf_output = subprocess.check_output(["nix", "show-config", "--json"])
    nix_conf_json = json.loads(nix_conf_output)
    req_attributes = get_required_attributes()
    check_set_attr_ = partial(check_set_attr, nix_conf_json, req_attributes)
    check_flag_attr_ = partial(check_flag_attr, nix_conf_json)
    attrs = ["experimental-features"]
    passed = all([
        *[check_set_attr_(a) for a in attrs],
        check_trusted_user(nix_conf_json),
        all([check_flag_attr_(a)
             for a in req_attributes["flags"]])
    ])

    return passed


def test_readiness() -> None:
    print(ind("JAMBHALA READINESS TEST:\n"))
    direnv_passed = check_direnv()
    nix_conf_passed = check_nix_conf()
    passed = direnv_passed and nix_conf_passed

    if passed:
        print_success(
            f'\n{ind("All checks passed. Open a new terminal window and enter your project directory to proceed with Jambhala installation.")}')
    else:
        print_fail(
            f'\n{ind("Readiness Test failed: correct the issue(s) above and re-run the Test before proceeding with installation.")}')
        sys.exit(1)


test_readiness()
