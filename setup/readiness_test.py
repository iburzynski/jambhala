#!/usr/bin/env python3

import json
import os
import subprocess
import sys
import getpass

# Check if Nix is installed
try:
    subprocess.check_output(["nix", "--version"])
except OSError:
    print("Error: Nix is not installed on this system.")
    sys.exit(1)

required_experimental_features = ['nix-command', 'flakes']
required_substituters = [
    'https://cache.iog.io',
    'https://iohk.cachix.org',
    'https://cache.zw3rk.com',
    'https://cache.nixos.org/'
    ]
required_trusted_public_keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=",
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=",
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ]

def mk_color_text(code, str):
    return f"\033[{code}m{str}\033[0m"

def mk_success_text(str):
    return mk_color_text(92, str)

def mk_neutral_text(str):
    return mk_color_text(93, str)

def mk_fail_text(str):
    return mk_color_text(91, str)

def check_direnv_version():
    result = subprocess.run(['direnv', '--version'], capture_output=True, text=True)
    output = result.stdout.strip()
    version = tuple(map(int, output.split('.')))
    return True if version >= (2, 30) else False

def check_direnv():
    print(mk_neutral_text("\t> Checking direnv..."))
    direnv_installed = check_direnv_version()
    bashrc_path = os.path.expanduser('~/.bashrc')
    bashrc_exists = os.path.isfile(bashrc_path)
    ready = True

    if not direnv_installed:
        print(mk_neutral_text("\t\tInstalling direnv using Nix..."))
        try:
            subprocess.run(['nix-env', '-iA', 'nixpkgs.direnv'])
        except:
            ready = False

    if not bashrc_exists:
        print(mk_neutral_text(f"\t\tCreating .bashrc file at '{bashrc_path}'..."))
        try:
            open(bashrc_path, 'a').close()
        except:
            ready = False
    try:
        with open(bashrc_path, 'r+') as bashrc_file:
            if 'eval "$(direnv hook bash)"' not in bashrc_file.read():
                print(mk_neutral_text("\t\tHooking direnv into bash shell..."))
                bashrc_file.write('\neval "$(direnv hook bash)"\n')
    except:
        ready = False

    if ready:
        print(mk_success_text("\t> direnv: PASSED"))
    return ready

def get_nix_conf_json():
    command_output = subprocess.check_output(["nix", "show-config", "--json"])
    return json.loads(command_output)

def is_trusted_user(trusted_users):
    username = getpass.getuser()
    result = "PASSED" if username in trusted_users else "FAILED"
    report = f"\t> trusted-user: {result}"
    if result == "PASSED":
        print(mk_success_text(report))
        return True
    else:
        print(mk_fail_text(report))
        print(mk_fail_text(f"\tCurrent user {username} is missing from trusted-users in nix.conf."))
        return False

def check(nix_conf_json, attribute, required):
    current_set  = set(nix_conf_json[attribute]["value"])
    required_set = set(required)
    passed, missing = (required_set.issubset(current_set), required_set.difference(current_set))
    result = "PASSED" if passed else "FAILED"
    report = f"\t> {attribute}: {result}"
    if passed:
        print(mk_success_text(report))
        return True
    else:
        print(mk_fail_text(report))
        print(mk_fail_text(f"\t\tThe following {attribute} are missing in nix.conf:\n"))
        for val in missing:
            print(mk_fail_text(f"\t\t{val}"))
        print("")
        return False

def check_experimental_features(nix_conf_json):
    return check(nix_conf_json, "experimental-features", required_experimental_features)

def check_substituters(nix_conf_json):
    return check(nix_conf_json, "substituters", required_substituters)

def check_trusted_keys(nix_conf_json):
    return check(nix_conf_json, "trusted-public-keys", required_trusted_public_keys)

def test_readiness():
  print("\tJambhala Readiness Test:\n")
  direnv_passed = check_direnv()
  print(mk_neutral_text("\n\t> Checking nix.conf..."))
  nix_conf_json = get_nix_conf_json()
  trusted_users = nix_conf_json["trusted-users"]["value"]
  checks = [
    direnv_passed,
    is_trusted_user(trusted_users),
    check_experimental_features(nix_conf_json),
    check_substituters(nix_conf_json),
    check_trusted_keys(nix_conf_json)
    ]

  if all(checks):
      print(mk_success_text("\n\tAll checks passed. Open a new terminal window and enter your project directory to proceed with Jambhala installation."))
  else:
      print(mk_fail_text("\n\tReadiness Test failed: correct the issue(s) above and re-run the Test before proceeding with installation."))

test_readiness()