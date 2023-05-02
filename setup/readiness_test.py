#!/usr/bin/env python3

import getpass, json, os, subprocess, sys

# Check if Nix is installed
try:
    subprocess.check_output(["nix", "--version"])
except OSError:
    print("Error: Nix is not installed on this system.")
    sys.exit(1)

def get_nix_conf_checks():
    required_experimental_features = ['nix-command', 'flakes']
    required_substituters = [
        'https://cache.iog.io',
        'https://iohk.cachix.org',
        'https://cache.zw3rk.com',
        'https://cache.nixos.org/'
        ]
    required_trusted_keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=",
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=",
        "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
        ]
    nix_conf_checks = {
        "experimental-features": required_experimental_features,
        "substituters": required_substituters,
        "trusted-public-keys": required_trusted_keys
        }
    
    return nix_conf_checks

# Formatting helpers
ind  = lambda str, n=1: "  " * n + str
ind2 = lambda str: ind(str, 2)
mk_color_text = lambda code, str: f"\033[{code}m{str}\033[0m"
mk_neutral_text = lambda str: mk_color_text(93, str)
print_success = lambda str: print(mk_color_text(92, str))
print_neutral = lambda str: print(mk_neutral_text(str))
print_fail = lambda str: print(mk_color_text(91, str))

def prompt_install_direnv():
    response = input(ind2(mk_neutral_text("Install direnv with Nix? (Y/n): ")))
    return (False, True if not response.strip() else response.lower() == 'y')

def check_direnv_version():
    try:
        result = subprocess.run(['direnv', '--version'], capture_output=True, text=True, check=True)
        output = result.stdout.strip()
        version = tuple(map(int, output.split('.')))
        if version >= (2, 30):
            print_success(ind2(f"direnv version: {output}"))
            return (True, False)
        else:
            print_fail(ind2(f"direnv {output} is below the required version (2.30+)."))
            return prompt_install_direnv()
    except FileNotFoundError:
            print_fail(ind2(f"direnv is not installed."))
            return prompt_install_direnv()

def check_direnv():
    print_neutral(ind("> Checking direnv..."))
    direnv_installed, install_direnv = check_direnv_version()
    bashrc_path = os.path.expanduser('~/.bashrc')
    bashrc_exists = os.path.isfile(bashrc_path)
    ready = True

    if not direnv_installed and not install_direnv:
        ready = False

    if install_direnv:
        result = subprocess.run(['nix-env', '-iA', 'nixpkgs.direnv'], stderr=subprocess.PIPE)

        if result.returncode == 0:
            print_neutral(ind2("direnv installed successfully."))
        else:
            print_fail(ind2(f"direnv installation failed with {result.stderr.decode('utf-8')}"))
            ready = False

    if not bashrc_exists:
        print_neutral(ind2(f"Creating .bashrc file at '{bashrc_path}'..."))
        try:
            open(bashrc_path, 'a').close()
        except:
            ready = False
    try:
        with open(bashrc_path, 'r+') as bashrc_file:
            if 'eval "$(direnv hook bash)"' not in bashrc_file.read():
                print_neutral(ind2("Hooking direnv into bash shell..."))
                bashrc_file.write('\neval "$(direnv hook bash)"\n')
    except:
        ready = False

    if ready:
        print_success(ind("> direnv: PASSED"))
    else:
        print_fail(ind("> direnv: FAILED"))
    return ready

def check_trusted_user(nix_conf_json):
    trusted_users = nix_conf_json["trusted-users"]["value"]
    username = getpass.getuser()
    result = "PASSED" if username in trusted_users else "FAILED"
    report = ind(f"> trusted-user: {result}")
    if result == "PASSED":
        print_success(report)
        return True
    else:
        print_fail(report)
        print_fail(ind(f"Current user {username} is missing from trusted-users in nix.conf."))
        return False

def check(nix_conf_json, attribute, required):
    current_set  = set(nix_conf_json[attribute]["value"])
    required_set = set(required)
    passed, missing = (required_set.issubset(current_set), required_set.difference(current_set))
    result = "PASSED" if passed else "FAILED"
    report = ind(f"> {attribute}: {result}")
    if passed:
        print_success(report)
        return True
    else:
        print_fail(report)
        print_fail(ind2(f"The following {attribute} are missing in nix.conf:\n"))
        for val in missing: print_fail(ind2(f"{val}"))
        print("")
        return False

def check_nix_conf():
    print_neutral(f'\n{ind("> Checking nix.conf...")}')
    
    show_config_output = subprocess.check_output(["nix", "show-config", "--json"])
    nix_conf_json = json.loads(show_config_output)
    nix_conf_checks = get_nix_conf_checks()

    return check_trusted_user(nix_conf_json) and all([check(nix_conf_json, k, v) for k, v in nix_conf_checks.items()])

def test_readiness():
  print(ind("JAMBHALA READINESS TEST:\n"))
  direnv_passed = check_direnv()
  nix_conf_passed = check_nix_conf()

  if direnv_passed and nix_conf_passed:
      print_success(f'\n{ind("All checks passed. Open a new terminal window and enter your project directory to proceed with Jambhala installation.")}')
  else:
      print_fail(f'\n{ind("Readiness Test failed: correct the issue(s) above and re-run the Test before proceeding with installation.")}')

test_readiness()