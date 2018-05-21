#!/bin/bash
set -euo pipefail

setup_wo_options() {
    # Save the configuration without options
    export HOZ_OPTS=
    haskell-overridez https://github.com/pcapriotti/optparse-applicative
    haskell-overridez https://github.com/Gabriel439/Haskell-Turtle-Library
    haskell-overridez https://github.com/Gabriel439/Haskell-Foldl-Library
    haskell-overridez https://github.com/Gabriel439/Haskell-Managed-Library
}

setup_with_options() {
    local cwd=$(pwd)
    local test_name="${cwd##*/}"

    # Set up the configuration with options

    # Make a directory under the tmp root in which to save the config with options
    local dst_dir="${HOZ_TMP_DIR}/dst/${test_name}"
    mkdir -p $dst_dir

    # Save the configuration
    export HOZ_OPTS=dontCheck
    haskell-overridez -o $dst_dir https://github.com/pcapriotti/optparse-applicative
    export HOZ_OPTS=doJailbreak
    haskell-overridez -o $dst_dir https://github.com/Gabriel439/Haskell-Turtle-Library
    export HOZ_OPTS=dontHaddock
    haskell-overridez -o $dst_dir https://github.com/Gabriel439/Haskell-Foldl-Library
    haskell-overridez -o $dst_dir https://github.com/Gabriel439/Haskell-Managed-Library

    (cat <<EOF
let
  overridez = import ./haskell-overridez.nix;
in
  overridez.allIn ${dst_dir}/nix
EOF
    ) > ./nix/indirect_import.nix
}

setup_test() {
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir > /dev/null
    local cwd=$(pwd)
    local test_name="${cwd##*/}"
    setup_wo_options
    setup_with_options
    popd
}

setup_test