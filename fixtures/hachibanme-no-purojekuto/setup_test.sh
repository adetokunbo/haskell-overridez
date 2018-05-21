#!/bin/bash
set -euo pipefail

setup_test() {
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir > /dev/null
    local cwd=$(pwd)
    local test_name="${cwd##*/}"

    # Make a directory under the tmp root in which to save the output
    local dst_dir="${HOZ_TMP_DIR}/dst/${test_name}"
    mkdir -p $dst_dir

    # Set up the configuration
    HOZ_OPTS=dontCheck
    haskell-overridez -o $dst_dir https://github.com/pcapriotti/optparse-applicative
    HOZ_OPTS=doJailbreak
    haskell-overridez -o $dst_dir https://github.com/Gabriel439/Haskell-Turtle-Library
    haskell-overridez -o $dst_dir https://github.com/Gabriel439/Haskell-Foldl-Library
    haskell-overridez -o $dst_dir https://github.com/Gabriel439/Haskell-Managed-Library

    (cat <<EOF
let
  overridez = import ./haskell-overridez.nix;
in
  overridez.allIn ${dst_dir}/nix
EOF
    ) > ./nix/indirect_import.nix

    popd > /dev/null
}

setup_test
