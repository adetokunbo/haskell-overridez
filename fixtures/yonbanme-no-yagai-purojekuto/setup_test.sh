#!/bin/bash
set -euo pipefail

setup_test() {
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir > /dev/null
    local cwd=$(pwd)
    local test_name="${cwd##*/}"
    local out_dir="${HOZ_TMP_DIR}/${test_name}"
    mkdir -p $out_dir

    # fetch the recent version of the github example, save it locally, and use
    # that as a local git repo (needs all the flags belows; plus the revision and hash)
    nix-prefetch-git --quiet \
                     --out $out_dir \
                     --builder \
                     --leave-dotGit \
                     https://github.com/adetokunbo/example-fetched-haskell-overridez \
                     e1dc13ae7dcb3a4c4b7f426a94ecf523532ea574 \
                     0sapkc9mpk95dlq6n87z8xa8w4r0cnxxpnyp971m5phjpbn83c7q \
    ls -l $out_dir

    # Make a directory under the tmp root in which to save the output
    local dst_dir="${HOZ_TMP_DIR}/dst/${test_name}"
    mkdir -p $dst_dir
    (cat <<EOF
let
  overridez = import ./haskell-overridez.nix;
in
  overridez.allIn ${dst_dir}/nix/localhost/${test_name}
EOF
    ) > ./nix/indirect_import.nix
    $HOZ_TEST_CMD -o $dst_dir fetch "file://${out_dir}"
    [[ -d $dst_dir ]] && ls -l $dst_dir
    popd > /dev/null
}

setup_test
