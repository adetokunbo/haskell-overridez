#!/bin/bash
set -euo pipefail
setup_test() {
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir > /dev/null
    local cwd=$(pwd)
    local out_dir="${HOZ_TMP_DIR}/${cwd##*/}"
     mkdir -p $out_dir

    # fetch a recent version of the github example, save it locally, and use
    # that as a local git repo (needs all the flags belows; plus the revision and hash)
    nix-prefetch-git --quiet \
                     --out $out_dir \
                     --builder \
                     --leave-dotGit \
                     https://github.com/adetokunbo/example-fetched-haskell-overridez \
                     e1dc13ae7dcb3a4c4b7f426a94ecf523532ea574 \
                     0sapkc9mpk95dlq6n87z8xa8w4r0cnxxpnyp971m5phjpbn83c7q \
    ls -l $out_dir

    $HOZ_TEST_CMD fetch "file://${out_dir}"
    popd > /dev/null
}

setup_test
