#!/bin/bash
set -euo pipefail

setup_test() {
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir > /dev/null
    local cwd=$(pwd)
    local out_dir="${HOZ_TMP_DIR}/${cwd##*/}"
     mkdir -p $out_dir

    # fetch the initial version of the github example, save it locally, and use
    # that as a local git repo (needs all the flags belows; plus the revision and hash)
    nix-prefetch-git --quiet \
                     --out $out_dir \
                     --builder \
                     --leave-dotGit \
                     https://github.com/adetokunbo/example-fetched-haskell-overridez \
                     bf8ec35e984fa0acf930bc5a39299cfc1079a761 \
                     12fynjncjjgi20jksmblq3pcrnhhwrdi2cajh7brhr7bsx87dssm
    ls -l $out_dir

    $HOZ_TEST_CMD fetch "file://${out_dir}"
    popd > /dev/null
}

setup_test
