#!/bin/bash
set -euo pipefail

setup_test() {
    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    pushd $this_dir > /dev/null
    local cwd=$(pwd)
    local out_dir="${HOZ_TMP_DIR}/${cwd##*/}"
    local out_dir=$(mktemp -d)
    mkdir -p $out_dir

    # fetch the initial version of the github example, save it locally, and use
    # that as a local git repo (needs all the flags belows; plus the revision and hash)
    nix-prefetch-git --quiet \
                     --out $out_dir \
                     --builder \
                     --leave-dotGit \
                     https://github.com/adetokunbo/example-fetched-haskell-overridez \
                     46f4f2197ee9b59091d6bbe30a47a74772806b61 \
                     0w3jqim7xnrs2fg3cg60sm8k9i8fr8r1lwlwglmrmwqfbxzfxxx3
    ls -l $out_dir

    haskell-overridez fetch "file://${out_dir}"
    popd > /dev/null
}

setup_test
