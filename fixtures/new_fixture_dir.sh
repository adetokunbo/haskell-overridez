#!/bin/bash
set -euo pipefail

new-fixture-dir() {
    local subst=$1
    local initial_prefix=${2:-'yonbanme-no'}
    local initial="${initial_prefix}-purojekuto"
    local updated="${subst}-purojekuto"

    local this_dir=$(dirname "${BASH_SOURCE[${#BASH_SOURCE[@]} - 1]}")
    cp -Rv $this_dir/$initial $this_dir/$updated
    mv $this_dir/$updated/{$initial,$updated}.cabal
    sed -i'.bak' -e "s/$initial/$updated/g" $this_dir/$updated/$updated.cabal
    sed -i'.bak' -e "s/$initial/$updated/g" $this_dir/$updated/default.nix
    rm -v $this_dir/$updated/*.bak
}

new-fixture-dir "$@"
