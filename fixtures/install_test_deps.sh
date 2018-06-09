#!/bin/bash

install_test_deps() {
    [[ -d /nix ]] || (curl https://nixos.org/nix/install | sh)
    nix-env -i cabal2nix
    nix-env -i nix-prefetch-git
}

install_test_deps
