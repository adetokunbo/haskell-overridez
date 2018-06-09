#!/bin/bash

install_test_deps() {
    nix-env -i cabal2nix
    nix-env -i nix-prefetch-git
}

install_test_deps
