#!/bin/bash

install_test_deps() {
    nix-env -i cabal2nix
}

install_test_deps
