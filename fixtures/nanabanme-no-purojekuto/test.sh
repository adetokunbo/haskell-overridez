#!/bin/bash

test() {
    nix-build --no-out-link || return 0
    test_desc="$(cat DESC)"
    echo
    echo "FAILED: '$test_desc' in $(pwd)"
    echo
    return 1
}

test
