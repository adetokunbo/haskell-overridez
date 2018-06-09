#!/bin/bash

install_nix() {
    [[ -d $HOME/.nix-defexpr ]] || {
        [[ -d /nix ]] && rm -fR /nix
        curl https://nixos.org/nix/install | sh
    }
}

install_nix
