#!/bin/bash
set -euo pipefail

test() {
    local nonOptions=("ignore" "andIgnore" "reallyIgnore")
    local v
    for v in "${nonOptions[@]}"
    do
        local option_file="nix/options/${v}"
        [[ -f "$option_file" ]] && {
            echo "Fail: found a config file for unrecognized option '$v'"
            return 1
        }
    done

    _has_config "dontCheck" "optparse-applicative turtle"
    _has_config "doJailbreak" "foldl turtle"
    _has_config "dontHaddock" "foldl turtle"
}

_has_config() {
    local option=${1:-''}
    local pkgs=${2:-''}
    [[ -z $pkgs ]] && return 1;

    local option_file="nix/options/${option}"
    [[ -f "$option_file" ]] || {
        echo "Fail: expected a config file for option $option; it's missing"
        echo
        ls -lR nix
        return 1
    }
    local p
    for p in $pkgs
    do
        grep -q $p $option_file || {
            echo "Fail: expected $option_file to contain $p"
            return 1
        }
    done
}

test
