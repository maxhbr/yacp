#!/usr/bin/env bash

curPwd="$(pwd)"
root="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
stackyaml="$root/stack.yaml"

trap times EXIT

stack --stack-yaml "$stackyaml" build
time stack --stack-yaml "$stackyaml" \
    exec -- yacp-exe \
    "$@"
