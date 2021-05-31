#!/usr/bin/env bash

set -euo pipefail

curPwd="$(pwd)"
root="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
stackyaml="$root/stack.yaml"

stack --stack-yaml "$stackyaml" build

set -x
stack --stack-yaml "$stackyaml" \
    exec yacp-exe -- \
    "$@"
>&2 times
