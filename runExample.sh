#!/usr/bin/env bash

set -exuo pipefail

stack build
stack exec yacp-exe -- \
  --ort "test/data/analyzer-result.json" \
  --sc "test/data/bat.scancode.pp.json" \
  --sc "test/data/black.scancode.pp.json" \
  --spdx "data/spdx-spdx-spec/examples/SPDXJSONExample-v2.2.spdx.json" \
  "_tmp"