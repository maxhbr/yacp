#!/usr/bin/env bash

set -exuo pipefail

stack build
stack exec yacp-exe -- \
  --component-detection "test/data/component-detection/component-detection.json" \
  --fosslight "test/data/fosslight/fosslight_dependency-Report_SRC.csv" \
  --it-depends "test/data/it-depends/it-depends.json" \
  --ort "test/data/ort/evaluated-model.json" \
  --syft "test/data/syft/report.json" \
  --scancode "test/data/scancode/scancode.pp.json" \
  "_tmp"
