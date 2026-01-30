#!/usr/bin/env bash
set -e

echo "Running Q1 batch tests"

CONFIG="config/functions_q1.json"

jq -c '.[]' "$CONFIG" | while read -r row; do
  PKG=$(echo "$row" | jq -r '.package')
  FUN=$(echo "$row" | jq -r '.function')

  echo
  echo "----------------------------------------"
  echo "Testing $PKG::$FUN"
  echo "----------------------------------------"

  python scripts/get_predicted_deps.py "$PKG" "$FUN" > predicted_deps.json

  Rscript scripts/run_one_q1.R \
    "$PKG" \
    "$FUN" \
    "fixtures/q1_inputs/${FUN}_cases.json"
done
