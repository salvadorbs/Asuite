#!/usr/bin/env bash
# Build and run the ASuite test suites.
#
#   ASuiteTests     : FPCUnit console tests, no display or widgetset required.
#   ASuiteGuiTests  : FPCUnit tests that build LCL controls; need a display
#                     server (xvfb-run is used automatically when available).
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

LAZBUILD="${LAZBUILD:-lazbuild}"
WIDGETSET="${WIDGETSET:-gtk3}"

echo "== ASuiteTests =="
"$LAZBUILD" ASuiteTests.lpi
./ASuiteTests --all --format=plain

echo "== ASuiteGuiTests =="
"$LAZBUILD" --widgetset="$WIDGETSET" ASuiteGuiTests.lpi
if [ -z "${DISPLAY:-}" ] && command -v xvfb-run >/dev/null 2>&1; then
  xvfb-run -a ./ASuiteGuiTests --all --format=plain
else
  ./ASuiteGuiTests --all --format=plain
fi
