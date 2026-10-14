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

# FPC 3.2.2 sometimes dies with an internal error while compiling big GUI
# units and leaves corrupt .ppu files behind, which then break every later
# incremental build deterministically. On failure, wipe the generated unit
# output and retry once from clean state.
build_gui_tests() {
  if "$LAZBUILD" --widgetset="$WIDGETSET" ASuiteGuiTests.lpi; then
    return 0
  fi
  echo "GUI build failed; cleaning generated units and retrying once..."
  rm -rf "$SCRIPT_DIR/lib"
  "$LAZBUILD" --widgetset="$WIDGETSET" ASuiteGuiTests.lpi
}

echo "== ASuiteTests =="
"$LAZBUILD" ASuiteTests.lpi
./ASuiteTests --all --format=plain

echo "== ASuiteGuiTests =="
build_gui_tests
if [ -z "${DISPLAY:-}" ] && command -v xvfb-run >/dev/null 2>&1; then
  xvfb-run -a ./ASuiteGuiTests --all --format=plain
else
  ./ASuiteGuiTests --all --format=plain
fi
