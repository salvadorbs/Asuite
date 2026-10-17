#!/usr/bin/env bash
# Build and run the ASuite test suites.
#
#   ASuiteTests     : FPCUnit console tests, no display or widgetset required.
#   ASuiteGuiTests  : FPCUnit tests that build LCL controls; need a display
#                     server (xvfb-run is used automatically when available).
# Both runners link heaptrc, so any "N unfreed memory blocks" with N > 0 in
# their output fails this script (see check_leaks below).
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

count_tests() {
  # Echoes the "Number of run tests" line value from FPCUnit plain output.
  printf '%s\n' "$1" | sed -n 's/^Number of run tests:[[:space:]]*\([0-9][0-9]*\).*/\1/p' | tail -n 1
}

echo "== ASuiteTests =="
"$LAZBUILD" ASuiteTests.lpi
CONSOLE_OUT="$(./ASuiteTests --all --format=plain 2>&1)"
printf '%s\n' "$CONSOLE_OUT"

echo "== ASuiteGuiTests =="
build_gui_tests
if [ -z "${DISPLAY:-}" ] && command -v xvfb-run >/dev/null 2>&1; then
  GUI_OUT="$(xvfb-run -a ./ASuiteGuiTests --all --format=plain 2>&1)"
else
  GUI_OUT="$(./ASuiteGuiTests --all --format=plain 2>&1)"
fi
printf '%s\n' "$GUI_OUT"

# Guard against a silently shrinking suite: fail when fewer than
# ASUITE_MIN_TESTS (default 150) tests actually run in total.
MIN_TESTS="${ASUITE_MIN_TESTS:-150}"
CONSOLE_N="$(count_tests "$CONSOLE_OUT")"
GUI_N="$(count_tests "$GUI_OUT")"
TOTAL=$(( ${CONSOLE_N:-0} + ${GUI_N:-0} ))
echo "=== Ran $TOTAL test(s) in total ==="
if (( TOTAL < MIN_TESTS )); then
  echo "ERROR: expected at least $MIN_TESTS tests, got $TOTAL" >&2
  exit 1
fi

check_leaks() {
  # Fails when heaptrc reports unfreed blocks in a suite's captured output.
  if printf '%s\n' "$2" | grep -Eq '[1-9][0-9]* unfreed memory blocks'; then
    echo "ERROR: memory leaks detected in $1 (see 'unfreed memory blocks' above)" >&2
    return 1
  fi
}

check_leaks "ASuiteTests" "$CONSOLE_OUT"
check_leaks "ASuiteGuiTests" "$GUI_OUT"
echo "=== No memory leaks detected ==="
