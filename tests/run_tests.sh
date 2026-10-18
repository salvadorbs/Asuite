#!/usr/bin/env bash
# Build and run the ASuite test suites.
#
#   ASuiteTests     : FPCUnit console tests, no display or widgetset required.
#   ASuiteGuiTests  : FPCUnit tests that build LCL controls; need a display
#                     server (xvfb-run is used automatically when available).
#
# Both runners link heaptrc and leak reports are checked on every widgetset.
# The only exception is Windows: COM keeps allocations referenced at exit and
# heaptrc's dump walks them, so HEAPTRC is disabled on win32 and results there
# still come from the FPCUnit summary and from the process exit code.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

LAZBUILD="${LAZBUILD:-lazbuild}"
WIDGETSET="${WIDGETSET:-gtk3}"
TEST_TIMEOUT="${ASUITE_TEST_TIMEOUT:-600}"
MIN_TESTS="${ASUITE_MIN_TESTS:-470}"

# See the header: drop the heaptrc dump where it is unreliable.
case "$WIDGETSET" in
  win32) export HEAPTRC=disabled ;;
esac

EXE_EXT=""
case "$(uname -s)" in
  MINGW*|MSYS*|CYGWIN*) EXE_EXT=".exe" ;;
esac

WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$WORK_DIR"' EXIT

build_suite() {
  local project="$1" widgetset="$2"
  local -a args=("$project" --no-write-project)
  if [ -n "$widgetset" ]; then
    args+=("--widgetset=$widgetset")
  fi
  # FPC 3.2.2 sometimes dies with an internal error while compiling big GUI
  # units and leaves corrupt .ppu files behind, which then break every later
  # incremental build deterministically. On failure, wipe the generated unit
  # output and retry once from a clean state.
  if "$LAZBUILD" "${args[@]}"; then
    return 0
  fi
  echo "Build of $project failed; cleaning generated units and retrying once..."
  rm -rf "$SCRIPT_DIR/lib"
  "$LAZBUILD" "${args[@]}"
}

# Run one suite, stream its output to the job log and keep a copy for the
# assertions below. Usage: run_suite <label> <binary> <logfile>
run_suite() {
  local label="$1" binary="$2" log="$3"
  local -a cmd=()

  # GNU timeout only: the Windows timeout.exe has a different command line.
  if command -v timeout >/dev/null 2>&1 && \
     timeout --version 2>/dev/null | grep -q 'GNU coreutils'; then
    cmd+=(timeout "$TEST_TIMEOUT")
  fi
  if [ -z "${DISPLAY:-}" ] && command -v xvfb-run >/dev/null 2>&1; then
    cmd+=(xvfb-run -a)
  fi
  cmd+=("$binary" --all --format=plain)

  echo "== $label =="
  set +e
  "${cmd[@]}" 2>&1 | tee "$log"
  local status=${PIPESTATUS[0]}
  set -e

  if [ "$status" -eq 124 ]; then
    echo "ERROR: $label timed out after ${TEST_TIMEOUT}s" >&2
    exit 1
  fi
  if [ "$status" -ne 0 ]; then
    echo "ERROR: $label exited with status $status" >&2
    exit 1
  fi
}

count_tests() {
  # Echoes the "Number of run tests" value from FPCUnit plain output.
  sed -n 's/^Number of run tests:[[:space:]]*\([0-9][0-9]*\).*/\1/p' "$1" | tail -n 1
}

check_leaks() {
  local label="$1" log="$2"
  if grep -Eq '[1-9][0-9]* unfreed memory blocks' "$log"; then
    echo "ERROR: memory leaks detected in $label (see 'unfreed memory blocks' above)" >&2
    exit 1
  fi
}

build_suite ASuiteTests.lpi ""
CONSOLE_LOG="$WORK_DIR/console.log"
run_suite "ASuiteTests" "./ASuiteTests$EXE_EXT" "$CONSOLE_LOG"

build_suite ASuiteGuiTests.lpi "$WIDGETSET"
GUI_LOG="$WORK_DIR/gui.log"
run_suite "ASuiteGuiTests ($WIDGETSET)" "./ASuiteGuiTests$EXE_EXT" "$GUI_LOG"

# Guard against a silently shrinking suite: fail when fewer than
# ASUITE_MIN_TESTS (default 470) tests actually run in total.
CONSOLE_N="$(count_tests "$CONSOLE_LOG")"
GUI_N="$(count_tests "$GUI_LOG")"
TOTAL=$(( ${CONSOLE_N:-0} + ${GUI_N:-0} ))
echo "=== Ran $TOTAL test(s) in total ==="
if (( TOTAL < MIN_TESTS )); then
  echo "ERROR: expected at least $MIN_TESTS tests, got $TOTAL" >&2
  exit 1
fi

check_leaks "ASuiteTests" "$CONSOLE_LOG"
check_leaks "ASuiteGuiTests" "$GUI_LOG"
if [ "${HEAPTRC:-}" = "disabled" ]; then
  echo "=== Leak check skipped (heaptrc disabled on $WIDGETSET) ==="
else
  echo "=== No memory leaks detected ==="
fi
