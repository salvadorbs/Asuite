#!/usr/bin/env bash
# Build and run the ASuite unit tests (FPCUnit console runner).
# These tests cover configuration observer/notification logic and do not need
# a display server or a LCL widgetset.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

LAZBUILD="${LAZBUILD:-lazbuild}"

"$LAZBUILD" ASuiteTests.lpi
./ASuiteTests --all --format=plain
