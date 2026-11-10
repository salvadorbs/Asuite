#!/usr/bin/env bash
#
# Build Linux packages for ASuite.
#
# Usage:
#   utils/build-linux-packages.sh [options]
#
# Options:
#   --version VERSION    Package version (default: current git tag)
#   --widgetset NAME     gtk2, gtk3, qt5 or qt6 (default: gtk3)
#   --formats LIST       Comma-separated list of deb,rpm,tar.gz,appimage,zip
#                        (default: deb,rpm,tar.gz,appimage; gtk2 supports zip only)
#   --binary PATH        ASuite executable (default: bin/ASuite)
#   --output DIR         Output directory (default: dist)
#   -h, --help           Show this help
#
# Required tools:
#   deb/rpm   nfpm  https://github.com/goreleaser/nfpm
#   appimage  appimagetool, plus linuxdeploy to bundle the toolkit libraries
#             (without linuxdeploy the AppImage is thin and uses host libraries)
#   qt tar.gz patchelf (to bundle libQt5Pas/libQt6Pas with an $ORIGIN rpath)
#
# The script expects a binary already built with the Release_GH_Action build
# mode, for example:
#   lazbuild ASuite.lpi --no-write-project \
#     --build-mode=Release_GH_Action --widgetset=gtk3

set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
VERSION=""
WIDGETSET="gtk3"
FORMATS="deb,rpm,tar.gz,appimage"
BIN="$ROOT/bin/ASuite"
OUT="$ROOT/dist"

usage() {
  awk 'NR == 1 { next } /^#/ { sub(/^# ?/, ""); print; next } { exit }' "$0"
}

die() {
  echo "error: $*" >&2
  exit 1
}

while [ $# -gt 0 ]; do
  case "$1" in
    --version|--widgetset|--formats|--binary|--output)
      [ $# -ge 2 ] || die "missing value for $1"
      case "$1" in
        --version) VERSION=$2 ;;
        --widgetset) WIDGETSET=$2 ;;
        --formats) FORMATS=$2 ;;
        --binary) BIN=$2 ;;
        --output) OUT=$2 ;;
      esac
      shift 2
      ;;
    -h|--help) usage; exit 0 ;;
    *) die "unknown option: $1 (use --help)" ;;
  esac
done

# Make user-supplied relative paths independent from later cd calls.
case "$OUT" in /*) ;; *) OUT="$PWD/$OUT" ;; esac
case "$BIN" in /*) ;; *) BIN="$PWD/$BIN" ;; esac

FORMATS=${FORMATS// /}
FORMATS=${FORMATS,,}

[ -n "$FORMATS" ] || die "empty --formats value"
[[ ",$FORMATS," == *",,"* ]] && die "empty format in --formats"
for format in ${FORMATS//,/ }; do
  case "$format" in
    deb|rpm|tar.gz|appimage|zip) ;;
    *) die "unknown format '$format' (expected deb, rpm, tar.gz, appimage or zip)" ;;
  esac
done

case "$WIDGETSET" in
  gtk2)
    PKG_NAME="asuite-gtk2"
    TOOLKIT_LABEL="GTK2"
    CONFLICTS=""
    PROVIDES=""
    QT_MAJOR=""
    DEB_TOOLKIT_DEPS=""
    APPIMAGE_PLUGIN=""
    ;;
  gtk3)
    PKG_NAME="asuite"
    TOOLKIT_LABEL="GTK3"
    CONFLICTS="asuite-qt5 asuite-qt6"
    PROVIDES=""
    QT_MAJOR=""
    DEB_TOOLKIT_DEPS="libgtk-3-0 | libgtk-3-0t64, libharfbuzz-gobject0"
    APPIMAGE_PLUGIN="gtk"
    ;;
  qt5)
    PKG_NAME="asuite-qt5"
    TOOLKIT_LABEL="Qt5"
    CONFLICTS="asuite asuite-qt6"
    PROVIDES="asuite"
    QT_MAJOR="5"
    DEB_TOOLKIT_DEPS="libqt5core5a | libqt5core5t64, libqt5gui5 | libqt5gui5t64, libqt5widgets5 | libqt5widgets5t64, libqt5dbus5 | libqt5dbus5t64, libqt5network5 | libqt5network5t64, libqt5printsupport5 | libqt5printsupport5t64, libqt5x11extras5 | libqt5x11extras5t64"
    APPIMAGE_PLUGIN="qt"
    ;;
  qt6)
    PKG_NAME="asuite-qt6"
    TOOLKIT_LABEL="Qt6"
    CONFLICTS="asuite asuite-qt5"
    PROVIDES="asuite"
    QT_MAJOR="6"
    DEB_TOOLKIT_DEPS="libqt6core6 | libqt6core6t64, libqt6gui6 | libqt6gui6t64, libqt6widgets6 | libqt6widgets6t64, libqt6printsupport6 | libqt6printsupport6t64, libqt6dbus6 | libqt6dbus6t64, qt6-qpa-plugins"
    APPIMAGE_PLUGIN="qt"
    ;;
  *)
    die "unsupported widgetset '$WIDGETSET' (expected gtk2, gtk3, qt5 or qt6)"
    ;;
esac

if [ -z "$VERSION" ]; then
  if [ -n "${GITHUB_REF_NAME:-}" ] && [ "${GITHUB_REF_NAME#v}" != "$GITHUB_REF_NAME" ]; then
    VERSION=${GITHUB_REF_NAME#v}
  elif VERSION=$(git -C "$ROOT" describe --tags --abbrev=0 2>/dev/null); then
    VERSION=${VERSION#v}
  else
    VERSION="0.0.0"
  fi
fi

# Debian/RPM versions use '~' for pre-releases: 2.1.0-beta -> 2.1.0~beta.
# Only the first hyphen is the pre-release separator; any further hyphen is
# turned into a dot so that the value stays valid for both deb and rpm.
if [[ "$VERSION" == *-* ]]; then
  PKG_VERSION="${VERSION%%-*}~${VERSION#*-}"
  PKG_VERSION="${PKG_VERSION//-/.}"
else
  PKG_VERSION="$VERSION"
fi
[[ "$PKG_VERSION" =~ ^[0-9][0-9A-Za-z._+~]*$ ]] || \
  die "invalid version '$VERSION' (expected something like 2.1.0 or 2.1.0-beta)"

case "$(uname -m)" in
  x86_64|amd64)
    NFPM_ARCH="amd64"
    APPIMAGE_ARCH="x86_64"
    ;;
  aarch64|arm64)
    NFPM_ARCH="arm64"
    APPIMAGE_ARCH="aarch64"
    ;;
  *)
    die "unsupported architecture: $(uname -m)"
    ;;
esac

[ -x "$BIN" ] || die "ASuite executable not found or not executable: $BIN"
[ -d "$ROOT/bin/themes" ] || die "themes directory not found: $ROOT/bin/themes"
[ -d "$ROOT/bin/locale" ] || die "locale directory not found: $ROOT/bin/locale"
[ -f "$ROOT/bin/docs/license.txt" ] || die "license not found: $ROOT/bin/docs/license.txt"

IFS=',' read -r -a FORMAT_ARRAY <<< "$FORMATS"
has_format() {
  local format
  for format in "${FORMAT_ARRAY[@]}"; do
    [ "$format" = "$1" ] && return 0
  done
  return 1
}

if [ "$WIDGETSET" = "gtk2" ]; then
  for format in "${FORMAT_ARRAY[@]}"; do
    [ "$format" = "zip" ] || die "gtk2 supports only the zip format"
  done
fi

mkdir -p "$OUT"
OUT=$(cd "$OUT" && pwd)
STAGE="$OUT/stage-$WIDGETSET"
APPDIR="$OUT/AppDir-$WIDGETSET"
rm -rf "$STAGE" "$APPDIR"
mkdir -p "$STAGE"

echo "ASuite $VERSION ($PKG_NAME, $TOOLKIT_LABEL)"

# ---------------------------------------------------------------------------
# Common file layout
# ---------------------------------------------------------------------------

install -Dm755 "$ROOT/packaging/linux/asuite-launcher" "$STAGE/usr/bin/asuite"
install -Dm755 "$BIN" "$STAGE/usr/lib/asuite/asuite"
mkdir -p "$STAGE/usr/share/asuite"

cp -a "$ROOT/bin/themes" "$STAGE/usr/share/asuite/themes"
cp -a "$ROOT/bin/locale" "$STAGE/usr/share/asuite/locale"
install -Dm644 "$ROOT/bin/docs/license.txt" "$STAGE/usr/share/asuite/docs/license.txt"
install -Dm644 "$ROOT/LICENSE" "$STAGE/usr/share/asuite/docs/LICENSE"
install -Dm644 "$ROOT/README.md" "$STAGE/usr/share/asuite/docs/README.md"

install -Dm644 "$ROOT/packaging/linux/asuite.desktop" \
  "$STAGE/usr/share/applications/asuite.desktop"
install -d "$STAGE/usr/share/metainfo"
sed -E "s|<release version=\"[^\"]*\" date=\"[^\"]*\">|<release version=\"$VERSION\" date=\"$(date -u +%F)\">|" \
  "$ROOT/packaging/linux/com.salvadorsoftware.asuite.metainfo.xml" \
  > "$STAGE/usr/share/metainfo/com.salvadorsoftware.asuite.metainfo.xml"
chmod 0644 "$STAGE/usr/share/metainfo/com.salvadorsoftware.asuite.metainfo.xml"
mkdir -p "$STAGE/usr/share/icons"
cp -a "$ROOT/packaging/linux/icons/hicolor" "$STAGE/usr/share/icons/hicolor"

install -Dm644 "$ROOT/LICENSE" "$STAGE/usr/share/licenses/asuite/LICENSE"
install -Dm644 "$ROOT/LICENSE" "$STAGE/usr/share/doc/$PKG_NAME/copyright"

# Package the private Lazarus Qt bindings. They are not available in every
# distribution repository, so the launcher adds usr/lib/asuite to
# LD_LIBRARY_PATH. The Qt runtime itself remains a distribution dependency.
if [ -n "$QT_MAJOR" ]; then
  found_qtpas=0
  shopt -s nullglob
  for dir in /usr/lib/x86_64-linux-gnu /usr/lib/aarch64-linux-gnu \
             /usr/lib64 /usr/lib /lib/x86_64-linux-gnu /lib64 /lib; do
    for lib in "$dir"/libQt"${QT_MAJOR}"Pas.so*; do
      cp -a "$lib" "$STAGE/usr/lib/asuite/"
      found_qtpas=1
    done
  done
  shopt -u nullglob
  [ "$found_qtpas" -eq 1 ] || \
    die "libQt${QT_MAJOR}Pas not found; install libqt${QT_MAJOR}pas before packaging"
fi

# ---------------------------------------------------------------------------
# tar.gz and zip
# ---------------------------------------------------------------------------

build_portable() {
  # The portable archives contain the executable and resources; the
  # configuration lives in the same directory tree when it is writable.
  local name="$PKG_NAME-$VERSION"
  local portable="$OUT/portable-$WIDGETSET/$name"
  local tarball="$OUT/${PKG_NAME}-${VERSION}-linux-${APPIMAGE_ARCH}.tar.gz"
  local zipfile="$OUT/${PKG_NAME}-${VERSION}-linux-${APPIMAGE_ARCH}.zip"

  rm -rf "$OUT/portable-$WIDGETSET"
  mkdir -p "$portable/docs"
  install -Dm755 "$BIN" "$portable/ASuite"
  cp -a "$ROOT/bin/themes" "$portable/themes"
  cp -a "$ROOT/bin/locale" "$portable/locale"
  cp -a "$ROOT/bin/docs/." "$portable/docs/"
  install -Dm644 "$ROOT/LICENSE" "$portable/LICENSE"
  install -Dm644 "$ROOT/README.md" "$portable/README.md"

  # The Lazarus Qt bindings are not available in every distribution, so bundle
  # them next to the executable and make it find them with an $ORIGIN rpath.
  # The Qt runtime itself is still provided by the system.
  if [ -n "$QT_MAJOR" ]; then
    command -v patchelf >/dev/null 2>&1 || \
      die "patchelf not found; it is required to bundle libQt${QT_MAJOR}Pas in the portable archive"
    cp -a "$STAGE/usr/lib/asuite"/libQt"${QT_MAJOR}"Pas.so* "$portable/"
    patchelf --set-rpath '$ORIGIN' "$portable/ASuite"
  fi

  if has_format tar.gz; then
    echo "creating $tarball"
    tar -C "$OUT/portable-$WIDGETSET" -czf "$tarball" "$name"
  fi

  if has_format zip; then
    command -v zip >/dev/null 2>&1 || \
      die "zip not found; it is required to build the portable zip"
    echo "creating $zipfile"
    ( cd "$OUT/portable-$WIDGETSET" && zip -qr "$zipfile" "$name" )
  fi
}

# ---------------------------------------------------------------------------
# deb and rpm
# ---------------------------------------------------------------------------

needed_sonames() {
  local file="$1"

  # Only direct NEEDED entries: relying on full ldd output would add
  # dependencies of Qt/GTK (ICU, systemd, ...) with version-specific sonames
  # that differ between distributions.
  if command -v readelf >/dev/null 2>&1; then
    readelf -d "$file" | awk '/\(NEEDED\)/ {gsub(/[][]/, "", $NF); print $NF}'
  else
    objdump -p "$file" | awk '/NEEDED/ {print $2}'
  fi
}

rpm_depends_block() {
  # Use RPM soname dependencies so the same package works on Fedora, RHEL and
  # openSUSE where the package names differ. The private Qt bindings bundled in
  # usr/lib/asuite are intentionally excluded, but their own dependencies are
  # collected so the Qt runtime is pulled in.
  {
    needed_sonames "$BIN"
    if [ -n "$QT_MAJOR" ]; then
      for lib in "$STAGE/usr/lib/asuite"/libQt"${QT_MAJOR}"Pas.so.*; do
        [ -f "$lib" ] && needed_sonames "$lib"
      done
    fi
  } | sort -u | while IFS= read -r soname; do
    case "$soname" in
      libQt"${QT_MAJOR}"Pas.so*) continue ;;
      libc.so*|libm.so*|libdl.so*|libpthread.so*|librt.so*|ld-linux*) continue ;;
    esac
    printf '      - "%s()(64bit)"\n' "$soname"
  done
}

build_nfpm() {
  command -v nfpm >/dev/null 2>&1 || die "nfpm not found (https://github.com/goreleaser/nfpm)"

  local config="$OUT/nfpm-$WIDGETSET.yaml"
  local conflicts_block="" provides_block="" deb_depends_block="" rpm_deps_block=""
  local item

  for item in $CONFLICTS; do
    conflicts_block+="  - $item"$'\n'
  done
  if [ -n "$PROVIDES" ]; then
    provides_block="provides:"$'\n'"  - $PROVIDES"$'\n'
  fi

  IFS=',' read -r -a deb_deps <<< "$DEB_TOOLKIT_DEPS"
  deb_depends_block="      - libc6"$'\n'"      - libx11-6"$'\n'"      - libdbus-1-3"$'\n'"      - zlib1g"$'\n'
  for item in "${deb_deps[@]}"; do
    item=$(echo "$item" | sed 's/^ *//;s/ *$//')
    deb_depends_block+="      - \"$item\""$'\n'
  done

  rpm_deps_block="      - glibc"$'\n'
  rpm_deps_block+=$(rpm_depends_block)

  cat > "$config" <<EOF
name: "$PKG_NAME"
arch: "$NFPM_ARCH"
platform: linux
version: "$PKG_VERSION"
section: utils
priority: optional
maintainer: "SalvadorSoftware <salvadorbs@gmail.com>"
description: |
  ASuite is a simple, fast and lightweight launcher for portable and installed
  applications, documents and folders.
vendor: "SalvadorSoftware"
homepage: "https://github.com/salvadorbs/Asuite"
license: "GPL-3.0-or-later"
${provides_block}conflicts:
${conflicts_block}replaces:
${conflicts_block}contents:
  - src: "$STAGE/usr/bin/asuite"
    dst: /usr/bin/asuite
    file_info:
      mode: 0755
  - src: "$STAGE/usr/lib/asuite"
    dst: /usr/lib/asuite
  - src: "$STAGE/usr/share/asuite"
    dst: /usr/share/asuite
  - src: "$STAGE/usr/share/applications/asuite.desktop"
    dst: /usr/share/applications/asuite.desktop
  - src: "$STAGE/usr/share/icons/hicolor"
    dst: /usr/share/icons/hicolor
  - src: "$STAGE/usr/share/metainfo/com.salvadorsoftware.asuite.metainfo.xml"
    dst: /usr/share/metainfo/com.salvadorsoftware.asuite.metainfo.xml
  - src: "$STAGE/usr/share/licenses/asuite/LICENSE"
    dst: /usr/share/licenses/asuite/LICENSE
  - src: "$STAGE/usr/share/doc/$PKG_NAME/copyright"
    dst: /usr/share/doc/$PKG_NAME/copyright
overrides:
  deb:
    depends:
$deb_depends_block  rpm:
    depends:
$rpm_deps_block
EOF

  if has_format deb; then
    nfpm package --config "$config" --packager deb --target "$OUT"
  fi
  if has_format rpm; then
    nfpm package --config "$config" --packager rpm --target "$OUT"
  fi
}

# ---------------------------------------------------------------------------
# AppImage
# ---------------------------------------------------------------------------

build_appdir() {
  cp -a "$STAGE/." "$APPDIR/"
  # The AppImage uses the real executable directly; linuxdeploy sets up the
  # runtime library path and the custom AppRun below points ASuite at the
  # bundled resources. The copy in usr/lib/asuite is only needed by the
  # wrapper used by the system packages.
  install -Dm755 "$BIN" "$APPDIR/usr/bin/asuite"
  rm -f "$APPDIR/usr/lib/asuite/asuite"
  install -Dm755 "$ROOT/packaging/linux/AppRun" "$APPDIR/AppRun"
  install -Dm644 "$ROOT/packaging/linux/asuite.desktop" "$APPDIR/asuite.desktop"
  install -Dm644 "$ROOT/packaging/linux/icons/hicolor/256x256/apps/asuite.png" \
    "$APPDIR/asuite.png"
}

# Normalize the AppDir after linuxdeploy. linuxdeploy links the root icon to a
# hicolor copy, but some desktop environments and AppImage tools do not follow
# that chain. Reference AppImages ship a real PNG at the root and point
# .DirIcon at it, so do the same.
finalize_appdir() {
  local desktop="$APPDIR/usr/share/applications/asuite.desktop"

  rm -f "$APPDIR/asuite.png"
  install -Dm644 "$ROOT/packaging/linux/icons/hicolor/256x256/apps/asuite.png" \
    "$APPDIR/asuite.png"
  ln -sfn asuite.png "$APPDIR/.DirIcon"

  # Version=1.0 in the desktop entry is only the Desktop Entry specification
  # version. Advertise the application version with the AppImage convention.
  if ! grep -q '^X-AppImage-Version=' "$desktop"; then
    printf 'X-AppImage-Version=%s\n' "$VERSION" >> "$desktop"
  fi
  ln -sfn usr/share/applications/asuite.desktop "$APPDIR/asuite.desktop"
}

build_appimage() {
  command -v appimagetool >/dev/null 2>&1 || \
    die "appimagetool not found (https://github.com/AppImage/appimagetool)"

  build_appdir
  local target="$OUT/${PKG_NAME}-${VERSION}-linux-${APPIMAGE_ARCH}.AppImage"
  local workdir="$OUT/appimage-$WIDGETSET"

  # Build in a private directory: building another widgetset into the same
  # output directory must not delete or pick up an existing AppImage.
  rm -rf "$workdir"
  mkdir -p "$workdir"
  rm -f "$target"

  export ARCH="$APPIMAGE_ARCH"

  if command -v linuxdeploy >/dev/null 2>&1; then
    export LD_LIBRARY_PATH="$APPDIR/usr/lib/asuite${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
    if [ "$APPIMAGE_PLUGIN" = "gtk" ]; then
      export DEPLOY_GTK_VERSION=3
    else
      case "$QT_MAJOR" in
        5) export QMAKE="${QMAKE:-$(command -v qmake-qt5 || command -v qmake5 || command -v qmake || true)}" ;;
        6) export QMAKE="${QMAKE:-$(command -v qmake6 || command -v qmake || true)}" ;;
      esac
      [ -n "${QMAKE:-}" ] || die "qmake for Qt$QT_MAJOR not found"
    fi

    # Deploy into the AppDir only: without an output plugin linuxdeploy just
    # prepares the bundle, so the icon and desktop metadata can be normalized
    # before appimagetool assembles the AppImage.
    if ! ( cd "$workdir" && linuxdeploy \
        --appdir "$APPDIR" \
        --executable "$APPDIR/usr/bin/asuite" \
        --desktop-file "$APPDIR/usr/share/applications/asuite.desktop" \
        --icon-file "$APPDIR/usr/share/icons/hicolor/256x256/apps/asuite.png" \
        --custom-apprun "$ROOT/packaging/linux/AppRun" \
        --plugin "$APPIMAGE_PLUGIN" ); then
      # A "thin" AppImage would depend on the toolkit libraries installed on
      # the host, which defeats the purpose of the bundle. Fail instead of
      # publishing an AppImage that only works on the build machine.
      die "linuxdeploy failed; not creating a thin AppImage"
    fi
  fi

  finalize_appdir
  ( cd "$workdir" && appimagetool "$APPDIR" "$target" )

  rm -rf "$workdir"
  echo "creating $target"
  chmod +x "$target"
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

if has_format tar.gz || has_format zip; then build_portable; fi
if has_format deb || has_format rpm; then build_nfpm; fi
if has_format appimage; then build_appimage; fi

echo "packages written to $OUT"
