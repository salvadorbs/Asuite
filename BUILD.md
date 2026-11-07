# ASuite building instructions

## Lazarus version
Lazarus 2.0.10 and FPC 3.2 are required for building ASuite. Older Lazarus & FPC versions aren't tested. 

## Sources
You must get sources from git repository (and submodule AsuiteComps) hosted in github.

```
$ git clone https://github.com/salvadorbs/Asuite.git
$ cd Asuite
$ git submodule init
$ git submodule update
```

For more informations, see https://docs.github.com/en/free-pro-team@latest/github/creating-cloning-and-archiving-repositories/cloning-a-repository and https://github.com/git-guides/git-clone

## Third party components
Once Lazarus is installed, you need to install these components from OPM (Online Package Manager):
- Lazarus VirtualTree https://github.com/blikblum/VirtualTreeView-Lazarus
- mORMot 1.18 http://synopse.info/fossil/wiki?name=SQLite3+Framework (probably you need download http://synopse.info/files/sqlite3fpc.7z and extract files in subfolder onlinepackagemanager\packages\mormot\static)
- BGRA Controls https://github.com/bgrabitmap/bgracontrols
- JPPack https://github.com/jackdp/JPPack
- HashLib4Pascal https://github.com/Xor-el/HashLib4Pascal
- Unique Instance https://github.com/blikblum/luipack/tree/master/uniqueinstance
- KControls https://github.com/kryslt/KControls

You need a last component, ASuiteComps, who you will find in subfolder https://github.com/salvadorbs/Asuite/tree/develop/3p/ASuiteComps. Install it and rebuild the IDE.

## Linux packages

The release workflow builds `.deb`, `.rpm`, `.AppImage` and `.tar.gz` packages
for the GTK3, Qt5 and Qt6 widgetsets. The GTK2 build is distributed as a
portable zip. The same packages can be built locally:

```bash
# Build a clean release binary (one widgetset at a time)
lazbuild ASuite.lpi --no-write-project --build-mode=Release_GH_Action --widgetset=gtk3

# Create all package formats in dist/
utils/build-linux-packages.sh --version 2.1.0 --widgetset gtk3

# Build the GTK2 portable zip
utils/build-linux-packages.sh --version 2.1.0 --widgetset gtk2 --formats zip
```

Use `--formats deb,rpm,tar.gz,appimage,zip` to select the formats and
`--output DIR` to choose another output directory. The script requires:

- `nfpm` for `.deb` and `.rpm` (https://github.com/goreleaser/nfpm)
- `linuxdeploy` (recommended) or `appimagetool` for `.AppImage`
- `patchelf` for the Qt5/Qt6 `.tar.gz`, so the bundled `libQt5Pas`/`libQt6Pas`
  can be found through an `$ORIGIN` rpath
- `zip` for the GTK2 portable archive
- `dpkg-deb`, `rpm`, `desktop-file-validate` and `appstreamcli` to validate the
  generated packages (the CI installs them automatically)

Package names are `asuite` (GTK3), `asuite-qt5` and `asuite-qt6`. They conflict
with each other because they install the same desktop entry and resources; the
AppImage and tar.gz archives can be used side by side. The Qt tarballs bundle
the Lazarus Qt bindings but still use the Qt runtime provided by the system.

The CI action in `.github/actions/linux-packages` also installs every generated
package in a clean container (Ubuntu 24.04 for `.deb`, Fedora for `.rpm`),
checks that the executable has no missing shared libraries and starts the GUI
under Xvfb. The AppImage and tar.gz are smoke-tested in the same way, and
`.github/actions/linux-portable-zip` extracts the GTK2 zip and starts it under
Xvfb. Release assets are flattened into `release/` and published together with
a `SHA256SUMS` file; the third-party packaging tools are pinned by version and
verified by SHA-256.

## Windows packages

The release workflow builds a portable zip archive and an Inno Setup installer
from the win64 build. The installer is described by `utils/setup.iss`; the
version is passed by the pipeline so it always matches the release tag:

```powershell
ISCC.exe /DMyAppVersion=2.1.0 utils\setup.iss
```

Both packages contain `ASuite.exe`, `docs`, `locale` and `themes`; build
artifacts such as `ASuite.dbg`, `ASuite.mab` and local settings are left out.
The CI installs the generated installer silently into a temporary directory,
checks the shipped files and runs the uninstaller again, so a broken
`setup.iss` fails the build before anything is published.


## Help?
Open an issue, if you can't find a component or you can't build correctly the project.
