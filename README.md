<h1 align="center">
  <br>
  <a href="https://github.com/salvadorbs/Asuite"><img src="https://i.imgur.com/iKPAPkD.png" alt="ASuite" width="128"></a>
  <br>
  ASuite
  <br>
</h1>

<h4 align="center">A simple open source portable launcher for Microsoft Windows</h4>

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
![GitHub top language](https://img.shields.io/github/languages/top/salvadorbs/asuite)
[![GitHub release (latest by date)](https://img.shields.io/github/v/release/salvadorbs/asuite)](https://github.com/salvadorbs/asuite/releases)
[![Github All Releases](https://img.shields.io/github/downloads/salvadorbs/asuite/total.svg)](https://github.com/salvadorbs/asuite/releases)
![GitHub](https://img.shields.io/github/license/salvadorbs/asuite)
![GitHub stars](https://img.shields.io/github/stars/salvadorbs/asuite)
![LOC](https://tokei.rs/b1/github/salvadorbs/asuite)

<p align="center">
  <a href="#key-features">Key Features</a> |
  <a href="#download">Download</a> |
  <a href="#contribute">Contribute</a> |
  <a href="#license">License</a> |
  <a href="#support">Support</a>
</p>

<p align="center"><img src="https://i.imgur.com/dKCmMWl.png" align="center"
     alt="ASuite Screenshot" width="384"></p>

## Key Features

* Any linked item can be organized in a hierarchical structure (tree view)
* Simple, fast and lightweight
* Runs any runnable files (portable or not applications, documents and other file types) or folder using relative paths
* Runs any list item on ASuite's startup or shutdown
* TrayIcon Menus (Graphic Menu and Classic Menu)
* Multilanguage
* Schedule any list item to run automatically using internal scheduler
* Create custom hotkeys for any list items or launcher itself
* Internal cache icons (speed up launcher's startup)
* Most Recently Used (MRU) and Most Frequently Used (MFU) in Trayicon Menus
* Import list from PStart 2.* , wPPLauncher 1.* e ASuite 1.*

## Download
You can [download](https://github.com/salvadorbs/asuite/releases) the latest version of ASuite.

On Windows, releases include a portable zip archive and an Inno Setup
installer (`asuite-<version>-setup.exe`). If you prefer a command-line
installer (APT style), you can use <a href="https://github.com/lukesampson/scoop#installation">Scoop</a>. To install ASuite using Scoop, you must add the bucket Extras:

```powershell
scoop bucket add extras
scoop install asuite
```

On Linux, releases include `.deb`, `.rpm`, `.AppImage` and `.tar.gz` packages for
the GTK3, Qt5 and Qt6 widgetsets; the GTK2 build is still distributed as a zip
archive only. Only one system package can be installed at a time because they
share the same desktop entry and resources. The AppImage and tar.gz builds can
be used side by side with an installed package. For example:

```bash
sudo apt install ./asuite_2.1.0_amd64.deb
# or
sudo dnf install ./asuite-2.1.0-1.x86_64.rpm
# or
chmod +x asuite-2.1.0-linux-x86_64.AppImage
./asuite-2.1.0-linux-x86_64.AppImage
```

Every release also publishes a `SHA256SUMS` file; verify a download with
`sha256sum -c SHA256SUMS --ignore-missing`.

System packages and the AppImage store the configuration in `~/.config/ASuite`
(or `$XDG_CONFIG_HOME/ASuite`). The portable zip/tar.gz builds keep the
configuration next to the executable instead.

## Contribute

This repo uses the [Git Flow](http://nvie.com/posts/a-successful-git-branching-model/) methodology - i.e. development proceeds on the `develop` branch and `feature` branches off it. Only released content appears on the `master` branch.

* Please read [BUILD.md](https://github.com/salvadorbs/ASuite/blob/master/BUILD.md) on how to set up an ASuite development environment.
* Read and follow our [Code of Conduct](https://github.com/salvadorbs/ASuite/blob/master/CODE_OF_CONDUCT.md).
* To get your patches merged, please adhere to the [Contributing](https://github.com/salvadorbs/ASuite/blob/master/CONTRIBUTING.md) guidelines.

## License
Copyright (C) 2006-2020 Matteo Salvi
This file is part of ASuite, which is free software. It is made 
available to you under the terms of the GNU General Public License
as published by the Free Software Foundation, either version 3 of
the License, or (at your option) any later version. For more
information, see [LICENSE](https://github.com/salvadorbs/Asuite/blob/master/LICENSE).

## Support
If you find this project useful, please consider making a donation.

[![paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=QYASG6DPAYSXW)
