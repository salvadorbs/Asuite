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

## Help?
Open an issue, if you can't find a component or you can't build correctly the project.
