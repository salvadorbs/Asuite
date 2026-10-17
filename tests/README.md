# ASuite automated tests

FPCUnit test suites for ASuite: pure-logic units run headless, LCL-dependent
units run with a display server.

## Layout

| File | Covers |
|---|---|
| `ASuiteTests.lpr` / `.lpi` | Console runner (exit code <> 0 on failure), no widgetset needed |
| `Tests.AppConfigObserver.pas` | `AppConfig.Observer` / `AppConfig.Notifications` |
| `Tests.RunActionLogic.pas` | `Utility.RunActionLogic` |
| `Tests.LanguageManager.pas` | `Language.Manager`: locale scan, native names, sort, LangID match |
| `ASuiteGuiTests.lpr` / `.lpi` | GUI runner (needs a display; `xvfb-run` is used automatically) |
| `Tests.MenuUtils.pas` | `Utility.MenuUtils` |
| `Tests.SortController.pas` | `Utility.SortController` |
| `Tests.LanguageCombo.pas` | Language combo fill on the general options page |
| `Tests.ConfigPaths.pas` | `TConfigPaths` layout and absolute/relative conversions, `TPathList` variable expansion |
| `Tests.Conversions.pas` | `Utility.Conversions` color/font serialization |
| `Tests.FileFolder.pas` | Side-effect-free helpers in `Utility.FileFolder` |
| `Tests.Scheduler.pas` | `TScheduler` firing logic (`CheckOnce/Daily/Hourly`) |
| `Tests.IconsManager.pas` | `Icons.Manager` registry, extension lists, concurrent add/clear |
| `Tests.IconsBaseLogic.pas` | `Icons.Base` lazy load/cache, `Icons.Thread` lifecycle |
| `Tests.IconsCacheKey.pas` | `Icons.CacheKey`: sanitizing, hashing, cache file names |
| `Tests.Helpers.Tree.pas` | Shared base fixture: tree as `MainTree`, node ownership, list cleanup, `Config` snapshot |
| `Tests.NodeDataTypes.pas` | `TvBaseNodeData`, `TvCustomRealNodeData` (via a test double), separators, categories |
| `Tests.NodeDataTypesFiles.pas` | `TvFileNodeData`: paths, working dir, MRU/MFU registration |
| `Tests.VirtualTreeMethods.pas` | `TVirtualTreeMethods`: creation, lookup, search, callbacks |
| `Tests.VirtualTreeEvents.pas` | `TVirtualTreeEvents` handlers, save/load round-trip |
| `Tests.VirtualTreeHelper.pas` | `VirtualTreeHelper` copy/cut/paste, every `SetupVST*` wiring |
| `Tests.Lists.pas` | `TBaseItemsList`, `TSpecialItemsList`, `TListManager`, `THotkeyItemsList` (no desktop) |
| `Tests.SystemUtils.pas` | `Utility.System` checks, `Utility.XML` hotkey tables |
| `Tests.MiscUtils.pas` | `Utility.Misc` pure helpers |
| `Tests.RunController.pas` | `TRunController` run-action enablement |
| `Tests.ClipboardController.pas` | `TClipboardController` marking and paste state |
| `Tests.SearchController.pas` | `TSearchController` search execution and events |
| `Tests.XmlImport.pas` | XML getters and legacy list importers |
| `Tests.Configuration.pas` | `TConfiguration` setters and batching |
| `Tests.ShellLink.pas` | `TShellLinkFile` link files |
| `Tests.FindFiles.pas` | `TFindFiles` directory scanner thread |
| `Tests.ASMenuItem.pas` | `TASMenuItem` node references and separators |
| `Tests.Frames.pas` | Option/property page load/save round-trips |
| `Tests.ImportList.pas` | Import form: counting, XML populate, path conversion |
| `Tests.TrayMenu.pas` | Tray menu building from `dmTrayMenu` (classic and graphic menus) |
| `Tests.Execute.pas` | Real node execution (platform "true" binary), single-instance probe (`/bin/sleep`), autorun lists, `RunFile` |
| `Tests.Settings.pas` | `LoadXMLSettings` legacy settings mapping and defaults |
| `Tests.Helpers.Database.pas` | Shared fixture for the SQLite suites: temp DB, tree as `MainTree`, deterministic dates |
| `Tests.DatabaseVersion.pas` | `TSQLtbl_version` version resource, save/load round-trip, Update branch, empty-table guard |
| `Tests.DatabaseList.pas` | `TSQLtbl_list` tree round-trip, ordering by position, update on `Changed`/`Position`/`ParentID`, import mode, Unicode |
| `Tests.DatabaseManager.pas` | `TDBManager`: idempotent `Setup`, `SaveData` result, backup creation/pruning, `RemoveItem`, `DeleteItems`, `ClearTable` |
| `Tests.Instance.pas` | `TASuiteInstance.HandleParam` command-line (`-list`) parsing |
| `run_tests.sh` | Build + run helper used locally and by CI |

## Run locally

```bash
./tests/run_tests.sh            # gtk3 (override with WIDGETSET=qt6 ...)
```

Set `ASUITE_MIN_TESTS` to change the minimum total number of tests required
(default 470); the script fails if the suites shrink below it.

GUI control tests need a display server on Linux. If `DISPLAY` is not set
and `xvfb-run` is available, the script re-executes the GUI binary under
`xvfb-run` automatically.

## Run in CI

`.github/workflows/main.yml` and `main_release.yml` run both suites on every
matrix entry (Windows `win32` and Linux `gtk2`/`gtk3`/`qt5`/`qt6`) before
uploading the build. The jobs call `tests/run_tests.sh` with an absolute
`LAZBUILD`, so local and CI runs share the same build, timeout and leak
logic. On Linux the GUI suite runs under `xvfb-run` (`xvfb` is installed per
job); on Windows it uses the runner's desktop session.

The CI step has `timeout-minutes: 30` and the script applies the
`ASUITE_TEST_TIMEOUT` limit (600s per suite) when GNU `timeout` is available.
A blocking GUI test therefore fails the job with the last test printed in the
log instead of keeping the runner busy until the 6-hour job limit.

`HEAPTRC` is disabled on `qt5`/`qt6`/`win32`: on Qt the LCL teardown runs
after heaptrc starts dumping and can abort the process (exit 217) even when
all tests passed, and on win32 COM/widgetset allocations are outside the
tests' control. Leak checking stays enabled on the GTK runners.

## Notes

- `TConfigPaths.Create` changes the process working directory: its tests
  save and restore it, and use a writable temp folder so nothing is written
  outside it.
- Non-ASCII string comparisons in objfpc test units go through `UTF8Encode`:
  with `DefaultSystemCodePage = 0` the RTL would otherwise convert the
  literal with the wrong code page.
- FPC 3.2.2 occasionally dies with an internal error compiling big GUI units
  and leaves corrupt `.ppu` files behind, breaking later incremental builds
  deterministically. `run_tests.sh` wipes `tests/lib` and retries the GUI
  build once on failure.
- The SQLite suites (`Tests.Database*.pas`) run inside the GUI runner because
  the database layer is wired to LCL/`VirtualTrees`: `TVirtualTreeMethods`
  resolves node data against `ASuiteInstance.MainTree`, and
  `TDBManager.DeleteItems` deletes rows through the singleton
  `ASuiteManager.DBManager`, never through a local instance.
- `Config.ASuiteState` is `lsImporting` for the whole database test run: node
  setters would otherwise register in the singleton lists of `TListManager`
  and keep dangling pointers after the tree is freed.
- Backup tests write to `tests/backup/` (gitignored) because
  `ASuiteInstance.Paths.SuitePathBackup` cannot be redirected from the test
  binary; each test cleans up the files it created. `DoBackupList` copies the
  database through a share-deny-none stream, so the backup also succeeds on
  Windows while SQLite holds the file open (the previous `FileUtil.CopyFile`
  opened it share-deny-write and silently produced nothing there).
- Some behaviour is platform-specific and the tests assert it explicitly
  instead of assuming a POSIX environment: `Tests.ConfigPaths` builds paths
  with `PathDelim` and a platform drive, `Tests.NodeDataTypesFiles` uses a
  real temp file for the existence flag, `Tests.VirtualTreeEvents` treats a
  drop without an OLE data object as a no-op on Windows, and
  `Tests.ClipboardController` switches the controller to the in-memory
  clipboard (`UseSystemClipboard := False`) because the headless Windows CI
  has no system clipboard.
- The tree fixture (`Tests.Helpers.Tree`) switches `toAutoSort` off: the
  default tree options re-sort on every insert once `OnCompareNodes` is
  wired, which would reorder nodes under test and run the production
  compare on half-initialized nodes (their `PNode` is set only after
  `AddChild` returns). Comparisons are covered with explicit node pairs.
- The save/load round-trip loads into a separate tree whose `OnNewText` is
  silenced and whose `OnFreeNode` frees through a direct cast: the
  production handlers assume the main/import tree (node resolution) and
  the running application (list save, tray menu).
- `Tests.Execute` runs real processes so the launch path is exercised end to
  end: `/bin/true` on Unix and `%SystemRoot%\System32\whoami.exe` on Windows
  (both exit immediately). The single-instance probe uses `/bin/sleep` and is
  Unix-only. Missing-file launches fail on every platform; `ASuiteGuiTests`
  sets `ShowMessageDialogs := False` (see `Utility.Misc`) so the modal error
  dialog that `ExecWithShell` shows on Windows cannot block the run. Errors
  are still written to the log.
- `Tests.Settings` drives `LoadXMLSettings` with `Config.HotKey` off (the
  legacy loader replays the hotkey setters) and snapshots/restores every
  touched `Config` property. It never enables `StartOnWindowsStartup`
  (that writes an OS autostart entry) and only writes to the gitignored
  `backup/`/`cache/` folders.
- Deliberately NOT covered:
  - `THotkeyItemsList.Clear` and the desktop `HotkeyTriggerChangedEx`
    callback (they register/unregister real shortcuts; add/remove/lookup
    are covered with `Config.HotKey` off),
  - graphic-menu handlers (`DoSingleClickGM`) and image painting
    (`DoBeforeCellPaintHotkey`): they need the running menu form and the
    image lists,
  - documents and admin launches (`ExecuteAsAdmin`, URL/document open),
    `ExplorePath`, desktop-shortcut/autostart writes, and clipboard
    clearing,
  - icon extraction on real files (`GetAllIcons`, `TNodeIcon.LoadIcon`) and
    the icon thread (`TTreeIconsThread`),
  - `TConfiguration.RestoreSettings`/`SaveConfig`/`LoadConfig`,
    `SetCache`, `SetStartWithWindows`, the hotkey-string setters and
    `SetTrayIcon` (real files, cache folders, desktop registration,
    tray availability),
  - `LoadDatabaseFromXML` (it rewrites large parts of `Config` and the
    database), `XMLToTree` with `ltASuite2` (the dispatch has no branch
    for it, so the converter stays nil),
  - `TThemeEngine` getters (private), `Forms.Main`, `Forms.GraphicMenu`,
    `Forms.ScanFolder`, the modal dialogs (`Forms.About`,
    `Forms.UILogin`, `Forms.PropertyItem`, `Forms.PropertySeparator`,
    `Forms.Dialog.BaseEntity`) and the hotkey/trayicon frames.
