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
| `Tests.ConfigPaths.pas` | `TConfigPaths` layout and absolute/relative conversions |
| `Tests.Conversions.pas` | `Utility.Conversions` color/font serialization |
| `Tests.FileFolder.pas` | Side-effect-free helpers in `Utility.FileFolder` |
| `Tests.Scheduler.pas` | `TScheduler` firing logic (`CheckOnce/Daily/Hourly`) |
| `run_tests.sh` | Build + run helper used locally and by CI |

## Run locally

```bash
./tests/run_tests.sh            # gtk3 (override with WIDGETSET=qt6 ...)
```

Set `ASUITE_MIN_TESTS` to change the minimum total number of tests required
(default 100); the script fails if the suites shrink below it.

GUI control tests need a display server on Linux. If `DISPLAY` is not set
and `xvfb-run` is available, the script re-executes the GUI binary under
`xvfb-run` automatically.

## Run in CI

`.github/workflows/main.yml` and `main_release.yml` run both suites on the
Linux GTK2 job (`Xvfb :1`, `DISPLAY=:1`) before uploading the build.

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
- Deliberately NOT covered (they need the running application singletons
  `Config`/`ASuiteInstance`/`ASuiteManager`):
  - `THotkeyItemsList` (`Lists.HotKey`) and the `HotkeyTriggerChangedEx`
    callback,
  - `TDBManager` save/load round-trip (`Database.Manager`, `Database.List`:
    `TSQLtbl_list.Load` registers hotkeys via the list manager),
  - `ExtractFileNameEx` / `DeleteOldBackups` (same reason).
