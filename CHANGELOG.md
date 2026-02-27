# Changelog

## [2.7.1] - 2026-02-27

### New Features

- **Toggle black screen during presentation**: A new button in the presentation control bar lets you instantly blank the screen to black and restore it again. The keyboard shortcut `B` has the same effect.

### Bug Fixes

- Fixed memory leaks: `DestroyPresentationStyleSettings` was a no-op and never freed the owned `TFont`, causing fonts to leak on every presentation start, style preview refresh, and image export.
- Fixed a SIGSEGV crash when closing Cantara while a presentation was active: `FormHide` was firing during form destruction (after `FormDestroy` had already freed objects), leading to null pointer dereferences via `PresentationCanvas` and invalid window handle accesses.
- Replaced all bare `.Destroy` calls in destructors and `FormDestroy` handlers with `FreeAndNil` to guard against nil pointer crashes during teardown.
- Fixed a memory leak in `TLoadImageThread`: the stored `TPresentationStyleSettings.Font` was never freed when overwritten by a new `LoadData` call.
- Fixed a memory leak in `TfrmSettings.gbPresentationClick`: the existing preview canvas was not freed before creating a new one.

---

## [2.7.0] - 2026-02-21

### New Features

- **Per-song style overrides**: Each song in the selection can now have its own background image, background colour, text colour, font, transparency, and horizontal alignment — independent of the global presentation settings. A dedicated "Song Style" dialog is accessible via the right-click context menu on any song.
- **Per-song PPTX masters**: When exporting to PowerPoint, each song can use its own custom master slide.
- **Fade transition between slides**: A smooth cross-fade effect between slides can be enabled in the settings. The fade duration is configurable (default: 150 ms).
- **Black screen on empty slide**: A new setting causes Cantara to show a pure black screen instead of the background when an empty slide is displayed.
- **JSON song selection export/import**: Song selections can now be saved and loaded as `.json` files. The format bundles song content, per-song style overrides, and background images into a single portable file, enabling full round-trip export/import across machines.
- **Windows dark mode**: Cantara now follows the system dark mode setting on Windows.
- **Windows installer**: An Inno Setup installer is now available for Windows.
- **Third-party libraries dialog**: A new dialog lists all third-party libraries used by Cantara along with their licences.

### Improvements

- Improved Flatpak permissions handling: Cantara now uses the XDG portal for file access in sandboxed environments and provides a standard open dialog for selecting background images.
- Improved text rendering with a custom word-wrap function for more accurate line breaking.
- Improved font rendering quality settings.
- Updated and extended translations.
- The main window title has been simplified to "Cantara".

### Bug Fixes

- Fixed a crash (SIGSEGV) when starting a presentation after songs were added via "Select All" or drag-and-drop, or after using the search filter — the `TRepoFile` object reference was not attached to list entries in those code paths.
- Fixed the song editor not opening the correct song when invoked from the context menu (wrong file lookup and a form initialisation timing issue).
- Fixed background colour and transparency changes not being reflected in the song style preview dialog.
- Fixed background colour and transparency changes not being applied in the presentation window and image export when two songs share the same background image but use different colours.
- Fixed metadata parsing truncating field values that contained a colon character.
- Fixed a layout overlap of the background colour panel and an incorrect sign on the transparency value in the song style dialog.
- Fixed a type cast error in the PPTX custom master slide lookup.
- Fixed an exception when closing the application under certain conditions.
- Fixed `.txt` files not being recognised as valid song files.
- Fixed several text sizing and line-length calculation issues in the presentation canvas.
- Fixed a content-area layout bug affecting Cantara running as a Flatpak.

---

## [2.6.0] - 2024-05-31

Initial baseline for this changelog.
