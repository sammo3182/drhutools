## Submission summary

This is a feature and maintenance update (1.1.2).

* `folderSystem()` now assembles a configurable Quarto manuscript template
  (language, output format, bibliography layout, and citation style) and copies
  only the support files the chosen configuration needs.
* `goodmap()` polygon maps were repaired and simplified, and its point/polygon
  colour-palette logic was unified.
* Internal clean-up: deduplicated the QCA false-positive tests, removed unused
  helper code, and dropped the unused `htmltools`, `gganimate`, and `magick`
  dependencies.

## Test environments

* Local Windows 11, R Under development (unstable) (2026-04-04 r89791 ucrt)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes
