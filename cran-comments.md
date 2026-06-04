## Submission summary

This is a feature update (1.1.1). The `goodmap()` function now uses the
Gaode/Amap (AutoNavi) Chinese basemap by default instead of OpenStreetMap, and
gains `tile_source` and `coord` arguments together with an internal coordinate
converter (WGS-84 / GCJ-02 / BD-09). The minimum R version is now declared as
R (>= 4.1.0) to match the package's use of the native pipe `|>`.

## Test environments

* Local Windows 11, R Under development (unstable) (2026-04-04 r89791 ucrt)
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes
