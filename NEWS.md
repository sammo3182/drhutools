# drhutools 1.1.2

- `folderSystem` now assembles a configurable manuscript template instead of copying a single fixed one. New arguments (each prompts interactively when left `NULL`, and falls back to the first option in non-interactive sessions):
  - `language`: `"english"` writes `paper_en.qmd`; `"chinese"` writes a new `paper_cn.qmd` (Chinese heading numbering, 图/表 cross-references, dual Chinese/English abstract, and an online appendix, modelled on the bundled `organizedParticipation.qmd`).
  - `format`: `"typst"` (PDF) or `"word"` (`.docx`).
  - `bibliography`: `"single"` (one reference list via Quarto citeproc) or `"separate"` (separate main-text and appendix lists via the Alexandria Typst package; Typst only, so Word output is forced to `"single"`).
  - `csl`: `"apsa"` (American Political Science Association) or `"apa"` (APA 7th edition).
- `folderSystem` now copies only the support files the chosen configuration needs (the selected `.csl`, the matching `.bib` file(s), and the relevant Lua filters) rather than the whole bundled template folder.
- Added the Chinese single-bibliography localization filters (`fix-lang.lua`, `localize-cnbib.lua`) and a `references.bib` placeholder for the single-bibliography layout; replaced the OSCOLA style with the APSA and APA citation styles.

# 1.1.1

- `goodmap` now correctly draws on the Amap (AutoNavi) Chinese basemap by default instead of OpenStreetMap:
  - New `tile_source` argument (`"amap"` default, or `"osm"`) selects the basemap provider.
  - New `coord` argument (`"WGS-84"` default, `"GCJ-02"`, or `"BD-09"`) declares the coordinate system of point input; points are reprojected to match the basemap so markers align with the tiles.
  - Added an internal closed-form WGS-84 / GCJ-02 / BD-09 coordinate converter (no new dependency).
- Added a default `.gitignore` file into `folderSystem`
- `folderSystem` now copies the bundled manuscript templates (Quarto/Typst paper template, main/appendix bibliographies, OSCOLA citation style, and the citation-prefix Lua filter) into `paper/`, with an `overwrite` argument to control replacing existing files

# 1.1.0

- Absorbed functions from Bear Braumoeller's archived CRAN package `QCAfalsePositive` for continued accessibility:
  - `csQCAbinTest`: binomial test for Type I error in crisp-set QCA
  - `mvQCAbinTest`: binomial test for Type I error in multi-value QCA
  - `fsQCApermTest`: permutation test for Type I error in fuzzy-set QCA
  - `p.threshold.adjust`: adjusted p-value threshold utility used by `fsQCApermTest`
  - `plot.fsQCApt`, `summary.csQCAbt`, `summary.fsQCApt`, `summary.mvQCAbt`: S3 methods for printing and plotting QCA test results
- Added two datasets: `Arab.Spring` and `social.revolutions`
- Relicensed from MIT to GPL (>= 3) to comply with the source package license

# 1.0.1

- Fixed the issue caused by `mapview`
- Add reference information in `traits`
- Add more information in the output of `traits`

# 1.0.0

- Add new function `goodmap`
- CRAN submission

# 0.1.2

- `folderSystem` create the folder system automatically at the project's root directory.

# 0.1.1

- Removed the useless functions:
  - `textWrapper` has been superseded by `ggfittext` functions.
  - `fitPage` has been deprecated because with the recent update of `gt` package, it support word output well. Dr. Hu's team lean on `gt` tables.
  - `addLine` has been deprecated. The recent version of `modelsummary` has a convenient build-in function `add_rows` to add custom lines into model summary tables. Together with the "data.frame" type of output, users can easily locate and add lines at the position they want.
- `ksplot` is superseded by the `cdplot` function
- Added the `folderSystem` function
- Clean the document

# 0.1.0

The initial release of the package.
