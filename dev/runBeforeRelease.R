# CRAN Release Checklist for drhutools
# Run top to bottom after a functional update. Each step is independent: run it,
# confirm a clean result, then move to the next. Reference: https://r-pkgs.org/release.html

## 0. Bump version & metadata -------------------------------------------------
# Bump by hand, or let usethis bump and seed a matching NEWS.md heading:
#   usethis::use_version("patch")   # x.y.Z  -> bug fixes
#   usethis::use_version("minor")   # x.Y.0  -> new feature / new arguments
# Then make sure these three agree on the same version number:
#   [ ] DESCRIPTION        Version:
#   [ ] NEWS.md            top heading + bullet points for this release
#   [ ] cran-comments.md   one-line summary + the 0/0/0 result

## 1. Regenerate documentation ------------------------------------------------

devtools::document()   # roxygen2 -> man/*.Rd + NAMESPACE   (shortcut: Ctrl+Shift+D)
devtools::load_all()   # reload in the session to sanity-check the build

## 2. Check URLs --------------------------------------------------------------
# CRAN rejects dead or redirecting URLs. url_check() reports them and can
# auto-rewrite redirects when you pass the result to urlchecker::url_update().

urlchecker::url_check()

## 3. Build & preview vignettes -----------------------------------------------

devtools::build_vignettes()
# Pandoc note: RStudio/Positron set RSTUDIO_PANDOC automatically. If you run this
# from a bare Rscript and vignette building fails with "Pandoc is required", set:
#   Sys.setenv(RSTUDIO_PANDOC = "C:/Users/yuehu/AppData/Local/Programs/Quarto/bin/tools")

## 4. Local CRAN check -- the gate --------------------------------------------
# cran = TRUE applies CRAN policies; remote = TRUE runs CRAN's incoming-feasibility
# checks (needs internet); manual = TRUE builds the PDF manual (needs LaTeX via
# tinytex::install_tinytex()). Drop manual = TRUE if you have no LaTeX toolchain.
# Target result: 0 errors | 0 warnings | 0 notes.

devtools::check(cran = TRUE, remote = TRUE, manual = TRUE)

# Optional cross-platform check via R-hub v2 (runs on GitHub Actions; needs the
# repo pushed and rhub::rhub_setup() done once). Skip if not wanted:
# rhub::rhub_check()

## 5. Housekeeping before the tarball -----------------------------------------
# Remove compiled vignette HTML so it does not bloat the source tarball.

ls_html <- list.files(here::here("vignettes"), pattern = "\\.html$",
                      recursive = TRUE, full.names = TRUE)
if (length(ls_html)) file.remove(ls_html)

## 6. Submit to CRAN ----------------------------------------------------------
# submit_cran() builds the tarball, uploads it with cran-comments.md through the
# CRAN web form, then CRAN emails the maintainer address to confirm. Done.

devtools::submit_cran()

# Guided alternative that walks the standard release questions before submitting:
# devtools::release()

## 7. After CRAN acceptance ---------------------------------------------------
#   [ ] git tag the released commit (e.g. usethis::use_github_release())
#   [ ] DESCRIPTION  Version: -> x.y.z.9000   (back to a development version)
#   [ ] pkgdown::build_site()                 # refresh the docs website
