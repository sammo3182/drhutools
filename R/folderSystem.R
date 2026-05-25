#' @name folderSystem
#' @title Folder hierarchy creator for academic research
#'
#' @details The function constructs a standardized folder hierarchy encompassing `codes/`, `data/`, `output/`, `paper/`, and `document/`. These directories are designated for storing programming scripts, data sets, processed outputs, manuscript drafts (including related images and submission documents), and assorted materials, respectively. This organization facilitates easy retrieval of project components, ensuring that members of Dr. Hu's Amazing Team can efficiently navigate and manage shared resources. This structure is advocated for collaborative projects to maintain uniformity across team operations.
#'
#' In addition, the manuscript-writing templates bundled with the package (a Quarto/Typst paper template, its main and appendix bibliographies, the OSCOLA citation style, and the citation-prefix Lua filter) are copied into the `paper/` folder so a new project starts ready to write.
#'
#' @param overwrite Logical. Should existing template files in `paper/` be overwritten? Defaults to `FALSE`, leaving any files the user has already edited untouched.
#'
#' @return Invisibly returns `NULL`. Called for its side effect of creating local folders, placeholder files, a default `.gitignore`, and the manuscript templates under the project's working directory.
#'
#' @export

folderSystem <- function(overwrite = FALSE) {
  ls_dir <- c(
    "codes",
    "data",
    "output",
    "paper",
    "paper/images",
    "paper/submission",
    "document",
    "codes/put codes here; delete this file after locating the real files.txt",
    "data/all data file here.csv",
    "output/image, results, and other output files here; delete this file after locating the real files here.txt",
    "paper/images/non-code-generated images here; delete this file after locating the real files.txt",
    "paper/submission/files for submission here; delete this file after locating the real files here.txt",
    "document/documents and materials here; delete this file after locating the real files.txt"
  )

  # First create all directories if they do not exist
  unique_dirs <- unique(dirname(ls_dir))
  lapply(unique_dirs, function(dir) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
  })

  # Then create files only if their respective directories already exist
  lapply(ls_dir, function(path) {
    if (grepl("\\.\\w+$", path) && !file.exists(path)) {
      file.create(path)
    }
  })

  # Copy the bundled manuscript templates into paper/
  template_dir <- system.file("template", package = "drhutools")
  if (nzchar(template_dir) && dir.exists(template_dir)) {
    template_files <- list.files(template_dir, full.names = TRUE)
    lapply(template_files, function(src) {
      dest <- file.path("paper", basename(src))
      if (overwrite || !file.exists(dest)) {
        file.copy(src, dest, overwrite = overwrite)
      }
    })
  } else {
    warning(
      "Could not locate the bundled 'template' folder; ",
      "no manuscript templates were copied into paper/.",
      call. = FALSE
    )
  }

  # Create default .gitignore in project root if not present
  gitignore_path <- ".gitignore"
  if (!file.exists(gitignore_path)) {
    gitignore_content <- c(
      "# R artifacts",
      ".Rhistory",
      ".RData",
      ".Ruserdata",
      ".Rproj.user/",
      "*.Rproj",
      "",
      "# Data files",
      "data/",
      "",
      "# Output",
      "output/",
      "",
      "# OS files",
      ".DS_Store",
      "Thumbs.db",
      "",
      "# Quarto / RMarkdown caches",
      "*_cache/",
      "*_files/",
      "*.html",
      "!paper/submission/*.html"
    )
    writeLines(gitignore_content, gitignore_path)
  }

  invisible(NULL)
}
