#' @name folderSystem
#' @title Folder hierarchy creator for academic research
#'
#' @details The function constructs a standardized folder hierarchy encompassing `codes/`, `data/`, `output/`, `paper/`, and `document/`. These directories are designated for storing programming scripts, data sets, processed outputs, manuscript drafts (including related images and submission documents), and assorted materials, respectively. This organization facilitates easy retrieval of project components, ensuring that members of Dr. Hu's Amazing Team can efficiently navigate and manage shared resources. This structure is advocated for collaborative projects to maintain uniformity across team operations.
#'
#' @return Local folders under the project's working directory.
#'
#' @export

folderSystem <- function() {
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
}
