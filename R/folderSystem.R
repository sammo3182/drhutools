#' @name folderSystem
#' @title Create Dr.Hu recommanded folder system for an academic paper project
#' 
#' @details The system contains specific places to store codes, output, data, manuscript (referred images and submission versions), and miscellaneous documents. It helps researchers to organize materials and drafts during the whole process of academic writing from data collection to manuscript publication. This is the recommanded folder structure for cooperative projects with members of Dr.Hu's Amazing Team.
#' 
#' @importFrom purrr walk
#' 
#' @return A set of folders under the working directory.
#' 
#' @examples 
#' 
#' # Not run
#' 
#' # folderSystem()
#' 
#' @export

folderSystem <- function() {
  ls_dir <-
    c("codes",
      "data",
      "output",
      "paper",
      "paper/images",
      "paper/submission",
      "document")
  
  walk(ls_dir, ~ if (!dir.exists(.))
    dir.create(file.path(.), recursive = TRUE))
}

