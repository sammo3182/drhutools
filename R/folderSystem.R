#' @name folderSystem
#' @title Create Dr.Hu recommanded folder system for an academic paper project
#' 
#' @details The system contains specific places to store codes, output, data, manuscript (referred images and submission versions), and miscellaneous documents. It helps researchers to organize materials and drafts during the whole process of academic writing from data collection to manuscript publication. This is the recommanded folder structure for cooperative projects with members of Dr.Hu's Amazing Team.
#' 
#' @importFrom purrr walk here fs
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
      "codes/all r codes here; delete this file when locating the real files here.txt", 
      "data",
      "data/all data file here.csv", 
      "output",
      "output/results produced by codes and qmd here; delete this file when locating the real files here.txt.md", 
      "paper",
      "paper/template_cn.docx",
      "paper/images",
      "paper/images/noncode generated images; delete this file when locating the real files here.txt.md", 
      "paper/submission",
      "paper/submission/submitted files here; delete this file when locating the real files here.txt.md", 
      "document",
      "document/documents and materials; delete this file when locating the real files here.txt.md")
  
  template_path <- here("template_cn.docx")
  
  walk(ls_dir, function(path) {
    if (grepl("\\.\\w+$", path)) {
      dir_name <- dirname(path)
      if (!dir.exists(dir_name)) {
        dir.create(dir_name, recursive = TRUE)
      }
      if (!file.exists(path)) {
        if (basename(path) == "template_cn.docx") {
          file_copy(template_path, path)
        } else {
          file_create(path)
        }
      }
    } else {
      if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
      }
    }
  })
}

folderSystem()
