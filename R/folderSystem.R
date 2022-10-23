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
      "docs",
      "paper/images",
      "paper/submission",
      "paper/workingPaper",
      "document")
  
  walk(ls_dir, ~ if (!dir.exists(.))
    dir.create(file.path(.), recursive = TRUE))
  
  download.file("https://cloud.tsinghua.edu.cn/f/ff08000d4ca546ddb0b5/?dl=1", paste0(getwd(), "/paper/woringPaper.zip"))
  unzip(paste0(getwd(), "/paper/woringPaper.zip"), exdir = paste0(getwd(), "/paper/workingPaper"))
  unlink(paste0(getwd(), "/paper/woringPaper.zip"))
  download.file("https://cloud.tsinghua.edu.cn/f/bf8e9a7fef3a4913b096/?dl=1", paste0(getwd(), "/docs/docs.zip"))
  unzip(paste0(getwd(), "/docs/docs.zip"), exdir = paste0(getwd(), "/docs"))
  unlink(paste0(getwd(), "/docs/docs.zip"))
}
