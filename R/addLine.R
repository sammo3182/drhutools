#' @name addLine
#' @title Shortcut for adding lines in a \code{\link[modelsummary]{modelsummary}} table.
#' @param term2add A character vector indicating the title of the added row.
#' @param signal A character vector indication the cell content in the added row.
#' @param ls_model A model project or a list of models.
#' @param tb_coef a named character vector. Values refer to the variable names that will appear in the table. Names refer to the original term names stored in the model object, e.g. c("hp:mpg"="hp X mpg") for an interaction term. Coefficients that are omitted from this vector will be omitted from the table. The table will be ordered in the same order as this vector. See more details in \code{\link[modelsummary]{modelsummary}}.
#' @param position2add The row name that the row is going to be added above.
#' 
#' @importFrom modelsummary modelsummary
#' @importFrom tibble rownames_to_column
#' 
#' @examples 
#' # Not run ####
#' # m_r2e <- map(ls_m, ~ clm(.x, data = df_r2e))
#' # tb_coef <- c(
#' #   "result_gap" = "Internal",
#' #   "result_gapRE" = "External"
#' # )
#' # 
#' # modelsummary(
#' #   m_r2e,
#' #   coef_map = tb_coef,
#' #   add_rows = addLine(term2add = "Fixed\n Effect",
#' #                      signal = "Yes",
#' #                      ls_model = m_r2e,
#' #                      tb_coef = tb_coef,
#' #                      position2add = "Num.Obs."))
#' #   )
#' 
#' @return A data.frame (or tibble) with the same number of columns as your main table. By default, rows are appended to the bottom of the table. See more details in \code{\link[modelsummary]{modelsummary}}.

globalVariables(c(".", "KSD", "term"))

addLine <- function(term2add,
         signal,
         ls_model,
         tb_coef,
         position2add) {
  addedLine <- c(term2add, rep(signal, times = length(ls_model))) %>%
    matrix(nrow = 1) %>%
    as_tibble
  names(addedLine) <- c("term", names(ls_model))
  attr(addedLine, 'position') <-
    modelsummary(ls_model, coef_map = tb_coef, output = "data.frame") %>%
    rownames_to_column() %>%
    filter(term == position2add) %>%
    .$rowname %>%
    as.numeric(.)
  return(addedLine)
}
