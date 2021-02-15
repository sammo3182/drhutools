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
