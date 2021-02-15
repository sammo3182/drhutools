#' @name fitPage
#' @title Shortcut for \code{flextable} to fit the MS Word output well.
#' 
#' @param ft A \code{\link[flextable]{flextable}} object.
#' @param pgwidth The width of the table in the Word document; the default is 6.
#' 
#' @import flextable
#' 
#' @export

fitPage <- function(ft, pgwidth = 6) {
        ft_out <- ft %>% autofit()
        
        ft_out <-
                width(ft_out,
                      width = dim(ft_out)$widths * pgwidth / (flextable_dim(ft_out)$widths))
        return(ft_out)
}
