#' @name text_wrapper
#' @title Function to wrap 
#' 
#' @param label A character string to wrap.
#' @param dev_width A numeric value specifying width of current device.
#' @param dev_scaler A numeric value to scale; the default is 12.
#' 
#' @return A character vector with wrapped signs.
#' 
#' @examples 
#' library(ggplot2)
#' ggplot(data = cars, aes(x = speed, y = dist)) +    
#' geom_smooth() +   
#'   labs(title = text_wrapper("A so very looooooooooooooooooooooooooooooong title"))
#'   
#' @export


text_wrapper <-
  function(label,
           dev_width = dev.size("in")[1],
           dev_scaler = 12)  {
    paste(strwrap(label, dev_width * dev_scaler), collapse = "\n")
  }
