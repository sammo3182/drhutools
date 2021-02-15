text_wrapper <- function(label, dev_width = dev.size("in")[1], dev_scaler = 12)  {   
  paste(strwrap(label, dev_width * dev_scaler), collapse = "\n") 
}