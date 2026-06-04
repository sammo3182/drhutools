#' Save Leaflet Map as Image
#' @description Internal function to replace mapview::mapshot
#' @param map A leaflet map object
#' @param file Output file path
#' @param width Image width
#' @param height Image height
#' @noRd

save_leaflet_png <- function(map, file, width = 800, height = 600) {
  # create a temporary HTML file
  tmp_html <- tempfile(fileext = ".html")

  # save the leaflet object as HTML
  htmlwidgets::saveWidget(map, tmp_html, selfcontained = FALSE)

  # screenshot to PNG with webshot
  # note: requires PhantomJS (webshot::install_phantomjs())
  if (requireNamespace("webshot", quietly = TRUE)) {
    webshot::webshot(
      url = tmp_html, 
      file = file, 
      vwidth = width, 
      vheight = height,
      cliprect = "viewport"
    )
  } else {
    stop("Please install 'webshot' package to save map images.")
  }
  
  # clean up temporary files
  unlink(tmp_html)
}
