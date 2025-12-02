#' Save Leaflet Map as Image
#' @description Internal function to replace mapview::mapshot
#' @param map A leaflet map object
#' @param file Output file path
#' @param width Image width
#' @param height Image height
#' @noRd

save_leaflet_png <- function(map, file, width = 800, height = 600) {
  # 创建临时 HTML 文件
  tmp_html <- tempfile(fileext = ".html")
  
  # 将 leaflet 对象保存为 HTML
  htmlwidgets::saveWidget(map, tmp_html, selfcontained = FALSE)
  
  # 使用 webshot 截图保存为 PNG
  # 注意：用户电脑需要安装 PhantomJS (webshot::install_phantomjs())
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
  
  # 清理临时文件
  unlink(tmp_html)
}
