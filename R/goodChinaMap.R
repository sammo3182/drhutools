#' Generate a Customizable Map of China with mark specific Provinces and Cities
#'
#' This function generates a map of China highlighting specified provinces and cities.
#' The colors of the provinces, province borders, cities, and city borders can be customized.
#' The generated map is saved as a PNG file in the specified directory.
#'
#' @param province_data A character vector containing the names of provinces to be highlighted on the map. Default is `NULL`.
#' @param city_data A character vector containing the names of cities to be marked on the map. Default is `NULL`.
#' @param saveDir A character string specifying the directory where the output PNG file will be saved. Default is `"output"`.
#' @param width The width of the saved PNG file, in pixels. Default is `1200`.
#' @param height The height of the saved PNG file, in pixels. Default is `800`.
#' @param province_color A character string specifying the fill color for the highlighted provinces. Default is `"yellow"`.
#' @param province_border_color A character string specifying the border color for the highlighted provinces. Default is `"blue"`.
#' @param city_color A character string specifying the color for the city markers. Default is `"red"`.
#' @param city_border_color A character string specifying the border color for the city markers. Default is `"black"`.
#'
#' @return A character string with the file path of the generated PNG map.
#' 
#' @import leaflet 
#' @import htmlwidgets 
#' @import webshot2 
#' @import dplyr 
#' @import mapview 
#' 
#' @export
#'
#' @examples
#' 
#'   goodChinaMap(
#'   province_data = c("吉林省", "山东省"), 
#'   city_data = c("北京", "海口"), 
#'   province_color = "grey", 
#'   province_border_color = "purple", 
#'   city_color = "black", 
#'   city_border_color = "gold"
#'   )
#'   

goodChinaMap <- function(
    province_data = NULL, 
    city_data = NULL, 
    saveDir = "output", 
    width = 1200, 
    height = 800, 
    province_color = "yellow", 
    province_border_color = "blue", 
    city_color = "red", 
    city_border_color = "black", 
    ...
) {

  filePath_province = system.file("data-raw/china.json",package = "drhutools")
  china_map_province <- read.geoShape(filePath_province)
  
  filePath_city = system.file("data-raw/city.json",package = "drhutools")
  china_map_city <- read.geoShape(filePath_city)
  
  dir.create(saveDir, showWarnings = FALSE)
  
  map <- leaflet() |>
    amap() |>
    setView(lng = 104.1954, lat = 35.8617, zoom = 4)
  
  if (!is.null(province_data)) {
    for (province in province_data) {
      polygons_map <- china_map_province[china_map_province$name == province, ]
      if (nrow(polygons_map) == 0) {
        warning(paste("Province not found:", province))
        next
      }
      map <- map |>
        addPolygons(
          data = polygons_map, 
          fillColor = province_color, 
          stroke = TRUE, 
          color = province_border_color, 
          weight = 1, 
          fillOpacity = 0.5, 
          label = ~name
        )
    }
  }
  
  if (!is.null(city_data)) {
    for (city in city_data) {
      plots_map <- china_map_city[china_map_city$name == city, ]
      if (nrow(plots_map) == 0) {
        warning(paste("City not found:", city))
        next
      }
      if (length(plots_map@polygons) == 0 || length(plots_map@polygons[[1]]@Polygons) == 0) {
        warning(paste("No polygons found for city:", city))
        next
      }
      city_center <- plots_map@polygons[[1]]@Polygons[[1]]@labpt
      lng <- city_center[1]
      lat <- city_center[2]
      map <- map |>
        addCircleMarkers(
          lat = lat, 
          lng = lng, 
          popup = city, 
          radius = 6, 
          color = city_color, 
          fillColor = city_border_color, 
          fillOpacity = 1
        )
    }
  }
  
  html_plot <- tempfile(fileext = ".html")
  saveWidget(map, html_plot, selfcontained = TRUE)
  
  name_file <- paste0("map_combined.png")
  mapshot(map, file = file.path(saveDir, name_file), vwidth = width, vheight = height)
  
  return(file.path(saveDir, name_file))
}
