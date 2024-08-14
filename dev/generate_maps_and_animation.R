#' Generate maps and animations for specified years
#'
#' This function generates maps for specified years and saves these maps as image files.
#' Finally, it combines the generated map images into an animated GIF.
#'
#' @param data_file A string, the path to the CSV file containing the data. The file should contain the following columns: `g_lat`, `g_lon`, `g_pro`, `g_city`, `type`, `year_set`.
#' @param years A numeric vector, a vector containing the years for which maps should be generated.
#' @param output_dir A string, optional, the output directory where the generated map images and animated GIF will be stored. Default is "output".
#' @param map_center A numeric vector, optional, the center point of the map, given as latitude and longitude coordinates. Default is `c(35.8617, 104.1954)` (center of China).
#' @param zoom_level A numeric value, optional, the zoom level of the map. Default is `4`.
#' @param lat_col A string, optional, the column name for latitude. Default is `g_lat`.
#' @param lon_col A string, optional, the column name for longitude. Default is `g_lon`.
#' @param pro_col A string, optional, the column name for province. Default is `g_pro`.
#' @param city_col A string, optional, the column name for city. Default is `g_city`.
#' @param type_col A string, optional, the column name for type. Default is `type`.
#' 
#' @import dplyr
#' @import leafletCN
#' @import leaflet
#' @import htmlwidget
#' @import webshot2
#' @import regioncode
#' @import pals
#' @import drhutools
#' @import rgeos
#' @import pacman
#' @import plyr
#' @import pacman
#' @import webshot
#' @import mapview
#' @import magick
#' 
#' @return The function saves the generated map images and animated GIF to the specified output directory.
#' 
#' @examples
#' years <- c(1997, 2001, 2005, 2009, 2017, 2019, 2021)
#' generate_maps_and_animation("data_file.csv", years)
#' 
#' @export
generate_maps_and_animation <- function(data_file, years, output_dir = "output", map_center = c(35.8617, 104.1954), zoom_level = 4) {
  Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
  plot_data <- read.csv(data_file, header = TRUE, na.strings = c("NA"))
  plot_data$type <- as.factor(plot_data$type)
  type_colors <- colorFactor(palette = gb_pal(palette = "main", reverse = TRUE)(2), domain = plot_data$type)
  for (year in years) {
    filtered_data <- plot_data |>
      filter(year_set == year) |>
      select(g_lat, g_lon, g_pro, g_city, type)
    
    map <- leaflet(filtered_data) |>
      amap() |>
      setView(lng = map_center[2], lat = map_center[1], zoom = zoom_level) |>
      addCircleMarkers(
        lng = ~g_lon, lat = ~g_lat,
        color = ~type_colors(type),
        fillOpacity = 1,
        popup = ~paste("类型:", type),
        radius = 1
      ) |>
      addLegend(
        "bottomright",
        pal = type_colors,
        values = ~type,
        title = "类型",
        opacity = 0.7
      )
    name_file <- paste0("map", year, ".png")
    mapshot(map, file = file.path(output_dir, name_file), vwidth = 800, vheight = 900, cliprect = c(0, 0, 800, 900))
  }
  image_files <- file.path(output_dir, paste0("map", years, ".png"))
  image_files <- image_files[file.exists(image_files)]
  images <- image_read(image_files)
  animation <- image_animate(images, fps = 0.5)
  image_write(animation, file.path(output_dir, "maps.gif"))
}
years <- c(1997, 2001, 2005, 2009, 2017, 2019, 2021)
generate_maps_and_animation("file.csv", years)
