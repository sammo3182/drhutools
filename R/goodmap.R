#' @name goodChinaMap
#' @title Generate a customizable map of China with specific provinces and cities highlighted
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

#' @name generate_maps
#' @title function to generate maps for specified years
#'
#' @param data_file A string, the path to the CSV file containing the data. The file should contain the following columns: `g_lat`, `g_lon`, `g_pro`, `g_city`, `type`, `year_set`.
#' @param years A numeric vector, a vector containing the years for which maps should be generated.
#' @param saveDir A string, optional, the output directory where the generated map images and animated GIF will be stored. Default is "output".
#' @param map_center A numeric vector, optional, the center point of the map, given as latitude and longitude coordinates. Default is `c(35.8617, 104.1954)` (center of China).
#' @param zoom_level A numeric value, optional, the zoom level of the map. Default is `4`.
#' @param lat_col A string, optional, the column name for latitude. Default is `g_lat`.
#' @param lon_col A string, optional, the column name for longitude. Default is `g_lon`.
#' @param pro_col A string, optional, the column name for province. Default is `g_pro`.
#' @param city_col A string, optional, the column name for city. Default is `g_city`.
#' @param type_col A string, optional, the column name for type. Default is `type`.
#' @param palette A string, optional, the color palette for the map. Default is "main".
#' @param reverse_palette A logical value, optional, whether to reverse the color palette. Default is TRUE.
#' @param marker_radius A numeric value, optional, the radius of the map markers. Default is `1`.
#' @param legend_opacity A numeric value, optional, the opacity of the legend. Default is `0.7`.
#' @param width A numeric value, optional, the width of the output map images. Default is `800`.
#' @param height A numeric value, optional, the height of the output map images. Default is `900`.
#' 
#' @import dplyr
#' @import leaflet
#' @import htmlwidgets
#' @import webshot2
#' @import pals
#' @import webshot
#' 
#' @return The function saves the generated map images in the specified output directory.
#' 
#' @examples
#' Example dataset:
#' 
#'   wave                 name year_set    g_lat    g_lon  g_pro g_city type
#' 1    7 中国共产党历史展览馆     2021 40.00379 116.3994 北京市 北京市    7
#' 2    7 中央礼品文物管理中心     2021 39.89785 116.4109 北京市 北京市    7
#' 3    7           中国美术馆     2021 39.92535 116.4090 北京市 北京市    8
#' 4    7       中国电影博物馆     2021 39.99636 116.5206 北京市 北京市    8
#' 5    7   中国邮政邮票博物馆     2021 39.91081 116.4311 北京市 北京市    8
#' 6    7       中国钱币博物馆     2021 39.90215 116.3949 北京市 北京市    8
#' 
#' generate_maps(data_file = ".../file.csv", years = c(1997, 2001, 2005), saveDir = "output", palette = "digitMixed")
#' 
#' @export
#' 
generate_maps <- function(data_file, years, saveDir = "output", map_center = c(35.8617, 104.1954), 
                          zoom_level = 4, palette = "main", reverse_palette = TRUE, 
                          marker_radius = 1, legend_opacity = 0.7, width = 800, height = 900) {
  plot_data <- read.csv(data_file, header = TRUE, na.strings = c("NA"))
  plot_data$type <- as.factor(plot_data$type)
  
  type_colors <- colorFactor(palette = gb_pal(palette = palette, reverse = reverse_palette)(2), domain = plot_data$type)
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
        radius = marker_radius
      ) |>
      addLegend(
        "bottomright",
        pal = type_colors,
        values = ~type,
        title = "类型",
        opacity = legend_opacity
      )
    
    name_file <- paste0("map", year, ".png")
    mapshot(map, file = file.path(saveDir, name_file), vwidth = width, vheight = height, cliprect = c(0, 0, width, height))
  }
}

#' @name generate_animation
#' @title function to combine generated map images of different years into an animated GIF.
#' 
#' @import magick
#' 
#' @return An animated GIF combined the generated map images in the specified output directory
#' 
#' @example
#' generate_animation(saveDir = "output", years = c(1997, 2001, 2005), gif_name = "my_animation.gif", fps = 1)
#' 
#' @export
#' 

generate_animation <- function(saveDir = "output", years, gif_name = "maps.gif", fps = 0.5, loop = TRUE){
  image_files <- file.path(saveDir, paste0("map", years, ".png"))
  image_files <- image_files[file.exists(image_files)]
  images <- image_read(image_files)
  animation <- image_animate(images, fps = fps, loop = ifelse(loop, 0, 1))
  image_write(animation, file.path(saveDir, gif_name))
}
