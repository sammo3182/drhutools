utils::globalVariables(c("g_lat", "g_lon", "prov", "city", "year_set", "variable", "value_var"))

#' The `goodmap` function is designed to create interactive PNG Map or GIF Map
#' The `goodmap` function creates interactive map animations from a provided data file.
#' It supports two types of maps: `point` and `polygon`.
#' The function can visualize data by either plotting points based on geographical coordinates 
#' or highlighting regions based on their administrative boundaries (province or city level). 
#' Additionally, the function can generate animated that showcase changes over time.
#' 
#' If the map type is `point`, the color and size of the points will be determined by the 
#' `type` column in the data file.
#' If the map type is `polygon`, the color of the polygons will be determined by the average 
#' value of the `variable` column
#' for each city or province in the data file.
#'
#' @param data_file Dataframe.
#'                  When generate point map, `data_file` should include required columns such 
#'                  as `g_lat` and `g_lon`.
#'                  When generate polygon map, `data_file` should include required columns such 
#'                  as `prov` or `city`.
#'                  The `prov` columns must be complete names, such as “内蒙古自治区” instead of “内蒙古”.
#'                  If there is only incomplete names or geocodes in your data_file, we recommend you to use function `regioncode` 
#'                  as a one-step solution to these conversion from incomplete names.
#'                  Ensure the file is formatted correctly with appropriate column headers.
#' @param type A string specifying the type of map to generate. Options are `point`for point 
#'             maps using `g_lat` and `g_lon`, 
#'             or `polygon` for maps with administrative boundaries.
#' @param level A string specifying the level of administrative boundaries for polygon maps.
#'              Acceptable values are `province` or `city`. This parameter is required if `type` is `polygon`.
#' @param animate A logical value indicating whether to generate an animation from the maps. The default is FALSE.
#'                If animate is FALSE, the map for the latest year will be generated as a PNG file. 
#'                If animate is TRUE, an animation will be generated from all panel data. 
#' @param animate_var A string specifying the variable to animate over. The default is `year_set`.
#' @param map_center A numeric vector of length 2 specifying the latitude and longitude
#'                   for the center of the map view. Default is `c(35.8617, 104.1954)`, which 
#'                   is approximately the center of China.
#' @param zoom_level A numeric value specifying the zoom level for the map. Default is 4.
#' @param custom_colors A vector of colors for customizing the color gradient. Default is `NULL`, 
#'                      which uses the predefined color palette.
#' @param base_radius A numeric value specifying the base radius for point markers on point maps. Default is 1.
#' @param radius_factor A numeric value specifying the multiplier for adjusting the radius of
#'                      point markers based on the type. Default is 1.
#' @param legend_opacity A numeric value specifying the opacity of the legend. Default is 0.7.
#' @param width A numeric value specifying the width of the map images. Default is 800.
#' @param height A numeric value specifying the height of the map images. Default is 900.
#'
#' @return Image in the viewer.
#' 
#' @import dplyr
#' @import magick
#' @import webshot
#' @import leaflet
#' @import htmlwidgets
#' @import mapview
#' @import purrr
#' @import gganimate
#' @import sf
#' @import rlang
#'
#' @examples
#' data_file <- data.frame(
#'   name = c(
#'     "中国共产党历史展览馆", "中央礼品文物管理中心", "中国美术馆",
#'     "中国电影博物馆", "中国邮政邮票博物馆", "中国钱币博物馆"
#'   ),
#'   year_set = c(2021, 2021, 2021, 2021, 2021, 2021),
#'   g_lat = c(40.00379, 39.89785, 39.92535, 39.99636, 39.91081, 39.90215),
#'   g_lon = c(116.3994, 116.4109, 116.4090, 116.5206, 116.4311, 116.3949),
#'   prov = c("北京市", "北京市", "北京市", "北京市", "北京市", "北京市"),
#'   city = c("北京", "北京", "北京", "北京", "北京", "北京"),
#'   type = c(7, 7, 8, 8, 8, 8)
#' )
#' Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
#' goodmap(data_file = data_file,
#'   type = "point",
#'   animate = TRUE,
#'   animate_var = "year_set",
#'   custom_colors = "pink", 
#'   base_radius = 1, radius_factor = 1)
#'
#' @export

goodmap <- function(data_file, 
                    type = "point", 
                    level = NULL, 
                    animate = FALSE, 
                    animate_var = "year_set",
                    map_center = c(35.8617, 104.1954), 
                    zoom_level = 4,
                    custom_colors = NULL, 
                    base_radius = 1, 
                    radius_factor = 1,
                    legend_opacity = 0.7, 
                    width = 800, 
                    height = 900) {
  temp_saveDir <- tempdir()
  
  if (!webshot::is_phantomjs_installed()) {
    webshot::install_phantomjs()
  }
  
  if (type == "point") {
    if (!all(c("g_lat", "g_lon") %in% colnames(data_file))) {
      stop("he data must include 'g_lat' and 'g_lon' columns for latitude and longitude.")
    }
  } else if (type == "polygon") {
    if (is.null(level) || !(level %in% c("province", "city"))) {
      stop("Polygon maps require 'level' to be either 'province' or 'city'.")
    }
    if (level == "province" && !("prov" %in% colnames(data_file))) {
      stop("The data must include a 'prov' column to specify the province.")
    } else if (level == "city" &&
               !("city" %in% colnames(data_file))) {
      stop("The data must include a 'city' column to specify the city.")
    }
  } else {
    stop("Unknown map type, please select either 'point' or 'polygon'.")
  }
  
  generate_map <- function(input_var_value) {
    filtered_data <- data_file |>
      filter(!!sym(animate_var) == input_var_value)
    
    if (type == "point") {
      filtered_data <- filtered_data |>
        select(g_lat, g_lon, type) |>
        mutate(radius = base_radius + (as.numeric(type) * radius_factor))
      
  if (!is.null(custom_colors)) {
    type_colors <- colorRampPalette(c("black", custom_colors))(length(unique(data_file$type)))
  } else {
    type_colors <- gb_pal(palette = "main", reverse = TRUE)(length(unique(data_file$type)))
  }
  
  domain <- unique(data_file$type)
  color_mapping <- type_colors
  type_colors <- colorFactor(palette = color_mapping, domain = domain)
      
      map <- leaflet(filtered_data) |>
        amap() |>
        setView(lng = map_center[2],
                lat = map_center[1],
                zoom = zoom_level) |>
        addCircleMarkers(
          lng = ~ g_lon,
          lat = ~ g_lat,
          color = ~ type_colors(type),
          fillOpacity = 1,
          stroke = FALSE,
          popup = ~ paste("Type:", type),
          radius = ~ radius
        ) |>
        addLegend(
          "bottomright",
          pal = type_colors,
          values = ~ type,
          title = paste("Type", input_var_value),
          opacity = legend_opacity
        )
      
    } else if (type == "polygon") {
      suppressWarnings({
        if (level == "province") {
        plot_prov <- data_file |>
          filter(!!sym(animate_var) == input_var_value) |>
          select(prov, variable) |>
          group_by(prov) |>
          summarise(value_var = mean(variable, na.rm = TRUE)) |>
          ungroup() |>
          right_join(data.frame(name = regionNames("china")), by = c("prov" = "name")) |>
          filter(!is.na(value_var))
        
        plot_prov_var <- select(plot_prov, prov, value_var) |>
          rename(value = value_var) |>
          as.data.frame()
        
        if (!is.null(custom_colors)) {
          value_colors <- colorRampPalette(c("black", custom_colors))(length(unique(plot_prov_var$value)))
        } else {
          value_colors <- gb_pal(palette = "main", reverse = TRUE)(length(unique(plot_prov_var$value)))
        }
        domain <- unique(plot_prov_var$value)
        value_color_mapping <- colorFactor(palette = value_colors, domain = domain)
        map <- geojsonMap(plot_prov_var, mapName = "china",
                          palette = value_colors,
                          colorMethod = "numeric",
                          legendTitle = paste("Variable", input_var_value),
                          na.color = "transparent")
        
        } else if (level == "city") {
          plot_city <- data_file |>
            filter(!!sym(animate_var) == input_var_value) |>
            select(city, variable) |>
            group_by(city) |>
            summarise(value_var = mean(variable, na.rm = TRUE)) |>
            ungroup() |>
            right_join(data.frame(name = regionNames("city")), by = c("city" = "name")) |>
            filter(!is.na(value_var))
          
          plot_city_var <- select(plot_city, city, value_var) |>
            rename(value = value_var) |>
            as.data.frame()
          if (!is.null(custom_colors)) {
            value_colors <- colorRampPalette(c("black", custom_colors))(length(unique(plot_city_var$value)))
          } else {
            value_colors <- gb_pal(palette = "main", reverse = TRUE)(length(unique(plot_city_var$value)))
          }
          
          domain <- unique(plot_city_var$value)
          value_color_mapping <- colorFactor(palette = value_colors, domain = domain)
          map <- geojsonMap(plot_city_var, mapName = "city",
                            palette = value_colors,
                            colorMethod = "numeric",
                            legendTitle = paste("Variable", input_var_value),
                            na.color = "transparent")
        }
      })
    }
    
    name_prefix <- switch(
      type,
      "point" = "point_map",
      "polygon" = ifelse(level == "province", "province_map", "city_map"),
      "map"
    )
    
    name_file <- paste0(name_prefix, "_", input_var_value, ".png")
    image_file <- file.path(temp_saveDir, name_file)
    mapshot(
      map,
      file = file.path(temp_saveDir, name_file),
      vwidth = width,
      vheight = height
    )
    
    return(map)
  }
  
  if (animate) {
    if (!(animate_var %in% colnames(data_file))) {
      stop(paste("The specified 'animate_var' column does not exist in the data:", animate_var))
    }
    unique_vars <- unique(data_file[[animate_var]])
    map_files <- lapply(unique_vars, generate_map)
  } else {
    if  (!(animate_var %in% colnames(data_file))) {
      stop(paste("The specified 'animate_var' column does not exist in the data:", animate_var))
    }
    
    latest_var <- max(data_file[[animate_var]], na.rm = TRUE)
    map_file <- generate_map(latest_var)
    return(map_file)
  }
  
  if (animate) {
    name_prefix <- switch(type,
                          "point" = "point_map_",
                          "polygon" = switch(
                            level,
                            "province" = "province_map_",
                            "city" = "city_map_",
                            "unknown_level"
                          ),
                          "unknown_type")
    
    image_files <- file.path(temp_saveDir, paste0(name_prefix, unique(data_file[[animate_var]]), ".png"))
    image_files <- image_files[file.exists(image_files)]
    
    if (length(image_files) == 0) {
      stop("No map files found, unable to create animation.")
    }
    
    images <- image_read(image_files)
    animation <- image_animate(images, fps = 0.5, loop = ifelse(TRUE, 0, 1))
    
    animation_file <- file.path(temp_saveDir, paste0("animation_", unique(data_file[[animate_var]])[1], ".gif"))
    image_write(animation, animation_file)
    return(animation)
  }
}
