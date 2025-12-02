utils::globalVariables(c("g_lat", "g_lon", "prov", "city", "animate_set", "value_var", "value_set"))

#' The `goodmap` function is designed to create interactive PNG Map or GIF Map from a provided data file.
#' It supports two types of maps: `point` and `polygon`.
#' The function can visualize data by either plotting points based on geographical coordinates
#' or highlighting regions polygon based on their administrative boundaries (province or city level).
#' Additionally, the function can generate animated that showcase the change of data.
#'
#' If the map type is `point`, the color and size of the points will be determined by the
#' `value_set` column in the data file, which means the different value of each point.
#' If the map type is `polygon`, the color of the polygons will be determined by the average
#' value of the `value_set` column for each city or province in the data file.
#'
#' @param data_file Dataframe.
#'                  When generate point map, `data_file` should include required columns such as `g_lat` and `g_lon`.
#'                  When generate polygon map, `data_file` should include required columns such as `prov` or `city`.
#'                  The `prov` columns must be complete, official names rather than any shortened form or abbreviation.
#'                  If there is only incomplete names or geocodes in your data_file, we recommend you to use function `regioncode`
#'                  as a one-step solution to these conversion from incomplete names.
#'                  Ensure the file is formatted correctly with appropriate column headers.
#' @param type A string specifying the type of map to generate. Options are `point`for point maps using `g_lat` and `g_lon`,
#'             or `polygon` for maps with administrative boundaries.
#' @param level A string specifying the level of administrative boundaries for polygon maps.
#'              Acceptable values are `province` or `city`. This parameter is required if `type` is `polygon`.
#' @param animate A logical value indicating whether to generate an animation from the maps. The default is FALSE.
#'                If animate is FALSE, the whole data will be generated as a PNG file.
#'                If animate is TRUE, an animation will be generated from all panel data, the animate_var must be assigned.
#' @param animate_var A string specifying the variable to animate over. 
#'                The default is `NULL`, the whole data will be generated as a PNG file.
#'                If an animation is needed, `data_file` should include required the `animate_set` column.
#'                `animate_set` includes a series of unique values that represents year or month, or other categorical variable.
#'                The frame number of the animation depends on the `animate_set`. 
#' @param map_center A numeric vector of length 2 specifying the latitude and longitude
#'                   for the center of the map view. Default is `c(35.8617, 104.1954)`, which
#'                   is approximately the center of China.
#' @param zoom_level A numeric value specifying the zoom level for the map. Default is 3.
#' @param color_type If the data is discrete, such as types or categories, choose `factor`. 
#'                   If the data is continuous, such as temperature or pressure, choose `numeric`. Default is numeric.
#' @param custom_colors A vector of colors for customizing the color gradient. Default is `NULL`,
#'                      which uses the predefined color palette.
#' @param point_radius A numeric value specifying the radius for point markers on point maps. Default is 5.
#' @param legend_opacity A numeric value specifying the opacity of the legend. Default is 0.7.
#' @param legend_name The name of the legend. Default is `Value`.
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
#' @import purrr
#' @import gganimate
#' @import sf
#' @import animation
#' @import png
#'
#' @examples
#' 
#' \dontrun{
#' goodmap(
#'    toy_poly,
#'    type = "polygon",
#'    level = "province"
#' )
#' }
#' 
#' @export

goodmap <- function(data_file,
                    type = "point",
                    level = NULL,
                    animate = FALSE,
                    animate_var = NULL,
                    map_center = c(35.8617, 104.1954),
                    zoom_level = 4,
                    color_type = "numeric",
                    custom_colors = NULL,
                    point_radius = 5,
                    legend_opacity = 0.7,
                    legend_name = NULL,
                    width = 800,
                    height = 900) {
  
  temp_saveDir <- file.path(tempdir(), "temp_maps")
  if (!dir.exists(temp_saveDir)) {
    dir.create(temp_saveDir, showWarnings = FALSE)
  }
  
  if (type == "point") {
    if (!all(c("g_lat", "g_lon") %in% colnames(data_file))) {
      stop("The data must include 'g_lat' and 'g_lon' columns for latitude and longitude.")
    }
  } else if (type == "polygon") {
    if (is.null(level) || !(level %in% c("province", "city"))) {
      stop("Polygon maps require 'level' to be either 'province' or 'city'.")
    }
    if (level == "province" && !("prov" %in% colnames(data_file))) {
      stop("The data must include a 'prov' column to specify the province.")
    } else if (level == "city" && !("city" %in% colnames(data_file))) {
      stop("The data must include a 'city' column to specify the city.")
    }
  } else {
    stop("Unknown map type, please select either 'point' or 'polygon'.")
  }
  
  generate_map <- function(input_var_value = NULL, all_data = FALSE) {
    if (animate == FALSE || all_data) {
      filtered_data <- data_file
    } else {
      filtered_data <- data_file |>
        dplyr::filter(!!sym(animate_var) == input_var_value)
    }
    
    map <- NULL
    

    if (type == "point") {
      plot_point <- filtered_data |>
        dplyr::select(g_lat, g_lon, value_set) |>
        dplyr::mutate(radius = point_radius)
      
      # 定义颜色
      if (color_type == "factor") {
        if (!is.null(custom_colors)) {
          pal_colors <- grDevices::colorRampPalette(c("black", custom_colors))(length(unique(data_file$value_set)))
        } else {
          pal_colors <- grDevices::colorRampPalette(c("black", "gold"))(length(unique(data_file$value_set)))
        }
        type_colors <- leaflet::colorFactor(palette = pal_colors, domain = unique(data_file$value_set))
      } else {
        if (!is.null(custom_colors)) {
          pal_colors <- grDevices::colorRampPalette(c("black", custom_colors))(length(unique(data_file$value_set)))
        } else {
          if (exists("gb_pal")) {
            pal_colors <- gb_pal(palette = "main", reverse = TRUE)(length(unique(data_file$value_set)))
          } else {
            pal_colors <- grDevices::topo.colors(length(unique(data_file$value_set)))
          }
        }
        type_colors <- leaflet::colorNumeric(palette = pal_colors, domain = unique(data_file$value_set))
      }
      
      map <- leaflet::leaflet(plot_point) |>
        leaflet::addTiles() |>
        leaflet::setView(lng = map_center[2], lat = map_center[1], zoom = zoom_level) |>
        leaflet::addCircleMarkers(
          lng = ~g_lon, lat = ~g_lat,
          color = ~type_colors(value_set),
          fillOpacity = 1, stroke = FALSE,
          popup = ~paste("Value:", value_set),
          radius = ~radius
        ) |>
        leaflet::addLegend(
          "bottomright", pal = type_colors, values = ~value_set,
          title = if (!is.null(legend_name)) legend_name else "Value",
          opacity = legend_opacity
        )
      

    } else if (type == "polygon") {
      
      # 通用颜色生成器
      get_pal_fun <- function(vals) {
        n_colors <- length(unique(vals))
        if (n_colors < 2) n_colors <- 2 
        
        if (color_type == "factor") {
          if (!is.null(custom_colors)) {
            cols <- grDevices::colorRampPalette(c("black", custom_colors))(n_colors)
          } else {
            cols <- grDevices::colorRampPalette(c("black", "gold"))(n_colors)
          }
          return(leaflet::colorFactor(palette = cols, domain = vals))
        } else {
          if (!is.null(custom_colors)) {
            cols <- grDevices::colorRampPalette(c("black", custom_colors))(n_colors)
          } else {
            if (exists("gb_pal")) {
              cols <- gb_pal(palette = "main")(n_colors)
            } else {
              cols <- grDevices::topo.colors(n_colors)
            }
          }
          return(leaflet::colorNumeric(palette = cols, domain = vals))
        }
      }
      
      if (level == "province") {
        plot_prov <- filtered_data |>
          dplyr::group_by(prov) |>
          dplyr::summarise(value = mean(value_set, na.rm = TRUE)) |>
          dplyr::ungroup()
        
        map_data <- dplyr::left_join(china_prov_sf, plot_prov, by = c("name" = "prov"))
        pal_fun <- get_pal_fun(map_data$value)
        
        map <- leaflet::leaflet(data = map_data) |>
          leaflet::addTiles() |>
          leaflet::addPolygons(
            fillColor = ~pal_fun(value), weight = 1, opacity = 1, color = "white",
            dashArray = "3", fillOpacity = 0.7,
            highlightOptions = leaflet::highlightOptions(weight = 3, color = "#666", fillOpacity = 0.7, bringToFront = TRUE),
            label = ~paste0(name, ": ", value),
            popup = ~paste0("<strong>", name, "</strong><br>Value: ", value)
          ) |>
          leaflet::addLegend(pal = pal_fun, values = ~value, opacity = 0.7, title = if (!is.null(legend_name)) legend_name else "Value", position = "bottomright")
        
      } else if (level == "city") {
        plot_city <- filtered_data |>
          dplyr::select(city, value_set) |>
          dplyr::group_by(city) |>
          dplyr::summarise(value = mean(value_set, na.rm = TRUE)) |>
          dplyr::ungroup()
        
        map_data <- dplyr::left_join(china_city_sf, plot_city, by = c("name" = "city"))
        pal_fun <- get_pal_fun(map_data$value)
        
        map <- leaflet::leaflet(data = map_data) |>
          leaflet::addTiles() |>
          leaflet::addPolygons(
            fillColor = ~pal_fun(value), weight = 1, opacity = 1, color = "white",
            dashArray = "3", fillOpacity = 0.7,
            highlightOptions = leaflet::highlightOptions(weight = 3, color = "#666", fillOpacity = 0.7, bringToFront = TRUE),
            label = ~paste0(name, ": ", value),
            popup = ~paste0("<strong>", name, "</strong><br>Value: ", value)
          ) |>
          leaflet::addLegend(pal = pal_fun, values = ~value, opacity = 0.7, title = if (!is.null(legend_name)) legend_name else "Value", position = "bottomright")
      }
    }
    
    name_prefix <- switch(type, "point" = "point_map", "polygon" = ifelse(level == "province", "province_map", "city_map"), "map")
    name_file <- if (animate) paste0(name_prefix, "_", input_var_value, ".png") else paste0(name_prefix, ".png")
    image_file <- file.path(temp_saveDir, name_file)
    
    save_leaflet_png(map = map, file = image_file, width = width, height = height)
    
    # 只有非动画模式才在 View 窗口显示
    if (!animate) {
      print(map)
    }
    
    return(image_file)
  }
  
  if (animate) {
    if (!(animate_var %in% colnames(data_file))) {
      stop(paste("The specified 'animate_var' column does not exist in the data:", animate_var))
    }
    unique_vars <- unique(data_file[[animate_var]])
    
    images <- list()
    for (input_var_value in unique_vars) {
      map_file <- generate_map(input_var_value)
      img <- png::readPNG(map_file)
      images[[length(images) + 1]] <- img
    }
    
    temp_gif_path <- file.path(tempdir(), "animated_map.gif")
    if (file.exists(temp_gif_path)) file.remove(temp_gif_path)
    
    animation::saveGIF({
      for (img in images) {
        grid::grid.newpage()
        grid::grid.raster(img)
      }
    }, movie.name = temp_gif_path, interval = 1)
  } else {
    generate_map(all_data = TRUE)
  }
  
  if (dir.exists("temp_maps")) {
    unlink("temp_maps", recursive = TRUE)
  }
}

#' Internal function to save leaflet map
#' @noRd
save_leaflet_png <- function(map, file, width, height) {
  tmp_html <- tempfile(fileext = ".html")
  htmlwidgets::saveWidget(map, tmp_html, selfcontained = FALSE)
  if (requireNamespace("webshot", quietly = TRUE)) {
    webshot::webshot(tmp_html, file = file, vwidth = width, vheight = height, cliprect = "viewport")
  }
  unlink(tmp_html)
}
