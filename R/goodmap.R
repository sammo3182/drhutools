utils::globalVariables(c("g_lat", "g_lon", "prov", "city", "animate_set", "variable", "value_var", "value_set"))

#' The `goodmap` function is designed to create interactive PNG Map or GIF Map from a provided data file.
#' It supports two types of maps: `point` and `polygon`.
#' The function can visualize data by either plotting points based on geographical coordinates
#' or highlighting regions polygon based on their administrative boundaries (province or city level).
#' Additionally, the function can generate animated that showcase the change of data.
#'
#' If the map type is `point`, the color and size of the points will be determined by the
#' `value_set` column in the data file, which means the different value of each point.
#' If the map type is `polygon`, the color of the polygons will be determined by the average
#' value of the `variable` column for each city or province in the data file.
#'
#' @param data_file Dataframe.
#'                  When generate point map, `data_file` should include required columns such as `g_lat` and `g_lon`.
#'                  When generate polygon map, `data_file` should include required columns such as `prov` or `city`.
#'                  The `prov` columns must be complete names, such as “内蒙古自治区” instead of “内蒙古”.
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
#' @param zoom_level A numeric value specifying the zoom level for the map. Default is 4.
#' @param custom_colors A vector of colors for customizing the color gradient. Default is `NULL`,
#'                      which uses the predefined color palette.
#' @param base_radius A numeric value specifying the base radius for point markers on point maps. Default is 1.
#' @param radius_factor A numeric value specifying the multiplier for adjusting the radius of
#'                      point markers based on the `value_set`. Default is 1.
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
#' @import animation
#' @import png
#'
#' @examples
#' data_file <- data.frame(
#'   name = c("航天科技空间技术研究院展示中心", "长辛店二七纪念馆", "天津觉悟社纪念馆", 
#'            "五台白求恩纪念馆（白求恩模范病室旧址）", "中国中铁装备集团郑州盾构总装车间", 
#'            "昆仑关抗日战役纪念地", "海南解放公园", "南充市阆中红军烈士纪念园", 
#'            "厂窖惨案遇难同胞纪念馆", "雅安市红军长征翻越夹金山纪念馆", 
#'            "中华苏维埃人民共和国川滇黔省革命委员会旧址", "抚顺雷锋纪念馆", 
#'            "杨靖宇烈士陵园", "瑷珲历史陈列馆", "中国共产党第一次全国代表大会会址纪念馆", 
#'            "河姆渡遗址博物馆", "陶行知纪念馆"),
#'   animate_set = c(2021, 2021, 2021, 2021, 2021, 2021, 2021, 2017, 2017, 2017, 2017, 
#'                1997, 1997, 1997, 1997, 1997, 1997),
#'   g_lat = c(39.947298, 39.830932, 39.159621, 38.745234, 34.705527, 23.090849, 
#'             20.008295, 31.564526, 29.153561, 30.368317, 27.302689, 41.850161, 
#'             41.7295, 49.977569, 31.220653, 29.962122, 29.865772),
#'   g_lon = c(116.322434, 116.20602, 117.196032, 113.58242, 113.755818, 108.685362, 
#'             109.715334, 105.974878, 112.248827, 102.811716, 105.28199, 123.801936, 
#'             125.962291, 127.493741, 121.47536, 121.349437, 118.436866),
#'   g_pro = c("北京市", "北京市", "天津市", "山西省", "河南省", "广西壮族自治区", 
#'             "海南省", "四川省", "湖南省", "四川省", "贵州省", "辽宁省", 
#'             "吉林省", "黑龙江省", "上海市", "浙江省", "安徽省"),
#'   g_city = c("北京市", "北京市", "天津市", "忻州市", "郑州市", "南宁市", "临高县", 
#'              "南充市", "益阳市", "雅安市", "毕节市", "抚顺市", "通化市", 
#'              "黑河市", "上海市", "宁波市", "黄山市"),
#'   value_set = c(8, 4, 4, 4, 8, 6, 6, 5, 2, 4, 4, 9, 5, 8, 4, 1, 3)
#' )
#' 
#' Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
#' goodmap(
#'   data_file = data_file,
#'   type = "point",
#'   animate = TRUE,
#'   animate_var = "animate_set",
#'   custom_colors = "pink",
#'   base_radius = 1, radius_factor = 1
#' )
#' @export

goodmap <- function(data_file,
                    type = "point",
                    level = NULL,
                    animate = FALSE,
                    animate_var = NULL,
                    map_center = c(35.8617, 104.1954),
                    zoom_level = 4,
                    custom_colors = NULL,
                    base_radius = 1,
                    radius_factor = 1,
                    legend_opacity = 0.7,
                    width = 800,
                    height = 900) {
  temp_saveDir <- file.path(getwd(), "temp_maps")
  dir.create(temp_saveDir, showWarnings = FALSE)
  
  if (!webshot::is_phantomjs_installed()) {
    webshot::install_phantomjs()
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
        filter(!!sym(animate_var) == input_var_value)
    }
    
    if (type == "point") {
      plot_point <- filtered_data |>
        select(g_lat, g_lon, value_set) |>
        mutate(radius = base_radius + (as.numeric(value_set) * radius_factor))
      
      if (!is.null(custom_colors)) {
        type_colors <- colorRampPalette(c("black", custom_colors))(length(unique(data_file$value_set)))
      } else {
        type_colors <- gb_pal(palette = "main", reverse = TRUE)(length(unique(data_file$value_set)))
      }
      
      domain <- unique(data_file$value_set)
      color_mapping <- type_colors
      type_colors <- colorFactor(palette = color_mapping, domain = domain)
      
      name_prefix <- "point_map"
      if (animate) {
        name_file <- paste0(name_prefix, "_", input_var_value, ".png")
      } else {
        name_file <- paste0(name_prefix, ".png")
      }
      image_file <- file.path(temp_saveDir, name_file)
      
      grDevices::png(image_file, width = width, height = height, res = 300)
      
      map <- leaflet(plot_point) |>
        amap() |>
        setView(
          lng = map_center[2],
          lat = map_center[1],
          zoom = zoom_level
        ) |>
        addCircleMarkers(
          lng = ~g_lon,
          lat = ~g_lat,
          color = ~ type_colors(value_set),
          fillOpacity = 1,
          stroke = FALSE,
          popup = ~ paste("Value"),
          radius = ~radius
        ) |>
        addLegend(
          "bottomright",
          pal = type_colors,
          values = ~value_set,
          title = if (is.null(animate_var)) "Value" else paste("Value", input_var_value),
          opacity = legend_opacity
        )
      
      print(map)
      grDevices::dev.off()
    } else if (type == "polygon") {
      suppressWarnings({
        if (level == "province") {
          plot_prov <- filtered_data |>
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
          
          
          name_prefix <- "province_map"
          if (animate) {
            name_file <- paste0(name_prefix, "_", input_var_value, ".png")
          } else {
            name_file <- paste0(name_prefix, ".png")
          }
          image_file <- file.path(temp_saveDir, name_file)
          
          grDevices::png(image_file, width = width, height = height, res = 300)
          
          map <- geojsonMap(plot_prov_var,
                            mapName = "china",
                            palette = value_colors,
                            colorMethod = "numeric",
                            legendTitle = if (is.null(animate_var)) "Value" else paste("Value", input_var_value),
                            na.color = "transparent"
          )
          print(map)
          grDevices::dev.off()
        } else if (level == "city") {
          plot_city <- filtered_data |>
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
          map <- geojsonMap(plot_city_var,
                            mapName = "city",
                            palette = value_colors,
                            colorMethod = "numeric",
                            legendTitle = if (is.null(animate_var)) "Value" else paste("Value", input_var_value),
                            na.color = "transparent"
          )
        }
      })
    }
    
    name_prefix <- switch(type,
                          "point" = "point_map",
                          "polygon" = ifelse(level == "province", "province_map", "city_map"),
                          "map"
    )
    
    if (animate) {
      name_file <- paste0(name_prefix, "_", input_var_value, ".png")
    } else {
      name_file <- paste0(name_prefix, ".png")
    }
    image_file <- file.path(temp_saveDir, name_file)
    mapshot(
      map,
      file = file.path(temp_saveDir, name_file),
      vwidth = width,
      vheight = height,
      res = 300
    )
    return(file.path(temp_saveDir, name_file))
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
    saveGIF(
      {
        for (img in images) {
          grid::grid.newpage()
          grid::grid.raster(img)
        }
      },
      movie.name = temp_gif_path,
      interval = 1
    )
    
    if (file.exists(temp_gif_path)) {
      file.remove(temp_gif_path)
    }
    
  } else {
    map_file <- generate_map(all_data = TRUE)
    img <- png::readPNG(map_file)
    grid::grid.raster(img)
  }
  if (dir.exists("temp_maps")) {
    unlink("temp_maps", recursive = TRUE)
  }
}
