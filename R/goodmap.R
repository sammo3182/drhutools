#' Goodmap Function for Generating Maps and Animations
#'
#' The `goodmap` function is designed to create interactive maps and animations
#' based on the provided dataset. It supports two types of maps: "point" and "polygon".
#' The function can visualize data by either plotting points based
#' on geographical coordinates or highlighting regions based on their
#' administrative boundaries (province or city level). Additionally, the function
#' can generate animated GIFs that showcase changes over time.
#' If it is the "point" type, the color and size of the point would be determined by the "type" col in the dataset.
#' If it is the "polygon" type, the color of the polygons would be determined by the average amount of the "variable" col
#' of each city or province in the dataset.
#'
#' @param data_file A string specifying the path to the CSV file containing the data.
#'                  The dataset should include required columns such as 'g_lat' and 'g_lon'
#'                  for point maps, and 'prov' or 'city' for polygon maps.
#' @param type A string specifying the type of map to generate. Options are "point"
#'             for point maps using latitude and longitude, or "polygon" for maps
#'             with administrative boundaries.
#' @param level A string specifying the level of administrative boundaries for polygon maps.
#'              Acceptable values are "province" or "city". This parameter is required
#'              if `type` is "polygon".
#' @param palette A character vector indicating the name of palette in gb_palettes. Available palettes:
#' \itemize{
#'   \item \code{main}: Gold and black colors.
#'   \item \code{tricol}: Gold, black, and dark grey to create a gradual effect.
#'   \item \code{digitMixed}: Five-pack colors specified for digital publications.
#'   \item \code{printMixed}: Five-pack colors specified for printed publications.
#'   \item \code{full}: A palette including all the colors \code{gb_cols} can call.
#' }
#' @param reverse A logic vector indicating whether the palette should be reversed; the default is FALSE.
#' @param animate A logical value indicating whether to generate an animation from the maps.
#' @param animate_var A string specifying the variable to animate over (typically "year").
#' @param years A numeric vector specifying the years for which maps should be generated.
#'              Each map will be saved as a separate image file.
#' @param saveDir A string specifying the directory where map images and animations will be saved.
#'                The default is "output".
#' @param map_center A numeric vector of length 2 specifying the latitude and longitude
#'                   for the center of the map view. Default is `c(35.8617, 104.1954)`,
#'                   which is approximately the center of China.
#' @param zoom_level A numeric value specifying the zoom level for the map. Default is 4.
#' @param palette A string specifying the color palette to use for the map. Default is "main".
#' @param reverse_palette A logical value indicating whether to reverse the color palette. Default is TRUE.
#' @param base_radius A numeric value specifying the base radius for point markers on point maps.
#'                    Default is 1.
#' @param radius_factor A numeric value specifying the multiplier for adjusting the radius of
#'                      point markers based on the type. Default is 1.
#' @param legend_opacity A numeric value specifying the opacity of the legend. Default is 0.7.
#' @param width A numeric value specifying the width of the saved map images. Default is 800.
#' @param height A numeric value specifying the height of the saved map images. Default is 900.
#'
#' @return The function saves the generated maps as PNG files in the specified `saveDir`
#'         and, if `animate = TRUE`, creates an animated GIF showcasing changes over time.
#'
#' @examples
#' #' Example dataset when 'type = "polygon"'
#' system setting before library the package: Sys.setlocale(, "Chinese")
#' 
#' id   city              prov year   variable
#' 1 乌鲁木齐 新疆维吾尔自治区 2010   0.2861395
#' 2     拉萨       西藏自治区 2010   0.3881083
#' 3 呼和浩特   内蒙古尔自治区 2010   0.9466682
#' 4     西宁           青海省 2011   0.8360043
#' 5     成都           四川省 2011   0.4622928
#' 6   哈尔滨         黑龙江省 2011   0.1387102
#'
#' Example for generating polygon maps at the province level with animation
#' goodmap(data_file, type = "polygon", level = "province", animate = TRUE,
#'         animate_var = "year", years = c(2011, 2012))
#'
#' Example for generating polygon maps at the city level with animation
#' goodmap(data_file, type = "polygon", level = "city", animate = TRUE,
#'         animate_var = "year", years = c(2011, 2012))
#'
#' Example dataset when type = "point"
#' system setting before library the package: Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
#' 
#'   wave                 name year_set    g_lat    g_lon  g_pro g_city type
#' 1    7 中国共产党历史展览馆     2021 40.00379 116.3994 北京市 北京市    7
#' 2    7 中央礼品文物管理中心     2021 39.89785 116.4109 北京市 北京市    7
#' 3    7           中国美术馆     2021 39.92535 116.4090 北京市 北京市    8
#' 4    7       中国电影博物馆     2021 39.99636 116.5206 北京市 北京市    8
#' 5    7   中国邮政邮票博物馆     2021 39.91081 116.4311 北京市 北京市    8
#' 6    7       中国钱币博物馆     2021 39.90215 116.3949 北京市 北京市    8
#'
#' Example for generating point maps with animation
#' goodmap(data_file, type = "point", animate = TRUE,
#'         animate_var = "year", years = c(2019, 2020), base_radius = 1, radius_factor = 1)
#'
#' @import dplyr
#' @import leaflet
#' @import readr
#' @import htmlwidgets
#' @import mapview
#' @import purrr
#' @import gganimate
#' @import magick
#' @import sf
#' @export


library(dplyr)
library(leaflet)
library(readr)
library(htmlwidgets)
library(mapview)
library(purrr)
library(gganimate)
library(magick)
library(sf)

goodmap <- function(data_file, type = "point", level = NULL, animate = FALSE, animate_var = NULL, years = NULL,
                    saveDir = "output", map_center = c(35.8617, 104.1954), zoom_level = 4,
                    palette = "main", reverse_palette = TRUE, base_radius = 1, radius_factor = 1,
                    legend_opacity = 0.7, width = 800, height = 900) {

  # 使用 read_csv 读取文件并设置编码
  plot_data <- read.csv(data_file, header=TRUE, na.strings=c("NA"))

  # 检查地图类型和必要列
  if (type == "point") {
    if (!all(c("g_lat", "g_lon") %in% colnames(plot_data))) {
      stop("数据需要包含经纬度列 'g_lat' 和 'g_lon'。")
    }
  } else if (type == "polygon") {
    if (is.null(level) || !(level %in% c("province", "city"))) {
      stop("多边形地图需要指定 'level' 为 'province' 或 'city'。")
    }
    if (level == "province" && !("prov" %in% colnames(plot_data))) {
      stop("数据需要包含 'prov' 列来指定省份。")
    } else if (level == "city" && !("city" %in% colnames(plot_data))) {
      stop("数据需要包含 'city' 列来指定城市。")
    }
  } else {
    stop("未知的地图类型，请选择 'point' 或 'polygon'。")
  }
  
  # 定义颜色
  type_colors <- colorFactor(palette = gb_pal(palette = palette, reverse = reverse_palette)(2), domain = plot_data$type)
  
  # 根据类型生成地图
  generate_map <- function(year) {
    filtered_data <- plot_data |>
      filter(year == year)

    if (type == "point") {
      filtered_data <- filtered_data |>
        select(g_lat, g_lon, type) |>
        mutate(radius = base_radius + (as.numeric(type) * radius_factor))

      map <- leaflet(filtered_data) |>
        amap() |>
        setView(lng = map_center[2], lat = map_center[1], zoom = zoom_level) |>
        addCircleMarkers(
          lng = ~g_lon, lat = ~g_lat,
          color = ~type_colors(type),
          fillOpacity = 1,
          stroke = FALSE,
          popup = ~paste("类型:", type),
          radius = ~radius
        ) |>
        addLegend(
          "bottomright",
          pal = type_colors,
          values = ~type,
          title = "类型",
          opacity = legend_opacity
        )

    } else if (type == "polygon") {
      if (level == "province") {
        plot_prov <- plot_data |>
          filter(year == year) |>
          select(prov, variable) |>
          group_by(prov) |>
          summarise(value_var = mean(variable, na.rm = TRUE)) |>
          ungroup() |>
          right_join(data.frame(name = regionNames("china")), by = c("prov" = "name"))|>
          filter(!is.na(value_var))
        
        plot_prov_var <- select(plot_prov, prov, value_var) |>
          rename(value = value_var) |>
          as.data.frame()

        map <- geojsonMap(plot_prov_var, mapName = "china",
                          palette = gb_pal(palette = "main", reverse = TRUE)(2),
                          colorMethod = "numeric",
                          legendTitle = paste("Variable", year))

      } else if (level == "city") {
        plot_city <- plot_data |>
          filter(year == year) |>
          select(city, variable) |>
          group_by(city) |>
          summarise(value_var = mean(variable, na.rm = TRUE)) |>
          ungroup() |>
          right_join(data.frame(name = regionNames("city")), by = c("city" = "name"))|>
          filter(!is.na(value_var))

        plot_city_var <- select(plot_city, city, value_var) |>
          rename(value = value_var) |>
          as.data.frame()
        
        map <- geojsonMap(plot_city_var, mapName = "city",
                          palette = gb_pal(palette = "full", reverse = TRUE)(2),
                          colorMethod = "numeric",
                          legendTitle = paste("variable", year))
      }
    }

    # 生成不同的文件名
    name_prefix <- switch(type,
                          "point" = "point_map",
                          "polygon" = ifelse(level == "province", "province_map", "city_map"),
                          "map") # 默认情况

    # 保存地图
    name_file <- paste0(name_prefix, "_", year, ".png")
    mapshot(map, file = file.path(saveDir, name_file), vwidth = width, vheight = height)

    return(name_file)
  }
  
  # 生成地图
  if (!is.null(years)) {
    map_files <- lapply(years, generate_map)
  } else {
    stop("请提供 'years' 参数。")
  }

  # 生成动画
  if (animate) {
    # 使用与地图生成时相同的文件名前缀
    name_prefix <- switch(
      type,
      "point" = "point_map_",
      "polygon" = switch(
        level,
        "province" = "province_map_",
        "city" = "city_map_",
        "unknown_level"  # 用于处理其他或未知的 level 值
      ),
      "unknown_type"  # 用于处理其他或未知的 type 值
    )
    
    image_files <- file.path(saveDir, paste0(name_prefix, years, ".png"))
    image_files <- image_files[file.exists(image_files)]
    
    if (length(image_files) == 0) {
      stop("没有找到任何生成的地图文件，无法生成动画。")
    }
    
    images <- image_read(image_files)
    animation <- image_animate(images, fps = 0.5, loop = ifelse(TRUE, 0, 1))
    image_write(animation, file.path(saveDir, paste0("animation_", animate_var, ".gif")))
  }
}
