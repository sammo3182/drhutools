utils::globalVariables(c("g_lat", "g_lon", "value_set", "value", "name"))

#' Draw a standard map of China from a data frame
#'
#' `goodmap()` creates an interactive PNG (or animated GIF) map from a data
#' frame. It supports two map types: `point` and `polygon`. Point maps plot
#' markers from geographic coordinates; polygon maps shade administrative
#' regions (province or city level) by the average of a value column. Setting
#' `animate = TRUE` strings per-frame maps into a GIF.
#'
#' For point maps the marker colour and size follow the `value_set` column. For
#' polygon maps each region is shaded by the mean of `value_set` for that city
#' or province.
#'
#' @param data_file Data frame.
#'                  For point maps it must include `g_lat` and `g_lon` columns.
#'                  For polygon maps it must include `prov` (province level) or
#'                  `city` (city level) columns holding the complete, official
#'                  names rather than abbreviations. Use a tool such as
#'                  `regioncode` to expand incomplete names beforehand.
#' @param type A string, `point` (default) or `polygon`.
#' @param level A string, `province` or `city`. Required when `type = "polygon"`.
#' @param animate Logical. Generate an animation across `animate_var`? Defaults
#'                to `FALSE`, which draws a single PNG from the whole data.
#' @param animate_var A string naming the column to animate over. One frame is
#'                drawn per unique value. Required when `animate = TRUE`.
#' @param map_center A length-2 numeric `c(lat, lon)` for the map centre.
#'                Default `c(35.8617, 104.1954)`, roughly the centre of China.
#' @param zoom_level Numeric initial zoom level. Default `4`.
#' @param tile_source A string, `amap` (Gaode/Amap Chinese basemap, default) or
#'                `osm` (OpenStreetMap). Amap tiles use the GCJ-02 coordinate
#'                system (see `coord`). Baidu is unsupported because its BD-09
#'                projection is incompatible with the standard leaflet tile grid.
#' @param coord A string giving the coordinate system of the **point** input
#'                (`g_lat`/`g_lon`): `WGS-84` (default, GPS/international
#'                geocoders), `GCJ-02` (Amap/Gaode/Tencent), or `BD-09` (Baidu).
#'                Points are reprojected to match `tile_source`. Ignored for
#'                polygon maps, whose bundled boundaries are GCJ-02 encoded.
#' @param color_type `numeric` (default) for continuous values or `factor` for
#'                discrete categories.
#' @param custom_colors Optional vector of colours for the gradient. Default
#'                `NULL` uses the package's gold-black palette.
#' @param point_radius Numeric marker radius for point maps. Default `5`.
#' @param legend_opacity Numeric legend opacity. Default `0.7`.
#' @param legend_name Legend title. Default `Value`.
#' @param width Numeric image width. Default `800`.
#' @param height Numeric image height. Default `900`.
#'
#' @return The path to the rendered image, invisibly for animations. Called
#'   mainly for its side effect of displaying the map in the viewer.
#'
#' @import dplyr
#' @import leaflet
#' @import htmlwidgets
#' @import webshot
#' @import animation
#' @import png
#' @import sf
#' @importFrom grid grid.newpage grid.raster
#' @importFrom grDevices colorRampPalette
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
                    tile_source = "amap",
                    coord = "WGS-84",
                    color_type = "numeric",
                    custom_colors = NULL,
                    point_radius = 5,
                    legend_opacity = 0.7,
                    legend_name = NULL,
                    width = 800,
                    height = 900) {
  type <- match.arg(type, c("point", "polygon"))
  tile_source <- match.arg(tile_source, c("amap", "osm"))
  coord <- match.arg(coord, c("WGS-84", "GCJ-02", "BD-09"))

  # Validate inputs up front.
  if (type == "point") {
    if (!all(c("g_lat", "g_lon") %in% names(data_file))) {
      stop("Point maps require 'g_lat' and 'g_lon' columns.")
    }
  } else {
    level <- match.arg(level, c("province", "city"))
    key <- if (level == "province") "prov" else "city"
    if (!key %in% names(data_file)) {
      stop(sprintf("%s-level polygon maps require a '%s' column.", level, key))
    }
  }
  if (animate && !(animate_var %in% names(data_file))) {
    stop("The 'animate_var' column does not exist in the data: ", animate_var)
  }

  temp_dir <- file.path(tempdir(), "temp_maps")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  legend_title <- if (is.null(legend_name)) "Value" else legend_name

  # One palette builder for both map types; `reverse` keeps the previous
  # point/polygon defaults (points run gold->black, polygons black->gold).
  make_pal <- function(vals, reverse = FALSE) {
    vals <- vals[!is.na(vals)]
    n <- max(length(unique(vals)), 2L)
    if (!is.null(custom_colors)) {
      cols <- colorRampPalette(c("black", custom_colors))(n)
    } else if (color_type == "factor") {
      cols <- colorRampPalette(c("black", "gold"))(n)
    } else {
      cols <- gb_pal(palette = "main", reverse = reverse)(n)
    }
    if (color_type == "factor") {
      leaflet::colorFactor(cols, domain = vals)
    } else {
      leaflet::colorNumeric(cols, domain = vals)
    }
  }

  # Polygon boundaries are the same for every frame: fetch them once.
  boundary_sf <- NULL
  if (type == "polygon") {
    boundary_sf <- sf::st_as_sf(readGeoLocal(if (level == "province") "\u4e2d\u56fd" else "city"))
  }

  generate_map <- function(frame_value = NULL) {
    filtered <- if (animate && !is.null(frame_value)) {
      dplyr::filter(data_file, !!dplyr::sym(animate_var) == frame_value)
    } else {
      data_file
    }

    if (type == "point") {
      plot_point <- dplyr::select(filtered, g_lat, g_lon, value_set)

      # Reproject points onto the basemap's system (Amap = GCJ-02, OSM = WGS-84).
      target_crs <- if (tile_source == "amap") "GCJ-02" else "WGS-84"
      if (coord != target_crs) {
        xy <- convert_coord(plot_point$g_lon, plot_point$g_lat, from = coord, to = target_crs)
        plot_point$g_lon <- xy$lon
        plot_point$g_lat <- xy$lat
      }

      pal <- make_pal(data_file$value_set, reverse = TRUE)
      map <- leaflet::leaflet(plot_point) |>
        add_basemap(tile_source) |>
        leaflet::setView(lng = map_center[2], lat = map_center[1], zoom = zoom_level) |>
        leaflet::addCircleMarkers(
          lng = ~g_lon, lat = ~g_lat,
          color = ~ pal(value_set),
          fillOpacity = 1, stroke = FALSE,
          popup = ~ paste("Value:", value_set),
          radius = point_radius
        ) |>
        leaflet::addLegend(
          "bottomright",
          pal = pal, values = ~value_set,
          title = legend_title, opacity = legend_opacity
        )
    } else {
      summarised <- filtered |>
        dplyr::group_by(!!dplyr::sym(key)) |>
        dplyr::summarise(value = mean(value_set, na.rm = TRUE), .groups = "drop")

      map_data <- dplyr::left_join(boundary_sf, summarised, by = stats::setNames(key, "name"))
      pal <- make_pal(map_data$value)

      map <- leaflet::leaflet(data = map_data) |>
        add_basemap(tile_source) |>
        leaflet::addPolygons(
          fillColor = ~ pal(value), weight = 1, opacity = 1, color = "white",
          dashArray = "3", fillOpacity = 0.7,
          highlightOptions = leaflet::highlightOptions(
            weight = 3, color = "#666", fillOpacity = 0.7, bringToFront = TRUE
          ),
          label = ~ paste0(name, ": ", value),
          popup = ~ paste0("<strong>", name, "</strong><br>Value: ", value)
        ) |>
        leaflet::addLegend(
          "bottomright",
          pal = pal, values = ~value,
          title = legend_title, opacity = legend_opacity
        )
    }

    prefix <- if (type == "point") "point_map" else paste0(level, "_map")
    image_file <- file.path(
      temp_dir,
      if (animate) paste0(prefix, "_", frame_value, ".png") else paste0(prefix, ".png")
    )
    save_leaflet_png(map, image_file, width = width, height = height)
    if (!animate) print(map)
    image_file
  }

  if (!animate) {
    return(invisible(generate_map()))
  }

  # Animation: one frame per unique value, stitched into a GIF.
  images <- lapply(unique(data_file[[animate_var]]), function(v) png::readPNG(generate_map(v)))
  gif_path <- file.path(tempdir(), "animated_map.gif")
  if (file.exists(gif_path)) file.remove(gif_path)
  animation::saveGIF(
    for (img in images) {
      grid::grid.newpage()
      grid::grid.raster(img)
    },
    movie.name = gif_path, interval = 1
  )
  invisible(gif_path)
}

#' Add a basemap tile layer
#'
#' Adds the Gaode/Amap Chinese basemap or OpenStreetMap to a leaflet map. Amap
#' serves standard XYZ tiles in Web Mercator (imagery is GCJ-02 encoded), so it
#' drops straight into leaflet; the `{s}` subdomains spread tile requests across
#' Amap's four servers for reliability.
#' @noRd
add_basemap <- function(map, tile_source = "amap") {
  if (tile_source == "amap") {
    leaflet::addTiles(
      map,
      urlTemplate = "https://webrd0{s}.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}",
      attribution = '&copy; <a href="https://amap.com">Amap / Gaode</a>',
      options = leaflet::tileOptions(
        subdomains = c("1", "2", "3", "4"),
        tileSize = 256, minZoom = 3, maxZoom = 18
      )
    )
  } else {
    leaflet::addTiles(map)
  }
}
