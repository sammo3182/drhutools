# Geometry helpers adapted from Yang Cao's (`yiluheihei`) fork of `leafletCN`
# (https://github.com/yiluheihei/leafletCN). Only the pieces `goodmap()` needs
# to download and parse a GeoJSON boundary are kept here.
#'
#' @importFrom jsonlite fromJSON
#' @import sf
#' @import sp
#' @importFrom methods as
#' @importFrom utils download.file
#' @noRd

globalVariables("leafletcn.map.names")

# Parse a GeoJSON file into a SpatialPolygonsDataFrame.
read.geoShape <- function(txt) {
  raw <- jsonlite::fromJSON(txt)

  datPart <- raw$features$properties
  if (is.null(datPart)) {
    stop("Unfortunately, no ", basename(txt), " in leafletCN now\n")
  }

  # remove the redundant data
  real_indx <- !sapply(datPart$name, function(x) x == "" || is.null(x) || is.na(x))
  datPart <- datPart[real_indx, ]
  ployList <- lapply(
    raw$features$geometry$coordinates[real_indx],
    function(x) {
      if (is.array(x)) {
        a <- as.vector(x)
        dim(a) <- c(length(a) / 2, 2)
        Sr <- sp::Polygon(a)
        sp::Polygons(list(Sr), "namei")
      } else {
        if (any(sapply(x, class) == "list")) {
          whilei <- 0
          while (any(sapply(x, class) == "list")) {
            whilei <- whilei + 1
            if (whilei == 10) {
              break
            }
            index <- which(sapply(x, class) == "list")[1]
            x <- append(x[-index], x[[index]])
          }
        }
        Sr <- lapply(x, function(y) {
          a <- as.vector(y)
          dim(a) <- c(length(a) / 2, 2)
          sp::Polygon(a)
        })
        sp::Polygons(Sr, "namei")
      }
    }
  )
  for (i in seq_along(ployList)) {
    ployList[[i]]@ID <- as.character(i)
  }
  ployPart <- sp::SpatialPolygons(ployList, seq_along(ployList))
  datPart <- raw$features$properties
  if (any(sapply(datPart, class) == "list")) {
    index <- which(sapply(datPart, class) == "list")
    outlist <- lapply(index, function(x) {
      # fix for missing data
      fix <- lapply(datPart[, x], function(y) {
        if (length(y)) y else NaN
      })
      out <- do.call(rbind, fix)
      colnames(out) <- paste0(names(datPart)[x], seq_len(ncol(out)))
      out
    })
    datPart <- cbind(datPart, do.call(cbind, outlist))
    datPart <- datPart[, -index]
  }

  rownames(datPart) <- row.names(ployPart)
  sp::SpatialPolygonsDataFrame(ployPart, datPart)
}

# Round-trip names through UTF-8 to normalise encoding on Windows.
encodingSolution <- function(str) {
  iconv(str, "UTF-8", "UTF-8")
}

# Download (and cache in tempdir) a named GeoJSON boundary, returning a
# validity-fixed Spatial object. Valid names come from `leafletcn.map.names`.
readGeoLocal <- function(city) {
  labels <- c(
    leafletcn.map.names$name,
    leafletcn.map.names$label,
    leafletcn.map.names$name_en
  )
  if (!city %in% labels) {
    stop(
      "\n", city, ": this mapType cannot be found!\n",
      "Valid map names are listed in the `leafletcn.map.names` dataset.\n",
      "Encoding convert: ?iconv"
    )
  }

  index <- leafletcn.map.names$name == city |
    leafletcn.map.names$label == city |
    leafletcn.map.names$name_en == city
  file_name <- leafletcn.map.names$files[index]

  url <- paste0("https://drhuyue.site:10002/sammo3182/data/geojson/", file_name)
  local_file_path <- file.path(tempdir(), file_name)

  if (!file.exists(local_file_path)) {
    message("Downloading GeoJSON for ", city)
    download.file(url, destfile = local_file_path, mode = "wb")
  }
  if (!file.exists(local_file_path)) {
    stop("Unfortunately, no geojson file for ", city, " in leafletCN now\n")
  }

  output <- read.geoShape(local_file_path)

  # for taiwan
  city_info <- leafletcn.map.names[index, ]
  if (city_info$name_en == "Taiwan") {
    output$name <- city_info$label
  }
  if (.Platform$OS.type == "windows") {
    output$name <- encodingSolution(output$name)
  }

  fix_orphaned_hole(output)
}

# Fix orphaned holes so every polygon has a valid outer edge and inner hole.
# https://cran.r-project.org/web/packages/maptools/vignettes/combine_maptools.pdf
fix_orphaned_hole <- function(x) {
  sf_obj <- sf::st_as_sf(x)
  fixed_sf <- sf::st_make_valid(sf_obj)
  fixed_sp <- methods::as(fixed_sf, "Spatial")

  if (inherits(x, "SpatialPolygonsDataFrame")) {
    fixed_sp <- sp::SpatialPolygonsDataFrame(fixed_sp, x@data)
  }
  fixed_sp
}
