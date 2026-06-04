# Coordinate-system conversion among WGS-84, GCJ-02 and BD-09.
#
# Chinese web maps do not use plain WGS-84. Amap/Gaode (and Tencent, and
# Google China) render tiles in GCJ-02 ("Mars coordinates"), while Baidu uses
# BD-09. Plotting WGS-84 points on GCJ-02 tiles leaves them shifted by roughly
# 300-500 m. `goodmap()` uses these helpers to move point data into the tile's
# native system so markers land where they should.
#
# Closed-form algorithm (a.k.a. "eviltransform" / "prcoords"); see
# https://github.com/googollee/eviltransform and https://github.com/hujiulong/gcoord.
# All helpers are vectorised over `lon`/`lat`.
#' @noRd

# Krasovsky 1940 ellipsoid, as used by the GCJ-02 obfuscation.
.gcj_a <- 6378245.0
.gcj_ee <- 0.00669342162296594323
.bd_factor <- pi * 3000 / 180 # BD-09's "x_pi"

# GCJ-02 leaves coordinates outside mainland China untouched.
out_of_china <- function(lon, lat) {
  !(lon > 72.004 & lon < 137.8347 & lat > 0.8293 & lat < 55.8271)
}

.transform_lat <- function(x, y) {
  ret <- -100 + 2 * x + 3 * y + 0.2 * y * y + 0.1 * x * y + 0.2 * sqrt(abs(x))
  ret <- ret + (20 * sin(6 * x * pi) + 20 * sin(2 * x * pi)) * 2 / 3
  ret <- ret + (20 * sin(y * pi) + 40 * sin(y / 3 * pi)) * 2 / 3
  ret + (160 * sin(y / 12 * pi) + 320 * sin(y * pi / 30)) * 2 / 3
}

.transform_lon <- function(x, y) {
  ret <- 300 + x + 2 * y + 0.1 * x * x + 0.1 * x * y + 0.1 * sqrt(abs(x))
  ret <- ret + (20 * sin(6 * x * pi) + 20 * sin(2 * x * pi)) * 2 / 3
  ret <- ret + (20 * sin(x * pi) + 40 * sin(x / 3 * pi)) * 2 / 3
  ret + (150 * sin(x / 12 * pi) + 300 * sin(x / 30 * pi)) * 2 / 3
}

wgs_to_gcj <- function(lon, lat) {
  d_lat <- .transform_lat(lon - 105, lat - 35)
  d_lon <- .transform_lon(lon - 105, lat - 35)
  rad_lat <- lat / 180 * pi
  magic <- 1 - .gcj_ee * sin(rad_lat)^2
  sqrt_magic <- sqrt(magic)
  d_lat <- (d_lat * 180) / ((.gcj_a * (1 - .gcj_ee)) / (magic * sqrt_magic) * pi)
  d_lon <- (d_lon * 180) / (.gcj_a / sqrt_magic * cos(rad_lat) * pi)
  inside <- !out_of_china(lon, lat) # 0/1 mask keeps off-shore points unshifted
  list(lon = lon + d_lon * inside, lat = lat + d_lat * inside)
}

# No exact inverse exists; the one-step subtraction is accurate to ~1 m.
gcj_to_wgs <- function(lon, lat) {
  g <- wgs_to_gcj(lon, lat)
  list(lon = lon * 2 - g$lon, lat = lat * 2 - g$lat)
}

gcj_to_bd <- function(lon, lat) {
  z <- sqrt(lon^2 + lat^2) + 0.00002 * sin(lat * .bd_factor)
  theta <- atan2(lat, lon) + 0.000003 * cos(lon * .bd_factor)
  list(lon = z * cos(theta) + 0.0065, lat = z * sin(theta) + 0.006)
}

bd_to_gcj <- function(lon, lat) {
  x <- lon - 0.0065
  y <- lat - 0.006
  z <- sqrt(x^2 + y^2) - 0.00002 * sin(y * .bd_factor)
  theta <- atan2(y, x) - 0.000003 * cos(x * .bd_factor)
  list(lon = z * cos(theta), lat = z * sin(theta))
}

# Dispatch any from/to pair, routing through GCJ-02 as the hub system.
convert_coord <- function(lon, lat, from, to) {
  systems <- c("WGS-84", "GCJ-02", "BD-09")
  from <- match.arg(from, systems)
  to <- match.arg(to, systems)
  if (from == to) return(list(lon = lon, lat = lat))

  # Step 1: bring input to GCJ-02.
  gcj <- switch(from,
    "WGS-84" = wgs_to_gcj(lon, lat),
    "BD-09" = bd_to_gcj(lon, lat),
    "GCJ-02" = list(lon = lon, lat = lat)
  )
  # Step 2: GCJ-02 to target.
  switch(to,
    "GCJ-02" = gcj,
    "WGS-84" = gcj_to_wgs(gcj$lon, gcj$lat),
    "BD-09" = gcj_to_bd(gcj$lon, gcj$lat)
  )
}
