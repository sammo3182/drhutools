#' Toy Dataset for `goodmap`
#'
#' A sample dataset designed to illustrate the functionality of the `goodmap` function.
#'
#' @docType data
#' @name toy_poly
#' @format A geographic data frame with 6 rows and 5 variables:
#' \describe{
#'   \item{id}{Optional data identifier.}
#'   \item{city}{Prefectural names, required for city-level map plotting.}
#'   \item{prov}{Provincial names, required for provincial-level map plotting.}
#'   \item{animate_set}{A numeric vector used to group data for animated plotting.}
#'   \item{value_set}{Values used to fill each polygon in the map.}
#' }
"toy_poly"

#' Leaflet Map Names Dataset
#'
#' A internal dataset providing city names and corresponding file names for mapping with Leaflet.
#'
#' @docType data
#' @name leafletcn.map.names
#' @format A data frame with city names and file names.
"leafletcn.map.names"
