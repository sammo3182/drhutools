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
#' @return A data.frame object
"toy_poly"

#' Leaflet Map Names Dataset
#'
#' A internal dataset providing city names and corresponding file names for mapping with Leaflet.
#'
#' @docType data
#' @name leafletcn.map.names
#' @format A data frame with city names and file names.
#' @return A data.frame object
"leafletcn.map.names"

#' Determinants of the Arab Spring Uprising
#'
#' A dataset containing fuzzy-set membership scores for ten sets of state
#' characteristics across 20 states in northern Africa and the Arab Peninsula
#' at the time of the Arab Spring uprisings of 2010--12.
#'
#' @docType data
#' @name Arab.Spring
#' @format A data frame with 20 rows and 10 variables, measured as fuzzy-set
#'   membership scores:
#' \describe{
#'   \item{Gdppc}{Per-capita GDP.}
#'   \item{Gini}{Economic inequality.}
#'   \item{Unemp}{Unemployment.}
#'   \item{Urban}{Degree of urbanization.}
#'   \item{Youth}{Size of youth bulge.}
#'   \item{Mobile}{Mobile phone usage.}
#'   \item{Internet}{Internet penetration.}
#'   \item{Fuel}{Fuel-dependence of economy.}
#'   \item{Pol}{Regime fragility.}
#'   \item{Success}{Social movement success.}
#' }
#' @source Hussain, Muzammil M., and Philip N. Howard. 2013. "What Best Explains
#'   Successful Protest Cascades? ICTs and the Fuzzy Causes of the Arab Spring."
#'   *International Studies Review* 15(1): 48--66.
#' @return A data.frame object
"Arab.Spring"

#' Determinants of Social Revolutions
#'
#' A data frame containing hypothetical fuzzy-set membership scores for
#' three sets of state characteristics across twenty cases.
#'
#' @docType data
#' @name social.revolutions
#' @format A data frame with 20 rows and 3 variables, measured as degree of
#'   fuzzy-set membership:
#' \describe{
#'   \item{soc.rev}{Presence of social revolution.}
#'   \item{breakdown}{Degree of state breakdown.}
#'   \item{pop.ins}{Presence of popular insurrection.}
#' }
#' @source Ragin, Charles C. 2000. *Fuzzy-Set Social Science*. Chicago:
#'   University of Chicago Press, p. 220.
#' @return A data.frame object
"social.revolutions"
