## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  message = FALSE,
  warning = FALSE
  # dpi = 300,
  # fig.dim = c(2.2, 2.5)
)

library(ggplot2)
library(drhutools)

# Functions preload
theme_set(theme_minimal())
set.seed(313)

## ----install-drhutools, eval=FALSE--------------------------------------------
# # Install the stable version
# install.packages("drhutools")
# 
# # Install drhutools from GitHub
# remotes::install_github("yuedeng/drhutools")

## ----folderTree-set, include=FALSE--------------------------------------------
# if (!require(dir2json)) remotes::install_github("stla/dir2json")
# library(dir2json)
#
# # Producing folder try in chunk `folderTree-output`
# wd_original <- getwd()
# 
# # Create a temporary directory
# temp_dir <- tempfile()
# dir.create(temp_dir)
# setwd(temp_dir)
# 
# folderSystem()
# 
# # Show the dendrogram of the folder structure using dir2tree
# dir2tree(temp_dir) |>
#   cat()
# 
# setwd(wd_original)

## ----folderSystem, eval=FALSE-------------------------------------------------
# library(drhutools)
# 
# folderSystem()

## ----cdplot-------------------------------------------------------------------
data("PlantGrowth")

plot_plant <- cdplot(PlantGrowth, ks_test = TRUE)
plot_plant

## ----gb-----------------------------------------------------------------------
ggplot(mtcars, aes(wt, mpg, color = cyl)) +
  geom_point() +
  scale_color_gb(discrete = FALSE)

ggplot(mpg, aes(y = class, fill = drv)) +
  geom_bar() +
  scale_fill_gb()

## ----polygon-output-----------------------------------------------------------
goodmap(
  toy_poly,
  type = "polygon",
  level = "province"
)

## ----point-input--------------------------------------------------------------
toy_point <- data.frame(
  g_lat = c(
    39.947298,
    39.830932,
    39.159621,
    38.745234,
    34.705527,
    23.090849,
    20.008295,
    31.564526,
    29.153561,
    30.368317,
    27.302689,
    41.850161,
    41.7295,
    49.977569,
    31.220653,
    29.962122,
    29.865772
  ),
  g_lon = c(
    116.322434,
    116.20602,
    117.196032,
    113.58242,
    113.755818,
    108.685362,
    109.715334,
    105.974878,
    112.248827,
    102.811716,
    105.28199,
    123.801936,
    125.962291,
    127.493741,
    121.47536,
    121.349437,
    118.436866
  ),
  value_set = c(8, 4, 4, 4, 8, 6, 6, 5, 2, 4, 4, 9, 5, 8, 4, 1, 3)
)

## ----point-output-------------------------------------------------------------
goodmap(
  toy_point,
  type = "point",
  color_type = "factor",
  point_radius = 7,
  legend_name = "Number",
)

## ----points-animate, eval=FALSE-----------------------------------------------
# toy_point$year <- c(
#     2021,
#     2021,
#     2021,
#     2021,
#     2021,
#     2021,
#     2021,
#     2017,
#     2017,
#     2017,
#     2017,
#     1997,
#     1997,
#     1997,
#     1997,
#     1997,
#     1997
#   )
# 
# goodmap(
#   toy_point,
#   type = "point",
#   color_type = "factor",
#   animate = TRUE,
#   animate_var = "year"
# )

## ----traits-------------------------------------------------------------------
column_names <- c(
  "Q3|R3", "Q3|R4", "Q4|R3", "Q4|R4", "Q5|R5", "Q5|R6", "Q6|R3", "Q6|R4", "Q7|R3",
  "Q7|R4", "Q8|R5", "Q8|R6", "Q9|R5", "Q9|R6", "Q10|R5", "Q10|R6", "Q11|R5", "Q11|R6", "Q12|R3",
  "Q12|R4", "Q13|R3", "Q13|R4", "Q14|1", "Q15|1", "Q16|1", "Q17|1", "Q18|1", "Q19|1", "Q20|1",
  "Q21|1", "Q22|1", "Q23|1", "Q24|1", "Q25|1"
)

toy_data <- data.frame(matrix(sample(1:5, 10 * length(column_names), replace = TRUE),
  ncol = length(column_names)
))

names(toy_data) <- column_names

traits(toy_data)

