library(magrittr)
library(dplyr)

# download and tar origin geojson file ------------------------------------
download.file(
  "https://github.com/wenccro/chinaMapJsonData/archive/master.zip",
  destfile = "data-raw/map.zip"
)
untar("data-raw/map.zip", exdir = "data-raw/")
map_files <- list.files("data-raw/chinaMapJsonData-master/")

## tar map files: proivnce map files
province_map_files <- setdiff(map_files, c("README.md", "datas.json"))

# # windows platform: use winrar to extract rar files
# unrar_path <- shQuote("C:\\Program Files\\WinRAR\\UnRAR.exe")
# # province_name <- tools::file_path_sans_ext(province_map_files)
# if (!exists("data-raw/map")) dir.create("data-raw/map")
# # sapply(file.path("data-raw/map", province_name),
# #   dir.create)
# cmds <- paste(
#   unrar_path,
#   "x",
#   file.path("data-raw/chinaMapJsonData-master",  province_map_files),
#   "data-raw/map"
# )
# sapply(cmds, shell)

# copy the province and country json file ---------------------------------

# reference china cities file, function get_china_cities is defined in
# china.city.R
china_cities <- get_china_cities()

# 海南藏族自治州和海南label一样，吉林省和吉林市的一样，需要手动修改其label
# 和files

# index <- china_cities$City == "海南"
# china_cities$City[index] <- china_cities$City_Admaster[index]


province <- unique(china_cities$Province)
province_en <- unique(china_cities$Province_EN)
province_name_full <- readr::read_csv("data-raw/province_full_name.csv") %>%
  .$province_name_full

# province index corresponding to province dir
province_index <- sapply(province, function(x) which(grepl(x, province_name_full)))
province_name_full <- province_name_full[province_index]

province_from_json <- file.path(
  "data-raw/map",
  province_name_full,
  "datas.json"
)
province_to_json <- file.path(
  "inst/geojson",
  paste0(province_en, ".json")
)
file.copy(province_from_json, province_to_json, overwrite = TRUE)

# china json
# file.copy("data-raw/chinaMapJsonData-master/datas.json",
#           "data-raw/map/china.json")
# for test, should be used as internal data
file.copy(province_to_json, "inst/geojson/")

# city json ---------------------------------------------------------------

## mac os
path <- file.path("data-raw/chinaMapJsonData-master",  province_map_files)
lapply(path,function(x)system(paste("unrar x", x, "data-raw/map/")))
provinces <- tools::file_path_sans_ext(province_map_files)
# remove zhixiashi and gangaotai
provinces <- setdiff(
  provinces,
  c("澳门特别行政区", "香港特别行政区", "台湾省",
    "北京市", "天津市", "上海市", "重庆市"
  )
)

city_copied <- lapply(provinces, copy_provicne_json)
city_missed <- purrr::map_lgl(city_copied, ~ !is.null(.x)) %>%
  city_copied[.] %>%
  dplyr::bind_rows()
city_missed$city_short <- c(
  "临夏", "东沙", "白沙", "保亭", "昌江", "澄迈", "定安", "东方",
  "乐东", "临高", "陵水", "琼海", "琼中", "三沙", "屯昌", "万宁",
  "文昌", "五指山", "济源", "潜江", "神农架", "天门", "仙桃", "襄阳",
  "淮安", "阿拉尔", "北屯", "哈密", "可克达拉", "昆玉", "石河子",
  "双河", "铁门关", "图木舒克","五家渠"
)

city_missed$province <- substr(city_missed$province, 1, 2)
city_missed <- dplyr::transmute(
  city_missed,
  City = city_short,
  City_Admaster = city,
  Province = province
)

# generate city en name using pinyin
mydic <- pydic(method = 'toneless', dic = "pinyin2")
city_missed$City_EN <- pinyin::py(city_missed$City, dic = mypy, sep = "") %>%
  conv_firstletter()

index <- match(city_missed$Province, china_cities$Province)
city_missed <- mutate(
  city_missed,
  Province_EN = china_cities$Province_EN[index],
  Region = china_cities$Region[index],
  Tier = china_cities$Tier[index]
) %>%
  select(one_of(names(china_cities)))

china_cities_full <- bind_rows(china_cities, city_missed)
readr::write_csv(china_cities_full, "data-raw/china_city_list_full.csv", )

# manulally fix Sichuan.json on https://mapshaper.org/
# it will raise an error while visulazie Sichuan.json using geojsonMap
# Error in createPolygonsComment(p) : rgeos_PolyCreateComment: orphaned hole, cannot find containing polygon for hole at index 3.
#
# Thus, the data is preprocessed on https://mapshaper.org/: load data, remove two lines, and export geojson data to replance the original Sichuan.json

# reference city to json file ---------------------------------------------
province_names <- data.frame(
  name = c(province_name_full, "中国", "南海诸岛", "世界", "city"),
  name_en = c(province_en, "china", "Nanhai", "world", "city"),
  label = c(province, "中国", "南海", "世界", "city"),
  files = c(
    paste0(province_en, ".json"),
    "china.json",
    NaN, # nanhai
    "countries.josn",
    "city.json"
  ),
  stringsAsFactors = FALSE
)
city_name <- unlist(city_copied)
index <- match(city_name, china_cities$City_EN)
city_names <- data.frame(
  name = china_cities$City_Admaster[index],
  name_en = china_cities$City_EN[index],
  label = china_cities$City[index],
  files = paste0(city_name, ".json"),
  stringsAsFactors = FALSE
)
leafletcn.map.names <- bind_rows(province_names, city_names)

usethis::use_data(leafletcn.map.names, overwrite = TRUE)

# Function ----------------------------------------------------------------

get_china_cities <- function() {
  china_cities <- readr::read_csv(
    "https://github.com/pzhaonet/ncovr/raw/master/inst/china_city_list.csv"
  )
  # data correction
  china_cities <- dplyr::mutate(
    china_cities,
    Province_EN = dplyr::case_when(
      Province_EN == "anhui" ~ "Anhui",
      Province_EN == "guizhou" ~ "Guizhou",
      Province_EN == "hubei" ~ "Hubei",
      Province_EN == "xinjiang" ~ "Xinjiang",
      TRUE ~ Province_EN
    ))
  china_cities
}

conv_firstletter <- function(x){
  paste(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)), sep = "")
}


#' copy city jsonfile and find missing cities in china_cities
copy_provicne_json <- function(province) {
  path <- file.path("data-raw/map", province)
  city_jsonfile <- setdiff(
    list.files(path),
    "datas.json"
  )
  china_cities <- readr::read_csv("data-raw/china_city_list_full.csv")
  city_index <- match(city_jsonfile, china_cities$City_Admaster)
  if (any(is.na(city_index))) {
    warning("City info inconsistent")
    city_missed <- data.frame(
      province = province,
      city = city_jsonfile[is.na(city_index)],
      stringsAsFactors = FALSE
    )

    return(city_missed)
  }
  city_en <- china_cities$City_EN[city_index]
  city_from_json <- file.path(
    "data-raw/map",
    province,
    city_jsonfile,
    "datas.json"
  )
  city_to_json <- file.path(
    "inst/geojson",
    paste0(city_en, '.json')
  )

  file.copy(city_from_json, city_to_json, overwrite = TRUE)

  # For checking missing cities
  # return(NULL)

  return(city_en)
}
