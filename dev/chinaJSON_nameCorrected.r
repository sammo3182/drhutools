# Prov name transfer

library(rio)

df_geo <- import("E:/data/geojson/china_original.json")

nm_prov <- df_geo$features$properties$name

library(regioncode)

nm_full <- regioncode(nm_prov, year_from = 2020, year_to = 2020, convert_to = "code", incomplete_name = TRUE, province = TRUE) |> 
  regioncode(year_from = 2020, year_to = 2020, convert_to = "name", province = TRUE)

nm_full[1] <- df_geo$features$properties$name[1]

df_geo$features$properties$name <- nm_full

export(df_geo, "E:/data/geojson/china.json")
