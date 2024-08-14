taiwan <- rnaturalearth::ne_countries(country = "taiwan") %>%
  geojson::as.geojson()
taiwan_js <- jsonlite::fromJSON(taiwan)
taiwan_js$features$properties <- NULL
taiwan_js$features$properties <- data.frame(name = "台湾")

write_json(taiwan_js, path = "data-raw/Taiwan.json")
