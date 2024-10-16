library(sf)
china_province <- sf::st_read("inst/geojson/china.json")
china_city <- sf::st_read("data-raw/city_old.json")

nanhai <- data.frame(
  id = "99",
  name = "南海诸岛",
  geometry = china1$geometry[1]
)

china_city <- rbind(china_city, st_sf(nanhai))

st_write(china_city, "data-raw/city.geojson")
file.copy("data-raw/city.geojson", "inst/geojson/city.json")
unlink("data-raw/city.geojson")
