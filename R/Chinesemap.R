#' @name process_data
#' @title Process data to calculate average Mandarin speaking and listening values by province.
#' 
#' @import leaflet
#' @import leafletCN
#' @import htmlwidgets
#' @import webshot2
#' @import regioncode
#' @import dplyr
#' @import purrr
#' @import here
#' 
#' @param data_file A string, the path to the CSV file containing the data. The example file contain the following columns: `prov`, `year`, `mandarinSpeak`, `mandarinListen`.
#' @param filter_year The year to filter the data
#' @param prov_var The column name for the province variable
#' @param mandarinSpeak_var The column name for the Mandarin speaking variable
#' @param mandarinListen_var The column name for the Mandarin listening variable
#' 
#' @example
#' process_data(cgss1021, 2010, "prov", "mandarinSpeak", "mandarinListen")
#' 
#' @return A data frame with the average Mandarin speaking and listening values by province
#' 
process_data <- function(datafile, filter_year, prov_var, mandarinSpeak_var, mandarinListen_var){
  plot_prov <- data.frame(
    name = regionNames("china")
  )
  
  prov_sym <- ensym(prov_var)
  mandarinSpeak_sym <- ensym(mandarinSpeak_var)
  mandarinListen_sym <- ensym(mandarinListen_var)
  
  plot_prov <- datafile |>
    filter(year == filter_year) |>
    select(!!prov_sym, !!mandarinSpeak_sym, !!mandarinListen_sym) |>
    group_by(!!prov_sym) |>
    summarise(
      value_speak = mean(!!mandarinSpeak_sym, na.rm = TRUE),
      value_listen = mean(!!mandarinListen_sym, na.rm = TRUE)
    ) |>
    ungroup() |>
    right_join(plot_prov, by = c("prov" = "name"))
  
  return(plot_prov)
}

#' @name process_data_select_rename
#' @title Select and rename columns in a data frame.
#' 
#' @param df The data frame to process
#' @param col1 The first column to select
#' @param col2 The second column to select and rename
#' 
#' @example 
#' process_data_select_rename(plot_prov, "prov", "value_speak")
#' 
#' @return A data frame with the selected columns and renamed second column
 
process_data_select_rename <- function(df, col1, col2){
  col1_sym <- ensym(col1)
  col2_sym <- ensym(col2)
  
  result <- df |>
    select(!!col1_sym, !!col2_sym) |>
    rename(value = !!col2_sym) |>
    as.data.frame()
  
  return(result)
}

#' @name geojsonMap_func
#' @title Generates a GeoJSON map using the specified data and map settings.
#' 
#' @param data The data to map
#' @param mapName The name of the map
#' @param palette The color palette to use
#' @param colorMethod The method to use for coloring
#' @param legendTitle The title for the legend
#' 
#' @example 
#' geojsonMap_func(data = "plot_prov_speak)", mapName = "china", palette = gb_pal(palette = "main", reverse = TRUE)(2), colorMethod = "bin", legendTitle = "Speaking")
geojsonMap_func <- function(data, mapName, palette, colorMethod, legendTitle) {
  geojsonMap(data,
             mapName = mapName,
             palette = palette,
             colorMethod = colorMethod,
             legendTitle = legendTitle)
}

#' @name ls_map_func
#' @title Generates a list of maps for speaking and listening, respectively.
#' 
#' @param plot_prov_speak The data for Mandarin speaking
#' @param plot_prov_listen The data for Mandarin listening
#' 
#' @example 
#' ls_map_func(plot_prov_speak, plot_prov_listen)
#' @return A list of maps for speaking and listening
ls_map_func <- function(plot_prov_speak, plot_prov_listen) {
  list(speak = plot_prov_speak, listen = plot_prov_listen) |>
    map2(c("Speaking", "Listening"), \(data, name) {
      geojsonMap(data,
                 mapName = "china",
                 palette = gb_pal(palette = "main", reverse = TRUE)(2),
                 colorMethod = "numeric",
                 legendTitle = name,
                 domain = range(data$value, na.rm = TRUE))
    })
}

#' @name save_maps_as_png
#' @title Save maps as PNG files in the specified directory.
#' 
#' @param map_list The list of maps to save
#' @param suffixes The suffixes for the file names
#' @param output_dir The directory to save the files in (default: "output")
#' 
#' @example
#' save_maps_as_png(ls_map, c("speak", "listen"), output_dir = "outputs")
#' @return Generates and saves the PNG files with geographic data on Chinese map
save_maps_as_png <- function(map_list, suffixes, output_dir = "output") {
  walk2(map_list, suffixes, \(plot, suffix) {
    html_plot <- tempfile(fileext = ".html")
    saveWidget(plot, html_plot, selfcontained = FALSE)
    name_file <- paste0("map_", suffix, ".png")
    webshot(
      html_plot,
      vheight = 900,
      expand = c(-170, 0, 0, -100),
      file = file.path(output_dir, name_file)
    )
  })
}
