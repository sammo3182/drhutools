# Functions import from Yang Cao (`yiluheihei`)'s version of [`leafletCN`](https://github.com/yiluheihei/leafletCN/tree/master) 

#'
#' @importFrom htmltools htmlEscape 
#' @importFrom htmlwidgets onRender
#' @importFrom jsonlite fromJSON
#' @importFrom maptools checkPolygonsHoles
#' @import leaflet
#' @import sp
#' @importFrom stats terms.formula
#' @importFrom methods slot
#' @noRd

# Add title to the leaflet

globalVariables(c("leafletcn.map.names", ".triList"))

addTitle <- function(object,
                    text,
                    color = "black",
                    fontSize = "20px",
                    fontFamily = "Sans",
                    leftPosition = 50,
                    topPosition = 2){
  
  htmlwidgets::onRender(object, paste0("
                                       function(el,x){
                                       h1 = document.createElement('h1');
                                       h1.innerHTML = '", text ,"';
                                       h1.id='titleh1';
                                       h1.style.color = '", color ,"';
                                       h1.style.fontSize = '",fontSize,"';
                                       h1.style.fontFamily='",fontFamily,"';
                                       h1.style.position = 'fixed';
                                       h1.style['-webkit-transform']='translateX(-50%)';
                                       h1.style.left='",leftPosition ,"%';
                                       h1.style.top='",topPosition,"%';
                                       document.body.appendChild(h1);
                                       }"))
}

# Load amap to leaflet

amap <- function(map, attribution = '&copy; <a href="http://amap.com">amap.com</a >', ...) {
  leaflet::addTiles(
    map,
    'http://webrd02.is.autonavi.com/appmaptile?lang=zh_cn&size=1&scale=1&style=8&x={x}&y={y}&z={z}',
    leaflet::tileOptions(
      tileSize = 256,
      minZoom = 3,
      maxZoom = 17
    ),
    attribution = attribution,
    ...
  )
}

# Show the basic shape of the data

demomap <- function(mapName){
  # if(.Platform$OS.type == "windows"){
  #   locate = Sys.getlocale("LC_CTYPE")
  #   Sys.setlocale("LC_CTYPE","eng")
  # }
  
  countries <- readGeoLocal(mapName)
  countries$popup = countries$name
  # countries$color = rainbow(length(countries$name))
  ## Encoding
  # Sys.setlocale("LC_CTYPE","eng")
  # if(.Platform$OS.type == "windows"){
  #   countries$popup = encodingSolution(countries$popup)
  # }
  
  map <- leaflet::leaflet(countries)
  output = map %>% leaflet::addTiles() %>%
    leaflet::addPolygons(stroke = T,
                         smoothFactor = 0.2,
                         fillOpacity = 0.2,
                         # fillColor = ~color,
                         weight = 1,
                         popup = ~htmltools::htmlEscape(popup))
  #
  #   if(.Platform$OS.type == "windows"){
  #     Sys.setlocale("LC_CTYPE",locate)
  #   }
  
  return(output)
}

#Load amap to leaflet

geojsonMap <- function(dat,
                     mapName,
                     namevar=NULL,
                     valuevar=NULL,
                     palette = "Blues",
                     colorMethod = "numeric",
                     na.color = "#808080",
                     popup = NULL,
                     stroke = T,
                     smoothFactor = 1,
                     weight = 1,
                     fillOpacity = 0.7,
                     legendTitle = "Legend",
                     tileType = NULL,
                     ...){
  if(!is.data.frame(dat)){
    stop("dat should be a data.frame")
  }
  if(is.null(namevar)){
    name = dat[, 1]
  }else{
    name = evalFormula(namevar,dat)
  }
  name = as.character(name)
  
  if(is.null(valuevar)){
    value = dat[, 2]
  }else{
    value = evalFormula(valuevar,dat)
  }
  
  
  countries <- readGeoLocal(mapName)
  # countries$label = toLabel(countries$name)
  countries$label = countries$name
  index = sapply(countries$label,function(x) which(name==x)[1])
  
  if(is.null(popup)){
    countries$popup = countries$name
  }else if(length(popup)!=dim(dat)[1]){
    warning("Length of popup and data don't match, use names instead!")
    countries$popup = countries$name
  }else{
    countries$popup = popup[index]
  }
  
  countries$value = value[index]
  
  ##
  if(colorMethod == "numeric"){
    pal <- leaflet::colorNumeric(
      palette = palette,
      domain = countries$value,
      na.color = na.color,
      ...
    )
  }else if( colorMethod == "bin" ){
    pal <- leaflet::colorBin(
      palette = palette,
      domain = countries$value,
      na.color = na.color,
      ...
    )
  }else if(colorMethod == "quantile"){
    pal <- leaflet::colorQuantile(
      palette = palette,
      domain = countries$value,
      na.color = na.color,
      ...
    )
  }else if(colorMethod == "factor"){
    pal <- leaflet::colorFactor(
      palette = palette,
      domain = countries$value,
      na.color = na.color,
      ...
    )
  }else{
    pal <- leaflet::colorNumeric(
      palette = palette,
      domain = countries$value,
      na.color = na.color,
      ...
    )
  }
  
  
  map <- leaflet::leaflet(countries)
  if (is.null(tileType)) {
    map %>%
      leaflet::addPolygons(stroke = stroke,
                           smoothFactor = smoothFactor,
                           fillOpacity = fillOpacity,
                           weight = weight,
                           color = ~pal(value),
                           popup = ~htmltools::htmlEscape(popup)
      ) %>%
      leaflet::addLegend("bottomright", pal = pal, values = ~value,
                         title = legendTitle,
                         labFormat = leaflet::labelFormat(prefix = ""),
                         opacity = 1
      )
  } else {
    map %>% tileType %>%
      leaflet::addPolygons(stroke = stroke,
                           smoothFactor = smoothFactor,
                           fillOpacity = fillOpacity,
                           weight = weight,
                           color = ~pal(value),
                           popup = ~htmltools::htmlEscape(popup)
      ) %>%
      leaflet::addLegend("bottomright", pal = pal, values = ~value,
                         title = legendTitle,
                         labFormat = leaflet::labelFormat(prefix = ""),
                         opacity = 1
      )
  }
}

# Create a sp object from a data.frame

leafletGeo <- function(mapName,
                      dat = NULL,
                      namevar = NULL,
                      valuevar = NULL){
  countries <- readGeoLocal(mapName)
  countries$popup = countries$name
  # if(.Platform$OS.type == "windows"){
  #   countries$popup = encodingSolution(countries$popup)
  # }
  
  if(is.null(dat)){
    return(
      countries
    )
  }else{
    if(!is.data.frame(dat)){
      stop("dat should be a data.frame")
    }
    if(is.null(namevar)){
      name = dat[, 1]
    }else{
      name = evalFormula(namevar,dat)
    }
    name = as.character(name)
    
    if(is.null(valuevar)){
      value = dat[, 2]
    }else{
      value = evalFormula(valuevar,dat)
    }
    # countries <- readGeoLocal(mapName)
    countries$label = countries$name
    index = sapply(countries$label,function(x) which(name==x)[1])
    countries$value = value[index]
    # countries$popup = countries$name
    return(
      countries
    )
  }
}

# Read geoshape file into R

read.geoShape <- function(txt) {
  raw = jsonlite::fromJSON(txt)
  
  datPart <- raw$features$properties
  # Taiwan json file: error
  if (is.null(datPart)) {
    stop("Unfortunately, no ", basename(txt), " in leafletCN now\n")
  }
  
  # remove the redundant data
  real_indx <- !sapply(datPart$name, function(x) x == "" || is.null(x) || is.na(x))
  datPart = datPart[real_indx, ]
  ployList = lapply(
    raw$features$geometry$coordinates[real_indx],
    function(x) {
      if (is.array(x)) {
        a = as.vector(x)
        dim(a) = c(length(a)/2, 2)
        Sr = sp::Polygon(a)
        Sp = sp::Polygons(list(Sr), "namei")
        return(Sp)
      }
      else {
        if (any(sapply(x, class) == "list")) {
          whilei = 0
          while (any(sapply(x, class) == "list")) {
            whilei = whilei + 1
            if (whilei == 10)
              break
            index = which(sapply(x, class) == "list")[1]
            x = append(x[-index], x[[index]])
          }
        }
        Sr = lapply(x, function(y) {
          a = as.vector(y)
          dim(a) = c(length(a)/2, 2)
          return(sp::Polygon(a))
        })
        Sp = sp::Polygons(Sr, "namei")
        return(Sp)
      }
    }
  )
  for (i in 1:length(ployList)) {
    ployList[[i]]@ID = as.character(i)
  }
  ployPart = sp::SpatialPolygons(ployList, 1:length(ployList))
  datPart = raw$features$properties
  if (any(sapply(datPart, class) == "list")) {
    index = which(sapply(datPart, class) == "list")
    outlist = lapply(index, function(x) {
      # fix for missing data
      fix <- lapply(datPart[, x], function(y) {
        if (length(y)) {
          return(y)
        } else {
          return(NaN)
        }
      })
      
      out = do.call(rbind, fix)
      colnames(out) = paste0(names(datPart)[x], 1:dim(out)[2])
      return(out)
    })
    datPart = cbind(datPart, do.call(cbind, outlist))
    datPart = datPart[, -index]
  }
  
  rownames(datPart) = row.names(ployPart)
  
  ex_1.7 = sp::SpatialPolygonsDataFrame(ployPart, datPart)
  return(ex_1.7)
}


# Show regions in submaps

regionNames <- function(mapName=NULL){
  # city = 'china'
  if(is.null(mapName)){
    print(leafletcn.map.names$name)
    cat("\nThese are valid mapName~\n")
    return("NULL")
  }
  
  ## read from local files
  countries <- readGeoLocal(mapName)
  
  ## convert Encoding in Windows
  if(.Platform$OS.type == "windows"){
    encodingSolution(countries$name)
  }
  
  countries$name
}


## Utility functions
## Encoding solution
encodingSolution <- function(str){
  iconv(str, "UTF-8", "UTF-8")
}

## read function
readGeoLocal <- function(city){
  # query = toLabel(city)
  labels <-  c(
    leafletcn.map.names$name,
    leafletcn.map.names$label,
    leafletcn.map.names$name_en
  )
  if (!city %in% labels){
    stop(paste0(
      "\n",
      city,
      ": this mapType cannot found!\n",
      "Please check the mapType name or use icnov to convert encoding.\n",
      "Valid mapTypes: regionNames()\n",
      "Encoding convert: ?iconv")
    )
  }
  
  index <- leafletcn.map.names$name == city | leafletcn.map.names$label == city | leafletcn.map.names$name_en == city
  file = paste0("geojson/", leafletcn.map.names$files[index])
  filePath = system.file(file,package = "leafletCN")
  
  # no nanhai json file
  if (length(filePath) == 0) {
    stop("Unfortunately, no geojson file for", city, " in leafletCN now\n")
  }
  
  # output = rgdal::readOGR(filePath, "OGRGeoJSON")
  output = read.geoShape(filePath)
  
  # for taiwan
  city_info <- leafletcn.map.names[index, ]
  if (city_info$name_en == "Taiwan"){
    output$name <- city_info$label
  }
  if (.Platform$OS.type == "windows"){
    output$name = encodingSolution(output$name)
  }
  
  return(fix_orphaned_hole(output))
}

## .triList
## Use first two words to match
toLabel <- function(city){
  labels = sapply(city, function(x){
    if(tolower(substr(x,1,1)) %in% letters){
      return(tolower(x))
    }else if(x == .triList[[5]] | grepl(paste0(.triList[[5]],.triList[[7]][1]), x)){
      warning("Using Jilin Province instead of Jilin City!")
      return(.triList[[5]])
    } else if(grepl(.triList[[5]], x) & !grepl(paste0(.triList[[5]],.triList[[7]][1]), x)){
      return(paste0(.triList[[5]],.triList[[7]][2]))
    }
    else if(x == .triList[[6]] | grepl(paste0(.triList[[6]],.triList[[7]][1]), x)){
      warning("Using Hainan Province instead of Hainan City!")
      return(.triList[[6]])
    }else if(grepl(.triList[[6]], x) & !grepl(paste0(.triList[[6]],.triList[[7]][1]), x)){
      return(paste0(.triList[[6]],.triList[[7]][2]))
    }else if(grepl(.triList[[1]], x)|
             grepl(.triList[[2]], x)|
             grepl(.triList[[3]], x)|
             grepl(.triList[[4]], x)
    ){
      return(substr(x, 1, 3))
    }else{
      return(substr(x, 1, 2))
    }
  })
  return(labels)
}

## Fork from echarts

evalFormula <- function(x, data) {
  # x = ~value; data = mapData
  if (!inherits(x, 'formula')) return(x)
  if (length(x) != 2) stop('The formula must be one-sided: ', deparse(x))
  x_formula = terms.formula(x)
  if (length(attr(x_formula, "term.labels")) == 1){
    eval(x[[2]], data, environment(x))
  }else{
    as.data.frame(sapply(attr(x_formula, "term.labels"),function(tmpTerm){
      return(eval(as.name(tmpTerm), data, environment(x)))
    }),stringsAsFactors=F)
  }
}

# Fix orphaned hole, ensure each polygon having an outer edge and an inner hole
# https://cran.r-project.org/web/packages/maptools/vignettes/combine_maptools.pdf
# https://github.com/MatMatt/MODIS/commit/1b14974063b371a69987e5ee218ee66f132b2d61#diff-786518131335adf2d5c6c59e7f1665a1
# 

#' @importFrom methods slot
#' @noRd

fix_orphaned_hole <- function(x) {
  polys <- slot(x, "polygons")
  fixed <- lapply(polys, maptools::checkPolygonsHoles)
  
  fixed_sp <- sp::SpatialPolygons(
    fixed,
    proj4string = sp::CRS((sp::proj4string(x)))
  )
  
  if (inherits(x, "SpatialPolygonsDataFrame")) {
    fixed_sp <- sp::SpatialPolygonsDataFrame(fixed_sp, x@data)
  }
  
  fixed_sp
}
