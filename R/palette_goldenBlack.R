#' @name  gb_cols
#' @title Function to extract colors as hex codes.
#'
#' @param ... A character vector indicating the names of colors. Options includes "gold", "black", "orange", "red", "light green", "green", "light blue", "blue", "light grey", "grey", and "dark grey".
#'
#' @importFrom grDevices colorRampPalette dev.size
#'
#' @return A character of hex codes.
#' 
#' @export

gb_cols <- function(...) {
    cols <- c(...)
    
    if (is.null(cols))
        return(gb_colors)
    
    gb_colors[cols]
}

gb_colors <- c(
    `gold`     = "#FFCD00",
    `orange`     = "#FF8200",
    `red`        = "#BD472A",
    `light green` = "#00AF66",
    `green`      = "#00664F",
    `light blue` = "#00A9E0",
    `blue`       = "#00558C",
    `light grey` = "#BBBCBC",
    `dark grey`  = "#63666A",
    `black`      = "#000000"
)

#' @name gb_pal
#' @title Function to interpolate a gold-black color palette.
#'
#' @param palette A character vector indicating the name of palette in gb_palettes. Available palettes:
#' \itemize{
#'   \item \code{main}: Gold and black colors.
#'   \item \code{tricol}: Gold, black, and dark grey to create a gradual effect.
#'   \item \code{digitMixed}: Five-pack colors specified for digital publications.
#'   \item \code{printMixed}: Five-pack colors specified for printed publications.
#'   \item \code{full}: A palette including all the colors \code{gb_cols} can call.
#' }
#' 
#' @param reverse A logic vector indicating whether the palette should be reversed; the default is FALSE.
#' @param ... Additional arguments to pass to \code{colorRampPalette()}
#' 
#' @return A function that takes an integer argument (the required number of colors) and returns a character vector of colors interpolating the given sequence.
#' 
#' @export

gb_pal <- function(palette = "main",
                      reverse = FALSE,
                      ...) {
    pal <- gb_palettes[[palette]]
    
    if (reverse)
        pal <- rev(pal)
    
    colorRampPalette(pal, ...)
}


gb_palettes <- list(
    `tricol`  = gb_cols("gold", "dark grey", "black"),
    `main`  = gb_cols("gold", "black"),
    `digitMixed`   = gb_cols("gold", "red", "green", "blue", "dark grey", "black"),
    `printMixed` = gb_cols(
        "gold",
        "orange",
        "light green",
        "light blue",
        "light grey",
        "black"
    ),
    
    `full`  = gb_cols(
        "gold",
        "orange",
        "red",
        "light green",
        "green",
        "light blue",
        "blue",
        "light grey",
        "dark grey",
        "black"
    )
)

#' @name scale_color_gb
#' @title Fill scale constructors for gold & black colors
#' 
#' @param palette A character vector indicating the name of palette in \code{gb_palettes}. Available palettes:
#' \itemize{
#'   \item \code{main}: Gold and black colors.
#'   \item \code{tricol}: Gold, black, and dark grey to create a gradual effect.
#'   \item \code{digitMixed}: Five-pack colors specified for digital publications.
#'   \item \code{printMixed}: Five-pack colors specified for printed publications.
#'   \item \code{full}: A palette including all the colors \code{gb_cols} can call.
#' }
#' @param discrete A logic vector indicating whether color aesthetic is discrete or not; the default is "main".
#' @param reverse A logic vector indicating whether the palette should be reversed.
#' @param ... Additional arguments passed to \code{discrete_scale()} or \code{scale_color_gradientn()}, used respectively when discrete is TRUE or FALSE
#'
#' @export

scale_color_gb <-
    function(palette = "main",
             discrete = TRUE,
             reverse = FALSE,
             ...) {
        pal <- gb_pal(palette = palette, reverse = reverse)
        
        if (discrete) {
            discrete_scale("colour", paste0("gb_", palette), palette = pal, ...)
        } else {
            scale_color_gradientn(colours = pal(256), ...)
        }
    }

#' @name scale_fill_gb 
#' @title Color scale constructors for gold & black colors
#' 
#' @param palette A character vector indicating the name of palette in \code{gb_palettes}. Available palettes:
#' \itemize{
#'   \item \code{main}: Gold and black colors.
#'   \item \code{tricol}: Gold, black, and dark grey to create a gradual effect.
#'   \item \code{digitMixed}: Five-pack colors specified for digital publications.
#'   \item \code{printMixed}: Five-pack colors specified for printed publications.
#'   \item \code{full}: A palette including all the colors \code{gb_cols} can call.
#' }
#' @param discrete A logic vector indicating whether color aesthetic is discrete or not; the default is "main".
#' @param reverse A logic vector indicating whether the palette should be reversed.
#' @param ... Additional arguments passed to \code{discrete_scale()} or \code{scale_color_gradientn()}, used respectively when discrete is TRUE or FALSE
#'
#' @export

scale_fill_gb <-
    function(palette = "main",
             discrete = TRUE,
             reverse = FALSE,
             ...) {
        pal <- gb_pal(palette = palette, reverse = reverse)
        
        if (discrete) {
            discrete_scale("fill", paste0("gb_", palette), palette = pal, ...)
        } else {
            scale_fill_gradientn(colours = pal(256), ...)
        }
    }

