luis_colours <- c('zero' = "#2644A7",
                  'one' = "#AF155C", 
                  'two' = "#208486",
                  'three' = "#B656BD",
                  'four' = "#B39421",
                  'five' = "#388AE2",
                  'six' = "#E04040",
                  'seven' = "#33A033",
                  'eight' = "#672DC4",
                  'nine' = "#AA5F18",
                  'fucsia' = "#CC2299", 
                  'yellow' = "#EECC55",
                  'brown' = "#EECC99",
                  'blue' = "#0000FF",
                  'blue1' = "#11448B",
                  'red' = "#EE0000",
                  'red1' = "#881111",
                  'orange' = "#FF7700",
                  'grey' = "#CCCCCC",
                  'green' = "#668822")


#' Function to extract  colors as hex codes
#'
#' @param ... Character names of colors
#'
luis_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols)) {
    return(luis_colours)
  }
  
  luis_colours[cols]
}


luis_palettes <- list('eurostat' = luis_cols("zero","one", "two", "three", "four", "five",
                                             "six", "seven", "eight", "nine"),
                      'heat' = luis_cols("red","grey","blue"),
                      'luis' = luis_cols("blue1", "red1", "orange", "green", "grey")
)

#' Return function to interpolate a  color palette
#'
#' @param palette Character name of palette in luis_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
luis_pal<- function (palette= "eurostat",
                     reverse = FALSE, ...) {
  pal <- luis_palettes[[palette]]
  if (reverse) pal <- rev (pal)
  colorRampPalette(pal, ...)
}


#' Color scale constructor for luis colors
#'
#' @param palette Character name of palette in luis_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_colour_luis <- function(palette = "eurostat", discrete = TRUE, reverse = FALSE, ...) {
  pal <- luis_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("luis_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for luis colors
#'
#' @param palette Character name of palette in luis_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_luis <- function(palette = "eurostat", discrete = TRUE, reverse = FALSE, ...) {
  pal <- luis_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("luis_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}