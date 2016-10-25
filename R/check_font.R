#' Map text annotation color to underlying value
#' 
#' @description
#' Find a corresponding grey color to overlay on top of a colored object within ggplot.
#' Should be used in conjuction with ggplot2 objects; geom_text object should specify colour/color aes argument 
#' By default, color values are linearly interpolated from the low value of the range to 50% of the highest value in the data; 
#' data larger than that are all given the same color (`light_color`) 
#'
#' @param data_col data containing the data frame to map the colors to
#' 
#' @import extrafont   


check_font = function(font_name){
  installed_fonts = extrafont::fonts()
  
  if (font_name %in% installed_fonts) {
    return(TRUE) 
  } else {
    return(FALSE)
  }
}