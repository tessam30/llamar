#' Modifies a data frame to determine color to overlay on a colored background
#' 
#' Takes a data frame with a value to map to a fill colour and determines whether 
#' light or dark text should be used as the label on top of the fill. For use with 
#' \code{ggplot2::scale_colour_identity()} downstream.
#' 
#' @import dplyr
#' 
#' @param df data frame containing the data
#' @param bckgrnd_column string containing the name of the column to map to fill values
#' @param colour_palette colour palette specification (list of hex values). Can use \code{RColorBrewer::brewer.pal} to generate
#' @param limits (optional) limits for the fill color palette mapping
#' @param sat_threshold (optional) breakpoint between the light and dark text color. 50 percent saturation, by default
#' @param dark_colour (optional) dark color to overlay on low fill values
#' @param light_colour (optional) light color to overlay on high fill values
#' 
#' @examples {
#' # Define a Color Brewer palette
#' library(RColorBrewer)
#' # Generate random data
#' df = data.frame(x = 1:9, y = 1:9)
#' pal = 'Reds'
#' 
#' limits = c(0,15)
#' df = map_colour_text(df, 'x',  brewer.pal(9, pal), limits)
#' 
#' library(ggplot2)
#' ggplot(df, aes(x = x, y = y, fill = x, colour = text_colour, label = round(hsv.s,2))) +
#' geom_point(size = 10, shape = 21) + 
#' geom_text() +
#' scale_fill_gradientn(colours = brewer.pal(9, pal), limits = limits) +
#' scale_colour_identity() +
#' theme_blank()
#' }
#' 
#' @seealso \code{\link{scale_colour_text}}
#' 
#' @export

map_colour_text = function(df,
                           bckgrnd_column,
                           colour_palette,
                           limits = c(min(df[[bckgrnd_column]]), max(df[[bckgrnd_column]])),
                           sat_threshold = 0.5,
                           dark_colour = grey90K,
                           light_colour = 'white') {
  
  # -- Create a color palette --
  # Returns RGB values
  ramp = colorRamp(colour_palette)
  
  # -- convert background values to colors --
  # Adjust to between 0 and 1
  df = df %>% 
    mutate_(.dots = setNames(paste0('(', bckgrnd_column, '-', limits[1],')/(',limits[2], '-', limits[1], ')'), 'bckgrnd')) %>% 
    mutate(bckgrnd = ifelse(is.na(bckgrnd), 0, 
                            ifelse(bckgrnd < 0, 0,
                                   ifelse(bckgrnd > 1, 1, bckgrnd))))
  
  # Check if any values are NA; replace w/ 0
  
  
  mapped_colours = ramp(df$bckgrnd)
  
  # convert to HSV
  mapped_colours = rgb2hsv(t(mapped_colours))
  
  mapped_colours = data.frame('hsv' = t(mapped_colours))
  
  if(all(round(mapped_colours$hsv.s, 1) == 0)) {
    # greyscale: use values
    df = df %>% 
      bind_cols(mapped_colours) %>% 
      mutate(text_colour = ifelse(hsv.v > sat_threshold, dark_colour, light_colour))
  } else {
    # colors: use saturation  
    # pull out the saturation
    df = df %>% 
      bind_cols(mapped_colours) %>% 
      mutate(text_colour = ifelse(hsv.s < sat_threshold, dark_colour, light_colour))
  }
  
  return(df)
}
