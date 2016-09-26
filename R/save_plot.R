#' Simple wrapper to ggsave
#' 
#' Wrapper to ggsave to auto-save the annoying fiddly arguments I always forget
#' 
#' @import ggplot2 extrafont
#' 
#' @export


save_plot <- function(filename,
                      width = NA, 
                      height = NA, 
                      units = 'in', 
                      scale = 1) {
  # -- load fonts for use by pdf writer --
  extrafont::loadfonts(quiet = TRUE)
  
  # -- save the file --
  ggsave(filename = filename, 
         width = width, height = height, 
         units = units,
         bg = "transparent", 
         scale = scale,
         paper = "special", 
         useDingbats = FALSE, 
         compress = FALSE, 
         dpi = 300)
}