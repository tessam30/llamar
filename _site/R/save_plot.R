#' Simple wrapper to ggsave
#' 
#' Wrapper to ggsave to auto-save the annoying fiddly arguments I always forget. Works only w/ ggplot2 objects.
#' 
#' @import ggplot2 extrafont
#' 
#' @param filename string containing file name
#' @param plot which plot to save; by default, the last one created
#' @param width width of rendered plot
#' @param height height of the rendered plot
#' @param units units of the width/height of rendered plot (typically inches -- 'in')
#' @param scale scalar factor to enlarge/shrink the rendered plot
#' 
#' @examples
#' # create a figure
#' p = ggplot(mtcars, aes(x = cyl, y = mpg)) + geom_point()
#' p + stat_summary(geom = 'point', fun.y = 'mean', colour = 'red', size = 5, shape = 21, fill = NA)
#' 
#' # save figures
#' save_plot('last_plot.pdf', width = 5, height = 5)
#' save_plot('plot_p.pdf', plot = p, width = 5, height = 5)
#' 
#' @export


save_plot <- function(filename,
                      plot = last_plot(),
                      width = NA, 
                      height = NA, 
                      units = 'in', 
                      scale = 1) {
  # -- load fonts for use by pdf writer --
  extrafont::loadfonts(quiet = TRUE)
  
  # -- save the file --
  ggsave(filename = filename, 
         plot,
         width = width, height = height, 
         units = units,
         bg = "transparent", 
         scale = scale,
         paper = "special", 
         useDingbats = FALSE, 
         compress = FALSE, 
         dpi = 300)
}