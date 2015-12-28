#' Theme with x and y axis labels
#' 
#' This function can be tacked onto a ggplot object to specify a new theme with the x and y axis labeled.
#' @export
#' @examples
#' ggplot(mtcars, aes(x = mpg, y = cyl)) + geom_point() + theme_xylab()

theme_xylab<- function() {
  grey60K = 'grey'
  grey90K = 'grey'
  
  theme_bw() +
    theme(
      text = element_text(family = 'Segoe UI Light', colour = grey60K),
      rect = element_blank(),
      plot.background = element_blank(),
      axis.text = element_text(size = 12,  color = grey60K),
      title =  element_text(size = 15, family = "Segoe UI", hjust = 0, color = grey90K),
      axis.title =  element_text(size = 14, family = "Segoe UI Semilight", color = grey60K, hjust = 0.5, vjust = -0.25),
      strip.text = element_text(size=14, family = "Segoe UI Semilight", hjust = 0.05, vjust = -2.5, color = grey90K),
      legend.position = 'none',
      strip.background = element_blank(),
      axis.ticks = element_blank(),
      panel.margin = unit(1, 'lines'),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank())
}
