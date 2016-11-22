#' Customized ggplot themes
#'
#' @name themes
NULL
# > NULL
#' @param font_normal string containing font name for normal text
#' @param font_semi string containing font name for semilight text
#' @param font_light string containing font name for light text
#' @param font_size value, in points, of the font size
#' @param legend.position (optional) legend position; takes either a ggplot legend position string like `'left'` or a tuple in percent of the x- and y-axes, like `c(0.3, 0.7)`
#' @param projector (optional) if TRUE, make lines and text bolder for an LCD projector
#' 
#' @examples
#' ggplot(mtcars, aes(x = wt, y = mpg, colour = cyl)) + geom_point() + facet_wrap(~am) + ggtitle('Heavier cars have worse gas efficiency') + theme_xgrid(legend.position = c(0.8, 0.9), grey_background = T)
#' 

#' @describeIn  themes Theme with axis labels, titles, grid lines, axes, and legend.
#' @export
theme_basic <- function(font_normal = 'Lato',
                        font_semi = 'Lato Light',
                        font_light = 'Lato Light',
                        large_font = 15,
                        medium_font = 14, 
                        small_font = 12) {
  theme_bw() + 
    theme(text = element_text(family = font_light, colour = grey60K), 
          plot.background = element_blank(), 
          panel.border = element_rect(size = 0.2, colour = grey90K, fill = NA), 
          axis.text = element_text(size = small_font, colour = grey60K), 
          title = element_text(size = large_font, family = font_normal, hjust = 0, colour = grey90K), 
          axis.title = element_text(size = medium_font, family = font_semi, colour = grey60K, hjust = 0.5, vjust = -0.25), 
          axis.ticks = element_blank(), 
          panel.spacing = unit(3, "lines"), 
          panel.grid.major.y = element_line(size = 0.2, colour = grey30K), 
          panel.grid.minor.y = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.x = element_line(size = 0.1, colour = grey30K), 
          strip.text = element_text(size = 13, colour = grey50K, family = font_semi), 
          legend.position = c(0.85, 0.85), 
          legend.title = element_text(size = 13), 
          legend.text = element_text(size = 11), 
          legend.background = element_blank(), 
          strip.background = element_blank())
}

#' @describeIn  themes Theme with x and y labels, titles, and gridlines
#' @export
theme_xygrid <- function(font_normal = 'Lato',
                         font_semi = 'Lato Light',
                         font_light = 'Lato Light') {
  theme_bw() + 
    theme(text = element_text(family = font_light, colour = grey80K), 
          plot.title = element_text(hjust = 0), rect = element_blank(), 
          plot.background = element_blank(), 
          axis.text = element_text(size = 12, colour = grey80K), 
          title = element_text(size = 15, family = font_normal, hjust = 0, colour = grey90K), 
          axis.title = element_text(size = 14, family = font_semi, colour = grey80K, hjust = 0.5, vjust = -0.25), 
          strip.text = element_text(size = 14, face = "bold", hjust = 0.05, vjust = -2.5, colour = grey70K), 
          legend.position = "none", strip.background = element_blank(), 
          axis.ticks = element_blank(), 
          panel.spacing = unit(3, "lines"), 
          panel.grid.major.y = element_line(size = 0.2, colour = grey80K), 
          panel.grid.minor.y = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.x = element_line(size = 0.2, colour = grey80K))
}

#' @describeIn  themes Theme with x and y labels, titles, and gridlines
#' @export
theme_xygridlight <- function(font_normal = 'Lato',
                              font_semi = 'Lato Light',
                              font_light = 'Lato Light') {
  theme_bw() + 
    theme(text = element_text(family = font_light, colour = grey80K), 
          plot.title = element_text(hjust = 0), rect = element_blank(), 
          plot.background = element_blank(), 
          axis.text = element_text(size = 12, colour = grey80K), 
          title = element_text(size = 15, family = font_normal, hjust = 0, colour = grey90K), 
          axis.title = element_text(size = 14, 
                                    family = font_semi, colour = grey80K, hjust = 0.5, vjust = -0.25), 
          strip.text = element_text(size = 14, face = "bold", hjust = 0.05, vjust = -2.5, 
                                    colour = grey70K), 
          legend.position = legend.position, 
          strip.background = element_blank(), 
          axis.ticks = element_blank(), 
          panel.spacing =  unit(1, "lines"), 
          panel.grid.major.y = element_line(size = 0.1, colour = grey70K), 
          panel.grid.minor.y = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.x = element_line(size = 0.1, colour = grey70K))
}

#' @describeIn  themes Theme with light x-grid lines, x and y axis labels, and x-axis title.
#' @export
theme_xgrid <- function(font_normal = 'Lato',
                        font_semi = 'Lato Light',
                        font_light = 'Lato Light',
                        legend.position = 'none',
                        legend.direction = 'horizontal',
                        font_axis_label = 12,
                        font_axis_title = font_axis_label * 1.15,
                        font_facet = font_axis_label * 1.15,
                        font_legend_title = font_axis_label, 
                        font_legend_label = font_axis_label * 0.8,
                        font_title = font_axis_label * 1.3,
                        grey_background = FALSE,
                        background_colour = grey10K,
                        projector = FALSE
) {
  
  # Decide whether to use projector mode or not.
  if(projector == FALSE) {
    grid_stroke = 0.1
    grid_colour = grey60K
    
    # Check if fonts are defined.
    font_normal = llamar::replace_font(font_name = font_normal)
    font_semi = llamar::replace_font(font_name = font_semi)
    font_light = llamar::replace_font(font_name = font_light)
    
  } else {
    grid_stroke = 0.25
    grid_colour = grey75K
    
    # Check if fonts are defined.
    font_normal = llamar::replace_font(font_name = font_normal)
    font_semi = llamar::replace_font(font_name = font_normal)
    font_light = llamar::replace_font(font_name = font_normal)
  }
  
  # Choose background colour
  background_colour = ifelse(grey_background == TRUE, background_colour, 'white')
  
  if(grey_background == TRUE) {
    plot_margin = margin(t = 5, r = 15, b = 5, l = 5, unit = "pt")
  } else{
    plot_margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  }
  
  
  # Define themes.
  theme(
    title = element_text(size = font_title, colour = grey90K, family = font_normal),
    text = element_text(family = font_light, colour = grey60K, hjust = 0.5), 
    
    axis.line = element_blank(), 
    axis.ticks.x = element_blank(), 
    axis.line.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    
    axis.text.x = element_text(size = font_title, colour = grey60K, family = font_light), 
    axis.title.x = element_text(size = font_axis_title, colour = grey60K, family = font_semi), 
    axis.text.y = element_text(size = font_axis_label, colour = grey60K, family = font_light), 
    axis.title.y = element_blank(), 
    
    
    legend.position = legend.position, 
    legend.title = element_text(size = font_legend_title, colour = grey60K, family = font_semi),
    legend.text = element_text(size = font_legend_label, colour = grey60K, family = font_semi),
    legend.direction = legend.direction,
    
    panel.background = element_rect(fill = 'white', colour = NA, size = NA), 
    plot.background = element_rect(fill = background_colour, colour = NA, size = NA, linetype = 1), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.major.x = element_line(size = grid_stroke, colour = grid_colour), 
    panel.grid.minor.y = element_blank(), 
    panel.grid.major.y = element_blank(), 
    panel.border = element_blank(), 
    plot.margin = plot_margin, 
    
    strip.text = element_text(size = font_facet, colour = grey75K, hjust = 0), 
    strip.background = element_blank())
}


#' @describeIn  themes Theme with thin grey border around edge
#' @export
#' 
theme_stroke = function(stroke_size = 0.25,
                        stroke_colour = grey90K) {
  theme(plot.background = element_rect(colour = stroke_colour, size = stroke_size, linetype = 1))
}

#' @describeIn  themes Theme with light y-grid lines, x and y axis labels, and y-axis title.
#' @export
theme_ygrid <- function(font_normal = 'Lato',
                        font_semi = 'Lato Light',
                        font_light = 'Lato Light') {
  theme(title = element_text(size = 16, colour = grey90K, family = font_normal), 
        plot.title = element_text(hjust = 0), 
        axis.line = element_blank(), axis.ticks.x = element_blank(), 
        axis.text.x = element_text(size = 16, colour = grey80K, family = font_light), 
        axis.title.y = element_text(size = 18, colour = grey80K, family = font_semi), 
        axis.text.y = element_text(size = 16, colour = grey80K, family = font_light), 
        axis.title.x = element_blank(), 
        axis.line.y = element_blank(), axis.ticks.y = element_blank(), 
        legend.position = "none", 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.y = element_line(size = 0.1, colour = grey80K), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.border = element_blank(), 
        plot.margin = rep(unit(0, units = "points"), 4), 
        panel.background = element_blank(), 
        strip.text = element_text(size = 13, face = "bold"), 
        strip.background = element_blank())
}




#' @describeIn  themes completely blank theme; similar to theme_void but without legend or margins.
#' @export
theme_blank <- function(legend.position = 'none') {
  theme(title = element_blank(), 
        
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.ticks.length = unit(0, units = "points"), 
        
        strip.text = element_blank(),
        strip.background = element_blank(),
        
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        panel.background = element_blank(), 
        plot.background = element_blank(), 
        
        legend.position = legend.position)
}

#' @describeIn  themes completely blank theme; similar to theme_void but without legend or margins.
#' @export
theme_legend <- function(font_normal = 'Lato',
                         font_semi = 'Lato Light',
                         font_light = 'Lato Light') {
  theme(title = element_text(size = 15, family = font_normal, hjust = 0, colour = grey90K), 
        axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.ticks.length = unit(0, units = "points"), panel.border = element_blank(), panel.grid = element_blank(), 
        panel.background = element_blank(), plot.background = element_blank(), legend.text = element_text(size = 12, 
                                                                                                          family = font_light, hjust = 0, colour = grey60K))
}

#' @describeIn  themes Theme with x labels, x title, and x-axis line; no gridlines
#' @export
theme_xaxis <- function(font_normal = 'Lato',
                        font_semi = 'Lato Light',
                        font_light = 'Lato Light') {
  theme_bw() + theme(text = element_text(family = font_light, colour = grey60K), 
                     rect = element_blank(), plot.background = element_blank(), axis.text = element_text(size = 12, 
                                                                                                         colour = grey60K), axis.text.y = element_blank(), title = element_text(size = 15, 
                                                                                                                                                                                family = font_normal, hjust = 0, colour = grey90K), axis.title = element_text(size = 14, 
                                                                                                                                                                                                                                                              family = font_semi, colour = grey60K, hjust = 0.5, vjust = -0.25), 
                     axis.title.y = element_blank(), 
                     strip.text = element_text(size = 14, face = "bold", hjust = 0.05, vjust = -2.5, colour = "#4D525A"), 
                     legend.position = "none", 
                     strip.background = element_blank(), 
                     axis.ticks = element_blank(), 
                     panel.spacing = unit(3, "lines"),
                     axis.line = element_line(size = 0.2, colour = grey50K), 
                     axis.line.y = element_blank(), 
                     panel.grid.minor = element_blank(), 
                     panel.grid.major = element_blank())
}

#' @describeIn  themes Theme with y labels, titles, and y-axis line; no gridlines
#' @export
theme_yaxis <- function(font_normal = 'Lato',
                        font_semi = 'Lato Light',
                        font_light = 'Lato Light') {
  theme_bw() + theme(text = element_text(family = font_light, colour = grey60K), 
                     rect = element_blank(), 
                     plot.background = element_blank(), 
                     axis.text = element_text(size = 12, colour = grey60K), 
                     axis.text.x = element_blank(), 
                     title = element_text(size = 15, family = font_normal, hjust = 0, colour = grey90K), 
                     axis.title = element_text(size = 14, family = font_semi, colour = grey60K, hjust = 0.5, vjust = -0.25), 
                     axis.title.x = element_blank(), 
                     strip.text = element_text(size = 14, face = "bold", hjust = 0.05, vjust = -2.5, colour = "#4D525A"), 
                     legend.position = "none", 
                     strip.background = element_blank(), 
                     axis.ticks = element_blank(), panel.spacing = unit(3, "lines"),  
                     axis.line = element_line(size = 0.2, colour = grey50K), 
                     axis.line.x = element_blank(), 
                     panel.grid.minor = element_blank(), 
                     panel.grid.major = element_blank())
}

#' @describeIn themes Theme with x and y axis labels; no gridlines.
#' @export
theme_xylab <- function(font_normal = 'Lato',
                        font_semi = 'Lato Light',
                        font_light = 'Lato Light') {
  theme_bw() + 
    theme(text = element_text(family = font_light, colour = grey60K), 
          rect = element_blank(), 
          plot.background = element_blank(), 
          axis.text = element_text(size = 10, colour = grey60K), 
          title = element_text(size = 10, family = font_normal, 
                               hjust = 0, colour = grey90K), 
          axis.title = element_blank(), 
          strip.text = element_text(size = 14, family = font_semi, hjust = 0.05, vjust = -2.5, colour = grey90K), 
          legend.position = "none", 
          strip.background = element_blank(), 
          axis.ticks = element_blank(), 
          axis.ticks.length = unit(0, "lines"),
          panel.spacing =  unit(0, "lines"), 
          panel.spacing.x = unit(0, "lines"),
          panel.spacing.y = unit(0, "lines"),
          plot.margin = unit(c(0, 0, 0, 0), "lines"),
          panel.grid.major.y = element_blank(), 
          panel.grid.minor.y = element_blank(), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.major.x = element_blank())
}

#' @describeIn themes Theme with  y axis labels; no gridlines.
#' @export
theme_ylab <- function(font_normal = 'Lato',
                       font_semi = 'Lato Light',
                       font_light = 'Lato Light') {
  theme_bw() + theme(text = element_text(family = font_light, colour = grey60K), 
                     rect = element_blank(), 
                     plot.background = element_blank(), 
                     axis.text = element_text(size = 12, colour = grey60K), 
                     axis.text.x = element_blank(), 
                     title = element_text(size = 15, family = font_normal, hjust = 0, colour = grey90K), 
                     axis.title = element_blank(), 
                     legend.position = "none", 
                     strip.background = element_blank(), 
                     axis.ticks = element_blank(), 
                     panel.spacing = unit(1, "lines"),  
                     panel.grid.major.y = element_blank(), 
                     panel.grid.minor.y = element_blank(), 
                     panel.grid.minor.x = element_blank(), 
                     panel.grid.major.x = element_blank())
}

#' @describeIn  themes Theme with only x and y labels; no axes, axis titles, gridlines, or legends
#' @export
theme_labelsOnly <- function(font_normal = 'Lato',
                             font_semi = 'Lato Light',
                             font_light = 'Lato Light') {
  theme(title = element_text(size = 16, hjust = 0, colour = grey90K, family = font_normal), 
        axis.title = element_blank(), axis.text = element_text(size = 12, hjust = 0.5, colour = grey60K, 
                                                               family = font_light), axis.ticks = element_blank(), panel.border = element_blank(), 
        plot.margin = rep(unit(0, units = "points"), 4), panel.grid = element_blank(), panel.background = element_blank(), 
        plot.background = element_blank(), legend.position = "none")
}
