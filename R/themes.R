#' Customized ggplot themes
#'
#' @name themes
NULL
#> NULL
#' @export
#' @examples
#' ggplot(mtcars, aes(x = mpg, y = wt, colour = cyl)) + geom_point() + theme_xylab()
#' 

#' @describeIn  themes Theme with axis labels, titles, grid lines, axes, and legend.
#' @export
theme_basic <- function() {
  theme_bw() +
    theme(
      text = element_text(family = 'Segoe UI Light', colour = grey60K),
      plot.background = element_blank(),
      panel.border = element_rect(size = 0.2, colour = grey90K, fill = NA),
      axis.text = element_text(size = 12,  color = grey60K),
      title =  element_text(size = 15, family = "Segoe UI", hjust = 0, color = grey90K),
      axis.title =  element_text(size = 14, family = "Segoe UI Semilight", color = grey60K, hjust = 0.5, vjust = -0.25),
      strip.background = element_blank(),
      axis.ticks = element_blank(),
      panel.margin = unit(3, 'lines'),
      panel.grid.major.y = element_line(size = 0.2, color = grey30K),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(size = 0.1, color = grey30K),
      strip.text = element_text(size=13, color = grey50K, family = 'Segoe UI Semilight'),
      legend.position = c(0.85, 0.85),
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 11),
      legend.background = element_blank(),
      strip.background = element_blank()
    )
}

#' @describeIn  themes Theme with x and y labels, titles, and gridlines
#' @export
theme_xygrid<- function() {
  theme_bw() +
    theme(
      text = element_text(family = 'Segoe UI Light', colour = grey60K),
      plot.title = element_text(hjust = 0),
      rect = element_blank(),
      plot.background = element_blank(),
      axis.text = element_text(size = 12,  color = grey60K),
      title =  element_text(size = 15, family = "Segoe UI", hjust = 0, color = grey90K),
      axis.title =  element_text(size = 14, family = "Segoe UI Semilight", color = grey80K, hjust = 0.5, vjust = -0.25),
      strip.text = element_text(size=14, face = 'bold', hjust = 0.05, vjust = -2.5, color = grey70K),
      legend.position = 'none',
      strip.background = element_blank(),
      axis.ticks = element_blank(),
      panel.margin = unit(3, 'lines'),
      panel.grid.major.y = element_line(size = 0.2, color = grey80K),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(size = 0.2, color = grey80K))
}



#' @describeIn  themes Theme with light x-grid lines, x and y axis labels, and x-axis title.
#' @export
theme_xgrid<- function() {
  theme(title = element_text(size = 16, color = grey90K, 
                             family = 'Segoe UI'),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 16, color = grey60K, family = 'Segoe UI Light'),
        axis.title.x = element_text(size = 18, color = grey60K, family = 'Segoe UI Semilight'),
        axis.text.y = element_text(size = 16, color = grey60K, family = 'Segoe UI Light'),
        axis.title.y = element_blank(), 
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position="none",
        panel.background = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(size = 0.1, colour = grey60K),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        plot.margin = rep(unit(0, units = 'points'),4),
        panel.background = element_blank(), 
        strip.text = element_text(size=13, face = 'bold'),
        strip.background = element_blank()
  )
}


#' @describeIn  themes Theme with light y-grid lines, x and y axis labels, and y-axis title.
#' @export
theme_ygrid<- function() {
  theme(title = element_text(size = 16, color = grey90K, 
                             family = 'Segoe UI'),
        plot.title = element_text(hjust = 0),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size = 16, color = grey80K, family = 'Segoe UI Light'),
        axis.title.y = element_text(size = 18, color = grey80K, family = 'Segoe UI Semilight'),
        axis.text.y = element_text(size = 16, color = grey80K, family = 'Segoe UI Light'),
        axis.title.x = element_blank(), 
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position="none",
        panel.background = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.2, colour = grey80K),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        plot.margin = rep(unit(0, units = 'points'),4),
        panel.background = element_blank(), 
        strip.text = element_text(size=13, face = 'bold'),
        strip.background = element_blank()
  )
}




#' @describeIn  themes completely blank theme; similar to theme_void but without legend or margins.
#' @export
theme_blank <- function() {
  theme(
    title = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, units = 'points'),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(), 
    plot.background = element_blank(), 
    legend.position="none"
  )
}

#' @describeIn  themes completely blank theme; similar to theme_void but without legend or margins.
#' @export
theme_legend <- function() {
  theme(
    title = element_text(size = 15, family = "Segoe UI", hjust = 0, color = grey90K),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.ticks.length = unit(0, units = 'points'),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(), 
    plot.background = element_blank(), 
    legend.text = element_text(size = 12, family = "Segoe UI Light", hjust = 0, color = grey60K)
  )
}

#' @describeIn  themes Theme with x labels, x title, and x-axis line; no gridlines
#' @export
theme_xaxis <- function() {
  theme_bw() +
    theme(
      text = element_text(family = 'Segoe UI Light', colour = grey60K),
      rect = element_blank(),
      plot.background = element_blank(),
      axis.text = element_text(size = 12,  color = grey60K),
      axis.text.y = element_blank(),
      title =  element_text(size = 15, family = "Segoe UI", hjust = 0, color = grey90K),
      axis.title =  element_text(size = 14, family = "Segoe UI Semilight", color = grey60K, hjust = 0.5, vjust = -0.25),
      axis.title.y = element_blank(),
      strip.text = element_text(size=14, face = 'bold', hjust = 0.05, vjust = -2.5, color = '#4D525A'),
      legend.position = 'none',
      strip.background = element_blank(),
      axis.ticks = element_blank(),
      panel.margin = unit(3, 'lines'),
      axis.line = element_line(size = 0.2, colour = grey50K),
      axis.line.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()
    )
}

#' @describeIn  themes Theme with y labels, titles, and y-axis line; no gridlines
#' @export
theme_yaxis <- function() {
  theme_bw() +
    theme(
      text = element_text(family = 'Segoe UI Light', colour = grey60K),
      rect = element_blank(),
      plot.background = element_blank(),
      axis.text = element_text(size = 12,  color = grey60K),
      axis.text.x = element_blank(),
      title =  element_text(size = 15, family = "Segoe UI", hjust = 0, color = grey90K),
      axis.title =  element_text(size = 14, family = "Segoe UI Semilight", color = grey60K, hjust = 0.5, vjust = -0.25),
      axis.title.x = element_blank(),
      strip.text = element_text(size=14, face = 'bold', hjust = 0.05, vjust = -2.5, color = '#4D525A'),
      legend.position = 'none',
      strip.background = element_blank(),
      axis.ticks = element_blank(),
      panel.margin = unit(3, 'lines'),
      axis.line = element_line(size = 0.2, colour = grey50K),
      axis.line.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()
    )
}

#' @describeIn themes Theme with x and y axis labels; no gridlines.
#' @export
theme_xylab<- function() {
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

#' @describeIn  themes Theme with only x and y labels; no axes, axis titles, gridlines, or legends
#' @export
theme_labelsOnly <- function() {
  theme(
    title =  element_text(size = 16, hjust = 0, color = grey90K,
                          family = 'Segoe UI'),
    axis.title = element_blank(),
    axis.text = element_text(size = 12, hjust = 0.5, 
                             color = grey60K, family = 'Segoe UI Light'),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    plot.margin = rep(unit(0, units = 'points'),4),
    panel.grid = element_blank(),
    panel.background = element_blank(), 
    plot.background = element_blank(), 
    legend.position="none"
  )
}
