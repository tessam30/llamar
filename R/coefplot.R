#' Plot coefficients of a model with errors 
#' 
#' Inspired by https://github.com/jaredlander/coefplot/blob/master/R/coefplot.r
#' but works w/ ggplot2 version > 2.2
#' 
#' @import ggplot2 broom dplyr forcats RColorBrewer
#' 
#' @param model Fitted model

#' @examples
#' data(diamonds)
#' model1 <- lm(price ~ carat + cut*color, data=diamonds)
#' coefplot(model1)
#' 
#' @export

coefplot = function(model,
                    level = 0.95,
                    exclude_intercept = TRUE,
                    plot_left_labels = TRUE,
                    plot_right_labels = TRUE,
                    alpha_insignificant = 0.3,
                    size_point = 3,
                    x_buffer = 0.1,
                    font_normal = 'Lato',
                    font_semi = 'Lato Light',
                    font_light = 'Lato Light'){
  
  # pull out coefficients in a nice data frame
  coefs = broom::tidy(model)
  
  # ditto for confidence intervals; assumes 95% level
  CIs = broom::confint_tidy(model, 0.95)
  
  df = dplyr::bind_cols(coefs, CIs)
  
  # determine if statistically significant difference
  df = df %>% 
    mutate(stat_signif = ifelse(p.value < (1 - level), 
                                1, alpha_insignificant))
  
  # find limits for colors
  if(exclude_intercept == TRUE){
    max_estimate = df %>% filter(term != '(Intercept)') %>% summarise(min = min(estimate),
                                                                      max = max(estimate))
    
    max_estimate = max(max_estimate$max, abs(max_estimate$min))
    
    # find limits for labels
    xmin = min(df$conf.low)
    xmax = max(df$conf.high)
    
    x_breaks = pretty(c(xmin, xmax), n = 5)
    x_breaks = x_breaks[x_breaks < xmax & x_breaks > xmin] # truncate at edges
    
  } else {
    max_estimate = max(max(df$estimate), abs(min(df$estimate)))
  }
  
  # plot --------------------------------------------------------------------
  
  p = ggplot(df, aes(x = estimate, y = forcats::fct_reorder(term, estimate),
                     fill = estimate, alpha = stat_signif)) +
    
    # -- zero point --
    geom_vline(xintercept = 0, colour = grey90K, size = 0.2) +
    
    
    # -- error bars --
    geom_segment(aes(x = conf.low, xend = conf.high,
                     yend = forcats::fct_reorder(term, estimate)),
                 colour = grey15K, size = 1.5) +
    
    # -- estimate --
    geom_point(size = size_point,
               shape = 21,
               stroke = 0.2, 
               colour = grey90K) +
    
    # -- scales --
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'RdBu'),
                         limits = c(-1 * max_estimate, max_estimate)) + 
    scale_alpha_identity() +
    
    # -- themes --
    theme_xgrid(font_normal = font_normal, font_semi = font_semi, font_light = font_light) +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_blank())
  
  # -- variable labels --
  if(plot_left_labels & plot_right_labels){
    p = p + geom_text(aes(x = xmin - abs(xmin) * x_buffer, y = forcats::fct_reorder(term, estimate),
                          label = forcats::fct_reorder(term, estimate)),
                      size = 3.5, hjust = 'outward',
                      colour = grey90K, family = font_light) +
      geom_text(aes(x = xmax + xmax * x_buffer, y = forcats::fct_reorder(term, estimate),
                    label = forcats::fct_reorder(term, estimate)),
                size = 3.5, hjust = 'outward',
                colour = grey90K, family = font_light) +
      scale_x_continuous(expand = c(x_buffer * 3.5, x_buffer * 3.5),
                         breaks = x_breaks)
  } else if(plot_left_labels) {
    p = p + geom_text(aes(x = xmin - abs(xmin) * x_buffer, y = forcats::fct_reorder(term, estimate),
                          label = forcats::fct_reorder(term, estimate)),
                      size = 3.5, hjust = 'outward',
                      colour = grey90K, family = font_light) +
      scale_x_continuous(expand = c(x_buffer * 3.5, 0),
                         breaks = x_breaks)
  } else if(plot_right_labels) {
    p = p + geom_text(aes(x = xmax + xmax * x_buffer, y = forcats::fct_reorder(term, estimate),
                          label = forcats::fct_reorder(term, estimate)),
                      size = 3.5, hjust = 'outward',
                      colour = grey90K, family = font_light) +
      scale_x_continuous(expand = c(x_buffer * 3.5, 0),
                         breaks = x_breaks)
  } else {
    p
  }
  
  return(p)
}

