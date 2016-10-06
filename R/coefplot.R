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
                    size_point = 3){
  
  # pull out coefficients in a nice data frame
  coefs = broom::tidy(model)
  
  # ditto for confidence intervals; assumes 95% level
  CIs = broom::confint_tidy(model, 0.95)
  
  df = dplyr::bind_cols(coefs, CIs)
  
  if(exclude_intercept == TRUE){
    max_estimate = df %>% filter(term != '(intercept)') %>% summarise(min = min(estimate),
                                                                      max = max(estimate))
    
    max_estimate = max(max_estimate$max, abs(max_estimate$min))
  
    } else {
    max_estimate = max(max(df$estimate), abs(min(df$estimate)))
  }
  
  ggplot(df, aes(x = estimate, y = forcats::fct_reorder(term, estimate),
                 fill = estimate)) +
    geom_segment(aes(x = conf.low, xend = conf.high,
                     yend = forcats::fct_reorder(term, estimate)),
                 colour = grey15K, size = 1.5) +
    geom_point(size = size_point,
               shape = 21,
               stroke = 0.2, 
               colour = grey90K) +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'RdBu'),
                         limits = c(-1 * max_estimate, max_estimate)) + 
    theme_xgrid() +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 10, colour = grey90K))
}