plot_comparison = function(df,
                           # negative_ontop = TRUE,
                           # negative_good = FALSE,
                           exclude_intercept = TRUE,
                           alpha_insignificant = 0,
                           font_normal = 'Lato',
                           font_semi = 'Lato Light',
                           font_light = 'Lato Light'){
  
  if(exclude_intercept == TRUE){
    max_estimate = df %>% filter(term != '(Intercept)') %>% summarise(min = min(estimate),
                                                                      max = max(estimate))
    
    max_estimate = max(max_estimate$max, abs(max_estimate$min))
    
  } else {
    max_estimate = max(max(df$estimate), abs(min(df$estimate)))
  }
  
  # check if the fonts are installed.  If not, default to 'sans'.
  font_normal = llamar::replace_font(font_normal)
  font_semi = llamar::replace_font(font_semi)
  font_light = llamar::replace_font(font_light)
  
  
  # Find alpha levels 
  df = df %>% 
    mutate(stat_signif = ifelse(p.value < (1 - level), 
                                1, 0))
  
  
  ggplot(df, aes(x = model, y = fct_reorder(term, estimate, .desc = TRUE),
                 fill = estimate,
                 alpha = stat_signif,
                 label = sprintf("%0.1f", estimate))) +
    geom_label(colour = grey75K, size = 3,
               family = 'Lato Light') +
    
    scale_x_discrete(position = 'top') +
    scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, 'RdYlBu'),
                         limits = c(-1 * max_estimate, max_estimate)) +
    scale_alpha_identity() +
    theme_xylab()
}

get_coefs = function(models, model_num, cluster_col = NA) {
  model_name = names(models)[model_num]
  
  model = models[[model_name]]
  
  
  # ditto for confidence intervals; assumes 95% level
  if(is.na(all(cluster_col))){
    # -- MODEL 1 --
    coefs1 = broom::tidy(model)
    
    CIs1 = broom::confint_tidy(model, level)
    
    # Remove any NAs from CIs (factors which have been dropped from model)
    CIs1 = CIs1 %>% filter(!is.na(conf.low))
    
    df1 = dplyr::bind_cols(coefs1, CIs1) %>% 
      mutate(model = model_name)
    
    
    
  } else {
    # check that cluster_col has the right number of observations
    if(length(cluster_col) != (length(model$fitted.values) + length(model$na.action))) {
      stop('check cluster_col; incorrect number of observations inputted.')
    }
    
    # recalculate the CIs based on the clustering variable
    coefs1 = lmtest::coeftest(model, vcov = multiwayvcov::cluster.vcov(model, cluster_col))
    
    df1 = data.frame(term = row.names(coefs1),
                     estimate = coefs1[, 'Estimate'],
                     std.error = coefs1[, 'Std. Error'],
                     statistic = coefs1[, 't value'],
                     p.value = coefs1[,'Pr(>|t|)'])
    
    df1 = df1 %>% 
      mutate(conf.low = estimate - CI_factor * std.error,
             conf.high = estimate + CI_factor * std.error, 
             model = model_name)
    
    
  }
  
  return(df1)
}



compare_models = function(all_models,
                          # negative_ontop = TRUE,
                          # negative_good = FALSE,
                          cluster_col = NA,
                          level = 0.95,
                          CI_factor = 1.96, # assuming normal distribution, 95% CI level
                          exclude_intercept = TRUE,
                          # plot_left_labels = TRUE,
                          # plot_right_labels = FALSE,
                          alpha_insignificant = 0,
                          # size_point = 3,
                          # x_buffer = 0.1,
                          # label_margin = 1,
                          font_normal = 'Lato',
                          font_semi = 'Lato Light',
                          font_light = 'Lato Light'){
  
  
  # pull out coefficients in a nice data frame
  df = lapply(seq_along(all_models), function(x) get_coefs(all_models, x, cluster_col = NA))
  
  # merge together
  df = bind_rows(df)


plot_comparison(df, 
                exclude_intercept = exclude_intercept,
                alpha_insignificant = alpha_insignificant,
                font_normal = font_normal,
                font_semi = font_semi,
                font_light = font_light)
}
