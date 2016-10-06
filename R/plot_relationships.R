

ggplot(ch, aes(x = rural_cat, y = stuntingZ)) +
  geom_boxplot() +
  annotate(geom = 'text', label = 'n') +
  scale_y_continuous(limits = stunting_range)

ggplot(ch, aes(x = rural_cat, y = isStunted)) +
  stat_summary(geom = 'point', size = 5, fun.y = 'mean') +
  scale_y_continuous(limits = stunted_range)

ggplot(ch, aes(x = wealth_idx, y = isStunted)) +
  geom_smooth() +
  theme_bw() +
  scale_y_continuous(limits = stunted_range)


plot_relationships = function(model, 
                              binary_y = FALSE,
                              stunting_range = c(-4, 4),
                              stunted_range = c(0, 1)){
  
  # Pull out the data from the model
  df = model$model
  
  # find the dependent and independent variables in model
  vars = attr(model$terms, 'dataClasses')
  
  vars = data.frame(variable = names(vars),
                    type = vars) %>% 
    mutate(variable = as.character(variable),
           type = as.character(type))
  
  dpndt_var = vars %>% slice(1)
  
  indpndt_vars = vars %>% slice(-1)
  
  
  # loop over the variables and plot the average value
  
  for (i in seq_along(indpndt_vars$variable)) {
    
    if(binary_y == TRUE & indpndt_vars$type[i] == 'factor') {
      
      p = ggplot(df, aes_string(x = indpndt_vars$variable[i], y = dpndt_var$variable)) +
        stat_summary(geom = 'point', size = 5, fun.y = 'mean_cl_boot', colour = '#66c2a5') +
        scale_y_continuous(limits = stunted_range) +
        theme_ygrid()
      
      print(p)
      readline()
    } else if (binary_y == TRUE & indpndt_vars$type[i] == 'numeric') {
        
      } 

    
  }
  
}