#' Plot relationships between variables in a model 
#' 
#' Loops through independent variables within a model and 
#' plots their relationship with the dependent variable
#' 
#' @import ggplot2 dplyr forcats RColorBrewer
#' 
#' @param model Fitted model

#' @examples
#' data(diamonds)
#' model1 <- lm(price ~ carat + cut*color, data=diamonds)
#' plot_relationships(model1)
#' 
#' @export

plot_relationships = function(model, 
                              binary_y = FALSE,
                              cont_range = c(-4, 4),
                              binary_range = c(0, 1),
                              binary_colour = '#66c2a5',
                              cont_colour = '#3288bd'){
  
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
        stat_summary(geom = 'point', size = 5, fun.y = 'mean_cl_boot', colour = binary_colour) +
        scale_y_continuous(limits = binary_range, labels = scales::percent) +
        theme_ygrid()
      
      print(p)
      readline()
      
    } else if (binary_y == TRUE & indpndt_vars$type[i] == 'numeric') {
      p = ggplot(df, aes_string(x = indpndt_vars$variable[i], y = dpndt_var$variable)) +
        geom_smooth(colour = binary_colour) +
        scale_y_continuous(limits = binary_range, labels = scales::percent) +
        theme_xygridlight()
      
      print(p)
      readline()
      
    } else if(indpndt_vars$type[i] == 'factor') {
      p = ggplot(df, aes_string(x = indpndt_vars$variable[i], 
                                y = dpndt_var$variable,
                                fill = indpndt_vars$variable[i])) +
        geom_boxplot(alpha = 0.5) +
        scale_y_continuous(limits = cont_range) +
        scale_fill_brewer(palette = 'Spectral') +
        theme_ygrid()
      
      print(p)
      readline()
      
    } else if(indpndt_vars$type[i] == 'numeric')
      p = ggplot(df, aes_string(x = indpndt_vars$variable[i], y = dpndt_var$variable)) +
        geom_smooth(colour = cont_colour) +
        scale_y_continuous(limits = cont_range) +
        theme_xygridlight()
    
    print(p)
    readline()
    
  }
  
}