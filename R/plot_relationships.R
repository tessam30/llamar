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
                              cont_range = c(-4, 4),
                              binary_range = c(0, 1),
                              binary_colour = '#66c2a5',
                              cont_colour = '#3288bd'){
  
  # Pull out the data from the model
  df = model$model
  
  # find binary variables
  binary = df  %>% summarise_each(funs(all(. %in% 0:1)))
  binary = data.frame(binary = t(binary),
                      variable = colnames(binary))
  
  # find the dependent and independent variables in model
  vars = attr(model$terms, 'dataClasses')
  
  vars = data.frame(variable = names(vars),
                    type = vars) %>% 
    mutate(variable = as.character(variable),
           type = as.character(type))
  
  vars = left_join(vars, binary, by = 'variable') 
  
  vars = vars %>% 
    mutate(type = case_when(vars$type == 'factor' ~ 'factor',
                            vars$binary == TRUE & vars$type == 'numeric' ~ 'binary',
                            vars$type == 'numeric' ~ 'numeric',
                            TRUE ~ 'unknown'))
  
  
  dpndt_var = vars %>% slice(1)
  
  indpndt_vars = vars %>% slice(-1)
  print(dpndt_var$type)
  if(dpndt_var$type == 'binary'){
    print('binary')
    binary_y = TRUE 
  } else {
    binary_y == FALSE
  }
  print(binary_y)
  
  # loop over the variables and plot the average value
  
  for (i in seq_along(indpndt_vars$variable)) {
    
    if(binary_y == TRUE){
      if(indpndt_vars$type[i] == 'factor' | indpndt_vars$type[i] == 'binary') {
        print(1)
        p = ggplot(df, aes_string(x = indpndt_vars$variable[i], y = dpndt_var$variable)) +
          stat_summary(geom = 'point', size = 5, fun.y = 'mean_cl_boot', colour = binary_colour) +
          scale_y_continuous(limits = binary_range, labels = scales::percent) +
          ggtitle(indpndt_vars$variable[i]) +
          theme_ygrid()

        print(p)
        readline()
        
      } else if (indpndt_vars$type[i] == 'numeric') {
        print(2)
        p = ggplot(df, aes_string(x = indpndt_vars$variable[i], y = dpndt_var$variable)) +
          geom_smooth(colour = binary_colour) +
          scale_y_continuous(limits = binary_range, labels = scales::percent) +
          ggtitle(indpndt_vars$variable[i]) +
          theme_xygridlight()
        
        print(p)
        readline()
      }
    } else if(indpndt_vars$type[i] == 'factor' | indpndt_vars$type[i] == 'binary') {
      print(3)
      p = ggplot(df, aes_string(x = indpndt_vars$variable[i],
                                y = dpndt_var$variable,
                                fill = indpndt_vars$variable[i])) +
        geom_boxplot(alpha = 0.5) +
        scale_y_continuous(limits = cont_range) +
        scale_fill_brewer(palette = 'Spectral') +
        ggtitle(indpndt_vars$variable[i]) +
        theme_ygrid()

      print(p)
      readline()
      
    } else if(indpndt_vars$type[i] == 'numeric')
      print(4)
      p = ggplot(df, aes_string(x = indpndt_vars$variable[i], y = dpndt_var$variable)) +
        geom_smooth(colour = cont_colour) +
        scale_y_continuous(limits = cont_range) +
        ggtitle(indpndt_vars$variable[i]) +
        theme_xygridlight()
    
    print(p)
    readline()
    
  }
  
}