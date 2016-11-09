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
                              orig_df,
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
  
  if(dpndt_var$type == 'binary'){
    binary_y = TRUE 
  } else {
    binary_y = FALSE
  }
  
  # loop over the variables and plot the average value
  
  for (i in seq_along(indpndt_vars$variable)) {
    current_type = indpndt_vars$type[i]
    
    current_var = indpndt_vars$variable[i]
    
    
    if(binary_y == TRUE){
      if(current_type == 'factor' | current_type == 'binary') {
        p = ggplot(df, aes_string(x = current_var, y = dpndt_var$variable)) +
          stat_summary(geom = 'pointrange', size = 1, fun.data = 'mean_cl_boot', colour = binary_colour) +
          scale_y_continuous(limits = binary_range, labels = scales::percent) +
          ggtitle(current_var, subtitle = paste0('Number of NAs: ', sum(is.na(orig_df[[current_var]])))) +
          theme_xygridlight() +
          theme(panel.grid.major.x = element_blank())
        
        print(p)
        readline()
        
      } else {
        p = ggplot(df, aes_string(x = current_var, y = dpndt_var$variable)) +
          geom_smooth(colour = binary_colour) +
          scale_y_continuous(limits = binary_range, labels = scales::percent) +
          ggtitle(current_var, subtitle = paste0('Number of NAs: ', sum(is.na(orig_df[[current_var]])))) +
          theme_xygridlight()
        
        print(p)
        readline()
      }
    } else {
      if(current_type == 'factor' | current_type == 'binary') {
        p = ggplot(df, aes_string(x = paste0('factor(', current_var, ')'),
                                           y = dpndt_var$variable,
                                           fill = paste0('factor(', current_var, ')'))) +
          geom_boxplot(alpha = 0.5) +
          scale_y_continuous(limits = cont_range) +
          scale_fill_brewer(palette = 'Spectral') +
          ggtitle(current_var, subtitle = paste0('Number of NAs: ', sum(is.na(orig_df[[current_var]])))) +
          theme_xygridlight() +
          theme(panel.grid.major.x = element_blank())
        
        print(p)
        readline()
        
      } else { #if(current_type == 'numeric')
        p = ggplot(df, aes_string(x = current_var, y = dpndt_var$variable)) +
          geom_smooth(colour = cont_colour) +
          scale_y_continuous(limits = cont_range) +
          theme_xygridlight() +           
          ggtitle(current_var, subtitle = paste0('Number of NAs: ', sum(is.na(orig_df[[current_var]]))))
          
        print(p)
        readline()
        
      }
    }
  }
}