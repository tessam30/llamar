#' Plots the sample size across a given variable.
#'  
#' @export
#' 
#' 
plot_n = function(df,
                  by_var,
                  n_var = 'N',
                  
                  sort_by = 'avg',
                  sort_asc = FALSE,
                  x_label = NULL,
                  
                  incl_y_labels = FALSE,
                  
                  dot_shape = 'square',
                  dot_size = 15,
                  dot_stroke = 0.1,
                  dot_outline = grey90K,
                  
                  low_colour = grey10K,
                  high_colour = grey70K,
                  
                  label_size = 4,
                  
                  file_name = NULL,
                  width = 10,
                  height = 6,
                  saveBoth = FALSE,
                  
                  font_normal = 'Lato',
                  font_semi = 'Lato',
                  font_light = 'Lato Light',
                  panel_spacing = 1, # panel spacing, in lines
                  font_axis_label = 12,
                  font_axis_title = font_axis_label * 1.15,
                  font_facet = font_axis_label * 1.15,
                  font_legend_title = font_axis_label, 
                  font_legend_label = font_axis_label * 0.8,
                  font_subtitle = font_axis_label * 1.2,
                  font_title = font_axis_label * 1.3,
                  legend.position = 'none', 
                  legend.direction = 'horizontal',
                  grey_background = FALSE,
                  background_colour = grey10K,
                  projector = FALSE) {
  
  # determine shape ---------------------------------------------------------
  
  if(dot_shape == 'square'){
    dot_shape = 22
  } else if(dot_shape == 'circle') {
    dot_shape = 21
  } else if (is.numeric(dot_shape)) {
    dot_shape = dot_shape
  } else{
    warning('unknown dot shape')
    dot_shape = 21
  }
  
  
  # determine sorting ----------------------------------------------------------
  if(!is.null(sort_by)) {
    if(sort_by %in% colnames(df)){
      if(is.numeric(df[[sort_by]])) {
        
        y_var = paste0('forcats::fct_reorder(', by_var, ',', 
                       sort_by, ', .desc = ', sort_asc, ')')
      } else {
        warning('sort_by column does not contain numeric data. Sorting by factor levels')
        y_var = paste0('forcats::fct_reorder(', by_var, 
                       ', as.numeric(', 
                       sort_by, '), .desc = ', sort_asc, ')')
      }
      
    } else {
      warning('sorting variable `sort_by` is not in df. No sorting will be applied.')
      y_var = by_var
    }
  } else{
    y_var = by_var
  }
  
  df = df %>% 
    map_colour_text(n_var, colour_palette = c(low_colour, high_colour))
  
  
  p = ggplot(df, aes_string(x = '1', 
                            y = y_var, 
                            label = n_var,
                            colour = 'text_colour', 
                            fill = n_var)) +
    
    geom_point(shape = dot_shape, size = dot_size,
               stroke = dot_stroke, colour = dot_outline) +
    
    geom_text(size = label_size, family = font_normal) +
    
    scale_fill_gradient2(low = low_colour, high = high_colour) +
    scale_colour_identity() +
    scale_x_continuous(limits = c(0.9, 1.1)) +
    xlab(x_label) +
    
    theme_xylab(font_normal = font_normal, font_semi = font_semi,
                font_light = font_light, legend.position = legend.position,
                legend.direction = legend.direction, panel_spacing = panel_spacing,
                font_axis_label = font_axis_label, font_axis_title = font_axis_title, 
                font_facet = font_facet, font_legend_title = font_legend_title, 
                font_legend_label = font_legend_label, font_subtitle = font_subtitle, 
                font_title = font_title, grey_background = grey_background, 
                background_colour = background_colour, projector = projector) +
    theme(axis.text.x = element_text(colour = 'white'))
  
  
  if(incl_y_labels == FALSE) {
    p = p +
      theme(axis.text.y = element_blank())
  }
  # save plot ----------------------------------------------------------
  if(!is.null(file_name)) {
    save_plot(file_name, saveBoth = saveBoth, width = width, height = height)
  }
  
  # return ----------------------------------------------------------
  
  return(p)
}


