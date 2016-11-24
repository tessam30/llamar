# to do:
# examples
# documentation
# dot plot
# switch: dot or bump
# apply sampling wts.
# check good packrat
# images --> website

#' @export

plot_change = function(df,
                       value_var, # value to average
                       time_var = 'year',
                       region_var = 'region',
                       
                       plot_type = 'dot', # dot or bump/slope
                       
                       facet_var = region_var,
                       sort_by = 'diff', # one of: 'diff', 'first', 'last', 'none'
                       sort_desc = TRUE,
                       
                       file_name = NA,
                       width = 10,
                       height = 6,
                       saveBoth = FALSE,
                       
                       line_stroke = 0.5,
                       dot_size = 5,
                       dot_shape = 21, 
                       label_size = 4, 
                       label_x_offset = 0.3,
                       value_y_offset = NA,
                       
                       label_vals = TRUE,
                       percent_vals = FALSE,
                       
                       tufte_style = FALSE,
                       
                       x_buffer = 0.5, # how much to adjust the x axis for labels
                       
                       font_normal = 'Lato',
                       font_semi = 'Lato',
                       font_light = 'Lato Light',
                       panel_spacing = 3, # panel spacing, in lines
                       font_axis_label = 12,
                       font_axis_title = font_axis_label * 1.15,
                       font_facet = font_axis_label * 1.15,
                       font_legend_title = font_axis_label, 
                       font_legend_label = font_axis_label * 0.8,
                       font_subtitle = font_axis_label * 1.2,
                       font_title = font_axis_label * 1.3,
                       grey_background = FALSE,
                       background_colour = grey10K,
                       projector = FALSE) {
  
  # facet variable isn't defined in the data frame 
  if(!is.na(facet_var) & !facet_var %in% colnames(df)) {
    warning('facet_var is not in df. Facetting is removed.')
    facet_var = NA
  }
  
  # Is there a data label that needs to be applied?
  if(!is.null(attr(df[[region_var]], 'labels'))) {
    df2 = llamar::factorize(df, df, region_var, region_var)
  }
  
  
  if(is.na(facet_var)){
  df2 = df2 %>% 
    filter_(paste0('!is.na(', value_var,')')) %>% 
    group_by_(time_var, region_var) %>% 
    summarise_(.dots = list(N = 'n()',
                            avg = paste0('mean(', value_var, ')')))
  } else {
    df2 = df2 %>% 
      filter_(paste0('!is.na(', value_var,')')) %>% 
      group_by_(time_var, region_var, facet_var) %>% 
      summarise_(.dots = list(N = 'n()',
                              avg = paste0('mean(', value_var, ')')))
  }
  
  
  
  p = plot_bump(df2, time_var = time_var, value_var = 'avg', region_var = region_var, facet_var = facet_var,
            sort_by = sort_by, sort_desc = sort_desc, line_stroke = line_stroke, dot_size = dot_size, dot_shape = dot_shape, 
            label_size = label_size, label_x_offset = label_x_offset, value_y_offset = value_y_offset, 
            label_vals = label_vals, percent_vals = percent_vals, tufte_style = tufte_style, x_buffer = x_buffer, 
            font_normal =  font_normal, font_semi = font_semi, font_light = font_light, panel_spacing = panel_spacing, 
            font_axis_label = font_axis_label, font_axis_title = font_axis_title, font_facet = font_facet, 
            font_legend_title = font_legend_title, font_legend_label = font_legend_label, font_subtitle = font_subtitle, font_title = font_title, 
            grey_background = grey_background, background_colour = background_colour, projector = projector)
  
  
  return(list('avg_data' = df2, 'bump' = p))
  
}