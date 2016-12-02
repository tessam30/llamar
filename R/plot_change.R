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
                       group_var = 'year',
                       region_var = 'region',
                       
                       plot_type = 'dot', # dot or bump/slope
                       
                       facet_var = region_var,
                       sort_by = 'diff', # one of: 'diff', 'first', 'last', 'none'
                       sort_desc = TRUE,
                       ncol = NULL,
                       nrow = NULL,
                       scales = 'fixed',
                       horiz = TRUE, 
                       
                       file_name = NA,
                       width = 10,
                       height = 6,
                       saveBoth = FALSE,
                       
                       line_stroke = 0.5,
                       dot_size = 5,
                       dot_shape = c(21,23, 22, 24),  
                       label_size = 4, 
                       label_x_offset = 0.3,
                       value_y_offset = NA,
                       
                       include_arrows = TRUE,
                       arrow_arg = arrow(length = unit(0.03, "npc")),
                       connector_length = 0.85, # fraction of total difference
                       fill_value = TRUE,
                       dot_fill_discrete = c('#D3DEED', '#3288BD'), # first year, second year tuple
                       dot_fill_cont = brewer.pal(9, 'YlGnBu'),
                       connector_stroke = 0.25,
                       connector_colour = grey75K,
                       
                       label_vals = TRUE,
                       label_colour = grey75K,
                       label_digits = 1,
                       percent_vals = FALSE,
                       value_label_offset = 0,
                       
                       label_group = TRUE,
                       label_group_size = 4,
                       group_label_offset = 0.25, 
                       
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
  
  # check in
  
  # facet variable isn't defined in the data frame 
  if(!is.na(facet_var) & !facet_var %in% colnames(df)) {
    warning('facet_var is not in df. Facetting is removed.')
    facet_var = NA
  }
  
  # Is there a data label that needs to be applied?
  if(!is.null(attr(df[[region_var]], 'labels'))) {
    df2 = llamar::factorize(df, df, region_var, region_var)
  } else{
    df2 = df
  }
  
  # -- calculate average --
  if(is.na(facet_var)){
    df2 = df2 %>% 
      filter_(paste0('!is.na(', value_var,')')) %>% 
      group_by_(group_var, region_var) %>% 
      summarise_(.dots = list(N = 'n()',
                              avg = paste0('mean(', value_var, ')')))
  } else {
    df2 = df2 %>% 
      filter_(paste0('!is.na(', value_var,')')) %>% 
      group_by_(group_var, region_var, facet_var) %>% 
      summarise_(.dots = list(N = 'n()',
                              avg = paste0('mean(', value_var, ')')))
  }
  
  
  if(plot_type == 'dot') {
    p = plot_dot_diff(df2, group_var = group_var, value_var = 'avg', region_var = region_var,
                     sort_desc = sort_desc,
                     sort_by = sort_by, 
                     facet_var = facet_var,
                     ncol = ncol,
                     nrow = nrow,
                     scales = scales,
                     include_arrows = include_arrows,
                     arrow_arg = arrow_arg,
                     connector_length = connector_length, # fraction of total difference
                     dot_size = dot_size, 
                     dot_shape = dot_shape,
                     fill_value = fill_value,
                     dot_fill_discrete = dot_fill_discrete,
                     dot_fill_cont = dot_fill_cont,
                     connector_stroke = connector_stroke,
                     connector_colour = connector_colour,
                     
                     label_vals = label_vals,
                     label_size = label_size,
                     label_colour = label_colour,
                     label_digits = label_digits,
                     percent_vals = percent_vals,
                     value_label_offset = value_label_offset,
                     
                     label_group = label_group,
                     label_group_size = label_group_size,
                     group_label_offset = group_label_offset, 
                     
                     horiz = horiz,
                     
                     file_name = file_name,
                     width = width,
                     height = height,
                     saveBoth = saveBoth,
                     
                     font_normal =  font_normal, font_semi = font_semi, font_light = font_light, panel_spacing = panel_spacing, 
                     font_axis_label = font_axis_label, font_axis_title = font_axis_title, font_facet = font_facet, 
                     font_legend_title = font_legend_title, font_legend_label = font_legend_label, font_subtitle = font_subtitle, font_title = font_title, 
                     grey_background = grey_background, background_colour = background_colour, projector = projector)
    
  } else if (plot_type %in% c('bump', 'slope')){
    p = plot_bump(df2, time_var = group_var, value_var = 'avg', region_var = region_var, facet_var = facet_var,
                  sort_by = sort_by, sort_desc = sort_desc, line_stroke = line_stroke, dot_size = dot_size, dot_shape = dot_shape, 
                  scales = scales, ncol = ncol, nrow = nrow,
                  label_size = label_size, label_x_offset = label_x_offset, value_y_offset = value_y_offset, 
                  label_vals = label_vals, percent_vals = percent_vals, tufte_style = tufte_style, x_buffer = x_buffer, 
                  font_normal =  font_normal, font_semi = font_semi, font_light = font_light, panel_spacing = panel_spacing, 
                  font_axis_label = font_axis_label, font_axis_title = font_axis_title, font_facet = font_facet, 
                  font_legend_title = font_legend_title, font_legend_label = font_legend_label, font_subtitle = font_subtitle, font_title = font_title, 
                  grey_background = grey_background, background_colour = background_colour, projector = projector)
    
  } else {
    warning('unknown plot; just returning average values')
    p = NULL
  }
  
  return(list('avg_data' = df2, 'plot' = p))
  
}