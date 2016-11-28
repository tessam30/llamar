#' Plot a dot plot, either with or without axis bars
#' 
#' @examples 
#' # generate random data
#' df = data.frame(value = sample(1:100, 8), region = letters[1:8])
#' 
#' plot_dot(df, by_var = 'region', value_var = 'value')

#' 
#' @export

plot_dot = function(df,
                    by_var = 'region',
                    value_var = 'avg',
                    use_weights = TRUE,
                    
                    
                    
                    sort_asc = TRUE,
                    sort_by = 'avg', # a column within df
                    
                    plot_ci = TRUE,
                    lb_var = 'lb',
                    ub_var = 'ub',
                    ci_colour = grey15K,
                    ci_size = 1,
                    
                    reference_line = TRUE,
                    line_stroke = 0.25,
                    line_colour = grey75K,
                    
                    lollipop = FALSE,
                    
                    facet_var = NA,
                    ncol = NULL,
                    nrow = NULL,
                    scales = 'fixed',
                    
                    dot_size = 6, 
                    dot_shape = 21,
                    dot_fill_cont = brewer.pal(9, 'YlGnBu'),
                    
                    label_vals = TRUE,
                    label_size = 3,
                    label_colour = grey75K,
                    label_digits = 1,
                    percent_vals = FALSE,
                    value_label_offset = 0,
                    
                    horiz = TRUE,
                    
                    file_name = NA,
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
                    projector = FALSE){
  
  
  # -- calculate the average, by a particular variable --
  # df_avgs = calcPtEst(df2, value_var, by_var = by_var, use_weights = use_weights)
  
  # -- calculate the sample mean --
  if (reference_line == TRUE) {
    # avg_val = calcPtEst(df, value_var, use_weights = use_weights)
    
    # reference_line = avg_val$avg
  }
  
  # determine sorting ----------------------------------------------------------
  if(!is.na(sort_by) | !is.null(sort_by)) {
    if(sort_by %in% colnames(df))
      if(is.numeric(df[[sort_by]])) {
        y_var = paste0('forcats::fct_reorder(', by_var, ',', 
                     sort_by, ', .desc = ', sort_asc, ')')
      } else {
        warning('sort_by column does not contain numeric data. Sorting by factor levels')
        y_var = paste0('forcats::fct_reorder(', by_var, 
                       ', as.numeric(', 
                       sort_by, '), .desc = ', sort_asc, ')')
      }
    else {
      warning('sorting variable `sort_by` is not in df. No sorting will be applied.')
      y_var = by_var
    }
  } else{
    y_var = by_var
  }
  # add the reference line ----------------------------------------------------------
  if(reference_line != FALSE){
    p = ggplot() + 
      geom_vline(xintercept = reference_line,
                 size = line_stroke,
                 colour = line_colour) +
      annotate(geom = 'text', x = reference_line * 1.1, y = 1, 
               label = 'sample average', hjust = 0, 
               colour = label_colour, size = label_size, family = font_light)
  } else {
    p = ggplot()
  }
  
  # add in CIs ----------------------------------------------------------
  if(plot_ci == TRUE) {
    p = p + 
      geom_segment(aes_string(x = lb_var, xend = ub_var,
                              y = y_var, yend = y_var),
                   data = df_avgs,
                   colour = ci_colour,
                   size = ci_size
      )
  }
  
  # lollipops ----------------------------------------------------------
  # -- add in lollipop lines to 0, if TRUE --
  if(lollipop == TRUE) {
    p = p +
      geom_segment(aes_string(x = value_var, xend = '0', y = y_var, yend = y_var),
                   colour = line_colour, size = line_stroke, 
                   data = df_avgs)
  }
  
  # plot dots ----------------------------------------------------------
  p = p +
    # geom_vline(xintercept = reference_line,
    # size = line_stroke,
    # colour = line_colour) +
    # annotate(geom = 'text', x = reference_line * 1.1, y = 1, 
    # colour = label_colour, size = label_size, family = font_light) +
    geom_point(aes_string(x = value_var, 
                          y = y_var,
                          fill = value_var),
               data = df_avgs,
               size = dot_size, shape = dot_shape, colour = grey90K, stroke = 0.1) +
    scale_fill_gradientn(colours = dot_fill_cont) +
    theme_xgrid()
  

  # save plot ----------------------------------------------------------
  if(!is.na(file_name)) {
    save_plot(file_name, saveBoth = saveBoth, width = width, height = height)
  }
  
  # return ----------------------------------------------------------
  
  return(p)
}

