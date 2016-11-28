#' Plot a dot plot, either with or without axis bars
#' 
#' @examples 
#' # generate random data
#' df = data.frame(avg = sample(1:100, 10), region = letters[1:10], ci = sample(1:100, 10)/10) %>% mutate(lb = avg - ci, ub = avg + ci)
#'
#' # sans confidence intervals
#' plot_dot(df, by_var = 'region', value_var = 'avg')
#' 
#' # with confidence intervals
#' plot_dot(df, by_var = 'region', value_var = 'avg', plot_ci = TRUE)
#' 
#' # as lollipops
#' df2 = data.frame(avg = sample(-100:100, 10), region = letters[1:10])
#' plot_dot(df2, by_var = 'region', value_var = 'avg', lollipop = TRUE, dot_fill_cont = brewer.pal(10, 'RdYlBu'))


#' 
#' @export

plot_dot = function(df,
                    by_var = 'region',
                    value_var = 'avg',
                    x_label = NULL,
                    use_weights = TRUE,
                    
                    sort_asc = FALSE,
                    sort_by = 'avg', # a column within df
                    
                    plot_ci = FALSE,
                    lb_var = 'lb',
                    ub_var = 'ub',
                    ci_colour = grey15K,
                    ci_size = 1,
                    
                    reference_line = NULL,
                    line_stroke = 0.25,
                    line_colour = grey75K,
                    
                    lollipop = FALSE,
                    
                    facet_var = NULL,
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
                    value_label_offset = 0.05 * diff(range(abs(df[[value_var]]))),
                    
                    horiz = TRUE,
                    
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
                    projector = FALSE){
  
  
  # -- calculate the average, by a particular variable --
  # df = calcPtEst(df2, value_var, by_var = by_var, use_weights = use_weights)
  
  # -- calculate the sample mean --
  # if (reference_line == TRUE) {
  # avg_val = calcPtEst(df, value_var, use_weights = use_weights)
  
  # reference_line = avg_val$avg
  # }
  
  # determine sorting ----------------------------------------------------------
  if(!is.na(sort_by) | !is.null(sort_by)) {
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
  
  # add the reference line ----------------------------------------------------------
  if(!is.null(reference_line)){
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
                   data = df,
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
                   data = df)
  }
  
  # plot dots (MAIN PLOT) ----------------------------------------------------------
  p = p +
    # geom_vline(xintercept = reference_line,
    # size = line_stroke,
    # colour = line_colour) +
    # annotate(geom = 'text', x = reference_line * 1.1, y = 1, 
    # colour = label_colour, size = label_size, family = font_light) +
    geom_point(aes_string(x = value_var, 
                          y = y_var,
                          fill = value_var),
               data = df,
               size = dot_size, shape = dot_shape, colour = grey90K, stroke = 0.1) +
    scale_fill_gradientn(colours = dot_fill_cont) +
    xlab(x_label) +
    theme_xgrid()
  
  # apply labels ----------------------------------------------------------
  
  # format
  if (label_vals == TRUE) {
    if(percent_vals == TRUE) {
      df = df %>%
        mutate_(.dots = setNames(paste0('llamar::percent(', value_var, ', 0)'), 'value_label'))
    } else {
      df = df %>%
        mutate_(.dots = setNames(paste0('llamar::round_exact(', value_var, ',', label_digits, ')'), 'value_label'))
    }
    
    if(any(df[[value_var]] < 0)){
      # negative numbers
      
      p = p +
        geom_text(aes_string(x = value_var, 
                             y = by_var,
                             label = 'value_label'),
                  size = label_size,
                  family = font_light,
                  nudge_x = -1 *value_label_offset,
                  colour = grey60K,
                  data = df %>% filter_(paste0(value_var, ' < 0'))) +
        geom_text(aes_string(x = value_var, 
                             y = by_var,
                             label = 'value_label'),
                  size = label_size,
                  family = font_light,
                  nudge_x = value_label_offset,
                  colour = grey60K,
                  data = df %>% filter_(paste0(value_var, ' >= 0')))
      
      
    } else {
    p = p +
      geom_text(aes_string(x = value_var, 
                           y = by_var,
                           label = 'value_label'),
                size = label_size,
                family = font_light,
                nudge_x = value_label_offset,
                colour = grey60K,
                data = df) 
    }
  }
  
  # save plot ----------------------------------------------------------
  if(!is.null(file_name)) {
    save_plot(file_name, saveBoth = saveBoth, width = width, height = height)
  }
  
  # return ----------------------------------------------------------
  
  return(p)
}

