#' Plot a dot plot, either with or without lollipop sticks
#' 
#' Creates a dot plot for a single grouping variable. Can make a simple dot plot or a lollipop plot
#' 
# @import ggplot2 forcats RColorBrewer
#' 
#' @param df data frame containing all the data.
#' @param by_var string containing the variable to put along the y-axis.
#' @param value_var string containing the variable to plot on the x-axis (and to use for the color values)
#' @param incl_x_axis TRUE/FALSE of whether to include x-axis lables
#' @param x_label (optional) string containing the label for the x-axis
#' @param x_limits tuple specifying x-axis limits
#' @param x_breaks list containing break point values for the x-axis.
#' @param sort_by string containing column name within df to sort the values by
#' @param sort_asc TRUE/FALSE of whether to sort the values in ascending or descending order
#' @param plot_ci TRUE/FALSE of whether to plot segments underneath the dots containing the confidence interval limits
#' @param lb_var string name within df containing the lower limit of confidence interval
#' @param ub_var string name within df containing the upper limit of confidence interval
#' @param ci_colour color for confidence intervals
#' @param ci_alpha alpha (opacity) for confidence intervals
#' @param ci_size thickness of confidence interval segment (in points)
#' @param ref_line TRUE/FALSE or number. If false, no reference line will be included.  If true, sample average will be calculated. Be careful: if there are a different number of observations in each observation, your average will be off. If a number, will include a reference line at that location.
#' @param ref_text what to label the reference line with
#' @param label_ref_val TRUE/FALSE. Whether to label the reference line with its value.
#' @param nudge_ref_label value to offset the reference line label from the line
#' @param ref_label_y  reference label y-position
#' @param ref_arrow size of arrow to connect the reference label to the line. Specified by arrow(...)
#' @param ref_stroke stroke size of the reference line
#' @param ref_colour colour of reference line
#' @param lollipop TRUE/FALSE of whether to plot as a lollipop plot (dots connected to 'lollipop_ref' by a line)
#' @param lollipop_ref string of a numeric value to connect the dots to in lollipop plot; '0' by default
#' @param lollipop_stroke lollipop segment stroke size
#' @param lollipop_colour lollipop colour
#' @param facet_var string with variable name in df to facet by.  NULL (no facetting) by default
#' @param ncol number of columns in facetting
#' @param nrow number of rows in facetting
#' @param scales scales wtihin facetting. One of 'fixed', 'free', 'free_y', 'free_x'
#' @param dot_size size of dots (in mm) 
#' @param dot_shape shape of dots (normal ggplot2 values for shapes)
#' @param dot_fill_cont colour palette for fill of dots. Can also be a single color.
#' @param dot_fill_limits limits for color fill palette
#' @param label_vals TRUE/FALSE of whether to directly label the dots with their value. if include CIs, value labels are directly on top of dots.
#' @param label_size size of the value labels (in mm)
#' @param label_colour colour of value labels
#' @param label_digits number of digits to include in value labels
#' @param percent_vals TRUE/FALSE of whether to turn value labels into percents.
#' @param value_label_offset numeric x-offset for value labels from their dot.
#' @param sat_threshold if include CIs, numeric break point for when numbers switch from being white text to dark grey. Based on the saturation values of the underlying color.
#' @param horiz = TRUE/FALSE of whether to plot horizontally or vertically.
#' @param file_name if not NULL, string containing the file name or path to save the plot to (as either .pdf or .png)
#' @param width = width (in inches) of exported plot
#' @param height = height (in inches) of exported plot
#' @param saveBoth TRUE/FALSE if should save both .png and .pdf. If TRUE, don't include file extension in 'file_name'
#' @param font_normal string containing font name for boldest text.
#' @param font_semi string containing font name for medium bold text.
#' @param font_light string containing font name for lightest text.
#' @param panel_spacing panel spacing, in lines, between facet panels
#' @param font_axis_label size (in points) of axis labels
#' @param font_axis_title size (in points) of axis title
#' @param font_facet size (in points) of axis facet labels
#' @param font_legend_title size (in points) of legend title 
#' @param font_legend_label size (in points) of legend text
#' @param font_subtitle size (in points) of plot subtitle
#' @param font_title size (in points) of plot title
#' @param legend.position where to put legend. Unplotted, by default
#' @param legend.direction 'horizontal' or 'vertical' placement of legend
#' @param grey_background TRUE/FALSE of whether to color the area outside the plot
#' @param background_colour (optional) colour of the background
#' @param projector if TRUE, make text and axis lines bolder for use with a projector
#' 
#' @examples 
#' # generate random data
#' library(dplyr)
#' df = data.frame(avg = sample(1:100, 10), region = letters[1:10], ci = sample(1:100, 10)/10) %>% mutate(lb = avg - ci, ub = avg + ci)
#'
#' # sans confidence intervals
#' plot_dot(df, by_var = 'region', value_var = 'avg')
#' 
#' # remove sorting
#' plot_dot(df, by_var = 'region', value_var = 'avg', sort_by = 'region')
#' 
#' # with confidence intervals, no labels
#' plot_dot(df, by_var = 'region', value_var = 'avg', plot_ci = TRUE, label_vals = FALSE)
#' 
#' # with confidence intervals and labels
#' plot_dot(df, by_var = 'region', value_var = 'avg', plot_ci = TRUE)

#' # adjusting breakpoint for text value labels on top of dots.
#' plot_dot(df, by_var = 'region', value_var = 'avg', plot_ci = TRUE, sat_threshold = 0.2)
#' 
#' # as lollipops
#' df2 = data.frame(avg = sample(-100:100, 10)/100, region = letters[1:10], ci = sample(1:100, 20)/1000) %>% mutate(lb = avg - ci, ub = avg + ci)
#' library(RColorBrewer)
#' plot_dot(df2, by_var = 'region', value_var = 'avg', lollipop = TRUE, dot_fill_cont = brewer.pal(10, 'RdYlBu'))
#' 
#' # change the lollipop reference point to 0.25
#' plot_dot(df2, by_var = 'region', value_var = 'avg', lollipop = TRUE, dot_fill_cont = brewer.pal(10, 'RdYlBu'), lollipop_ref = '0.25')
#' 
#' # percent labels
#' plot_dot(df2, by_var = 'region', value_var = 'avg', percent_vals = TRUE, lollipop = TRUE, dot_fill_cont = brewer.pal(10, 'RdYlBu'))
#'
#' # with reference line
#' plot_dot(df2, by_var = 'region', value_var = 'avg', ref_line = 0, ref_text = 'no change', label_ref = FALSE, lollipop = TRUE, dot_fill_cont = brewer.pal(10, 'RdYlBu'), percent_vals = TRUE)
#'
#' # horizontal
#' plot_dot(df2, by_var = 'region', value_var = 'avg', horiz = FALSE, ref_line = 0, ref_text = 'no change', label_ref = FALSE, lollipop = TRUE, plot_ci = TRUE, dot_fill_cont = brewer.pal(10, 'RdYlBu'))
#'
#' # in-built facet_wrap. Note: may screw up ordering, since will sort based on ALL the data.
#' df3 = data.frame(avg = sample(-100:100, 20), region = rep(letters[1:10], 2), group = c(rep('group1', 10), rep('group2', 10)))
#' plot_dot(df3, by_var = 'region', value_var = 'avg', facet_var = 'group', lollipop = TRUE, dot_fill_cont = brewer.pal(10, 'RdYlBu'))
#'
#'

plot_dot = function(df,
                    by_var = 'region',
                    value_var = 'avg',
                    
                    incl_x_axis = TRUE,
                    x_label = NULL,
                    x_limits = NULL,
                    x_breaks = waiver(),
                    
                    sort_asc = FALSE,
                    sort_by = 'avg', # a column within df
                    
                    plot_ci = FALSE,
                    lb_var = 'lb',
                    ub_var = 'ub',
                    ci_colour = grey25K,
                    ci_alpha = 0.6,
                    ci_size = 2,
                    
                    ref_line = FALSE,
                    ref_text = 'sample average',
                    label_ref_val = TRUE,
                    nudge_ref_label = 0.05 * diff(range(abs(df[[value_var]]))),
                    ref_label_y = 1, # reference label y-position
                    ref_arrow = arrow(length = unit(0.007, "npc")),
                    ref_stroke = 0.5,
                    ref_colour = grey75K,
                    
                    lollipop = FALSE,
                    lollipop_ref = '0',
                    lollipop_stroke = 0.25,
                    lollipop_colour = grey75K,
                    
                    facet_var = NULL,
                    ncol = NULL,
                    nrow = NULL,
                    scales = 'fixed',
                    
                    dot_size = 6, 
                    dot_shape = 21,
                    dot_fill_cont = brewer.pal(9, 'YlGnBu'),
                    dot_fill_limits = c(min(df[[value_var]]), max(df[[value_var]])),
                    
                    label_vals = TRUE,
                    label_size = 3,
                    label_colour = grey75K,
                    label_digits = 1,
                    percent_vals = FALSE,
                    value_label_offset = 0.05 * diff(range(abs(df[[value_var]]))),
                    sat_threshold = 0.5,
                    
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
  
  
  # check inputs ----------------------------------------------------------
  if(plot_ci == TRUE) {
    if(!lb_var %in% colnames(df)) {
      stop('lb_var not found in df. Turn off plot_ci or fix inputs.')
    }
    
    if(!ub_var %in% colnames(df)) {
      stop('ub_var not found in df. Turn off plot_ci or fix inputs.')
    }
  }
  
  
  
  # if(incl_x_axis == TRUE & is.null(x_limits)) {
  #   x_limits = range(df[[value_var]])
  # }

  # if(incl_x_axis == TRUE & is.null(x_breaks)) {
  #   x_breaks = pretty(range(df[[value_var]]))
  # }
  
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
  
  if(is.logical(ref_line) & ref_line == FALSE){
    p = ggplot()
  } else {
    if(ref_line == TRUE) {
      warning('Averaging all the values within df. If each value contains a different sample size, specify a numeric "ref_line')
      ref_line = mean(df[[value_var]])
    }
    p = ggplot() + 
      geom_vline(xintercept = ref_line,
                 size = ref_stroke,
                 colour = ref_colour)
  }
  
  # add in CIs ----------------------------------------------------------
  if(plot_ci == TRUE) {
    p = p + 
      geom_segment(aes_string(x = lb_var, xend = ub_var,
                              y = y_var, yend = y_var),
                   data = df,
                   colour = ci_colour,
                   alpha = ci_alpha,
                   size = ci_size
      )
  }
  
  # lollipops ----------------------------------------------------------
  # -- add in lollipop lines to 0, if TRUE --
  if(lollipop == TRUE) {
    p = p +
      geom_segment(aes_string(x = value_var, xend = lollipop_ref, y = y_var, yend = y_var),
                   colour = lollipop_colour, size = lollipop_stroke, 
                   data = df)
  }
  
  # plot dots (MAIN PLOT) ----------------------------------------------------------
  p = p +
    geom_point(aes_string(x = value_var, 
                          y = y_var,
                          fill = value_var),
               data = df,
               size = dot_size, shape = dot_shape, colour = grey90K, stroke = 0.1) +
    scale_fill_gradientn(colours = dot_fill_cont, limits = dot_fill_limits) +
    xlab(x_label) +
    theme_xgrid(font_normal = font_normal, font_semi = font_semi,
                font_light = font_light, legend.position = legend.position,
                legend.direction = legend.direction, panel_spacing = panel_spacing,
                font_axis_label = font_axis_label, font_axis_title = font_axis_title, 
                font_facet = font_facet, font_legend_title = font_legend_title, 
                font_legend_label = font_legend_label, font_subtitle = font_subtitle, 
                font_title = font_title, grey_background = grey_background, 
                background_colour = background_colour, projector = projector
    )
  
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
    
    if(plot_ci == TRUE) {
      df = map_colour_text(df, value_var, colour_palette = dot_fill_cont, sat_threshold = sat_threshold)
      
      p = p +
        geom_text(aes_string(x = value_var, 
                             y = by_var,
                             colour = 'text_colour',
                             label = 'value_label'),
                  size = label_size,
                  family = font_light,
                  nudge_x = 0,
                  data = df) +
        scale_colour_identity()
      
    } else if(any(df[[value_var]] < 0)){
      # negative numbers
      
      p = p +
        geom_text(aes_string(x = value_var, 
                             y = by_var,
                             label = 'value_label'),
                  size = label_size,
                  family = font_light,
                  nudge_x = -1 *value_label_offset,
                  colour = label_colour,
                  data = df %>% filter_(paste0(value_var, ' < 0'))) +
        geom_text(aes_string(x = value_var, 
                             y = by_var,
                             label = 'value_label'),
                  size = label_size,
                  family = font_light,
                  nudge_x = value_label_offset,
                  colour = label_colour,
                  data = df %>% filter_(paste0(value_var, ' >= 0')))
      
      
    } else {
      p = p +
        geom_text(aes_string(x = value_var, 
                             y = by_var,
                             label = 'value_label'),
                  size = label_size,
                  family = font_light,
                  nudge_x = value_label_offset,
                  colour = label_colour,
                  data = df) 
    }
  }
  
  # -- ref line annotation. --
  # must be done after the rest so order of the y-axis isn't mucked up
  if(!is.logical(ref_line)){
    if(!is.null(ref_text)) {
      p = p +
        annotate(geom = 'text', x = ref_line + nudge_ref_label, y = ref_label_y,
                 label = ref_text, hjust = 0,
                 colour = label_colour, size = label_size, family = font_light) +
        geom_curve(aes(x = ref_line + nudge_ref_label * 0.8, xend = ref_line,
                       y = ref_label_y, yend = ref_label_y + 0.1),
                   colour = ref_colour,
                   size = ref_stroke / 2,
                   arrow = ref_arrow, 
                   data = data.frame()) # empty data frame, since manually coded
    }
    
    if(label_ref_val == TRUE & percent_vals == TRUE) {
      p = p +
        annotate(geom = 'text', x = ref_line + nudge_ref_label, y = ref_label_y - 0.2,
                 label = llamar::percent(ref_line, 0), hjust = 0,
                 colour = label_colour, size = label_size, family = font_light)
    } else if(label_ref_val == TRUE){
      p = p +
        annotate(geom = 'text', x = ref_line + nudge_ref_label, y = ref_label_y - 0.2,
                 label = llamar::round_exact(ref_line, label_digits), hjust = 0,
                 colour = label_colour, size = label_size, family = font_light)
    }
  }
  
  # facet wrap ----------------------------------------------------------
  if(!is.null(facet_var)) {
    p = p +
      facet_wrap(as.formula(paste0('~', facet_var)),
                 ncol = ncol, nrow = nrow,
                 scales = scales)
  }
  
  # axis----------------------------------------------------------
  if(incl_x_axis == TRUE) {
    if(percent_vals == TRUE) {
      p = p + 
        scale_x_continuous(labels = percent, limits = x_limits, breaks = x_breaks)
    } else{
      p = p + 
        scale_x_continuous(limits = x_limits, breaks = x_breaks)
    }
  } else {
    p = p +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            panel.grid.major.x = element_blank())
  }
  
  # horizontal ----------------------------------------------------------
  if(horiz != TRUE) {
    p = p +
      coord_flip() +
      ylab(x_label)
  }
  
  # save plot ----------------------------------------------------------
  if(!is.null(file_name)) {
    save_plot(file_name, saveBoth = saveBoth, width = width, height = height)
  }
  
  # return ----------------------------------------------------------
  
  return(p)
}

