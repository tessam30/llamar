#' Plot a dot plot after averaging the values
#' 
#' @examples 
#' # generate random data
#' library(dplyr)
#' df = data.frame(avg = sample(1:100, 10), region = letters[1:10], ci = sample(1:100, 10)/10) %>% mutate(lb = avg - ci, ub = avg + ci)
#'
#' # sans confidence intervals
#' plot_dot(df, by_var = 'region', value_var = 'avg')
#' 
#' # with confidence intervals, no labels
#' plot_dot(df, by_var = 'region', value_var = 'avg', plot_ci = TRUE, label_vals = FALSE)
#' 
#' # as lollipops
#' df2 = data.frame(avg = sample(-100:100, 10)/100, region = letters[1:10], ci = sample(1:100, 20)/1000) %>% mutate(lb = avg - ci, ub = avg + ci)
#' library(RColorBrewer)
#' plot_dot(df2, by_var = 'region', value_var = 'avg', lollipop = TRUE, dot_fill_cont = brewer.pal(10, 'RdYlBu'))
#' 
#' # percent labels
#' plot_dot(df2, by_var = 'region', value_var = 'avg', percent_vals = TRUE, lollipop = TRUE, dot_fill_cont = brewer.pal(10, 'RdYlBu'))
#'
#' # with reference line
#' plot_dot(df2, by_var = 'region', value_var = 'avg', ref_line = 0, ref_label = 'no change', lollipop = TRUE, dot_fill_cont = brewer.pal(10, 'RdYlBu'), percent_vals = TRUE)
#'
#' # horizontal
#' plot_dot(df2, by_var = 'region', value_var = 'avg', horiz = FALSE, ref_line = 0, ref_label = 'no change', lollipop = TRUE, plot_ci = TRUE, dot_fill_cont = brewer.pal(10, 'RdYlBu'))
#'
#' # in-built facet_wrap. Note: may screw up ordering, since will sort based on ALL the data.
#' df3 = data.frame(avg = sample(-100:100, 20), region = rep(letters[1:10], 2), group = c(rep('group1', 10), rep('group2', 10)))
#' plot_dot(df3, by_var = 'region', value_var = 'avg', facet_var = 'group', lollipop = TRUE, dot_fill_cont = brewer.pal(10, 'RdYlBu'))
#'
#'


#' 
#' @export

plot_avg_dot = function(df,
                        by_var = 'region',
                        value_var = 'avg',
                        x_label = NULL,
                        
                        use_weights = FALSE,
                        strata_var = 'strata',
                        psu_var = 'psu',
                        weight_var = 'weight',
                        na.rm = TRUE,
                        
                        sort_asc = FALSE,
                        sort_by = 'avg', # a column within df
                        
                        plot_ci = TRUE,
                        ci_factor = 2,
                        lb_var = 'lb',
                        ub_var = 'ub',
                        ci_colour = grey15K,
                        ci_size = 2,
                        
                        ref_line = TRUE,
                        ref_label = 'sample average',
                        nudge_ref_label = NULL,
                        ref_label_y = 1, # reference label y-position
                        ref_arrow = arrow(length = unit(0.007, "npc")),
                        ref_stroke = 0.5,
                        ref_colour = grey75K,
                        
                        lollipop = FALSE,
                        lollipop_stroke = 0.25,
                        lollipop_colour = grey75K,
                        
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
                        value_label_offset = NULL,
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
  
  # -- calculate the average, by a particular variable --
  avg_df = calcPtEst(df = df, var = value_var, by_var = by_var, 
                     use_weights = use_weights, na.rm = na.rm, 
                     psu_var = psu_var, strata_var = strata_var, weight_var = weight_var,
                     ci_factor = ci_factor)
  

  # -- calculate the sample mean --
  if (ref_line == TRUE) {
    avg_val = calcPtEst(df, value_var, use_weights = use_weights, na.rm = na.rm,
                        psu_var = psu_var, strata_var = strata_var, weight_var = weight_var,
                        ci_factor = ci_factor)
    
    ref_line = avg_val$avg
    
  }
  
  # reset the label location
  if(is.null(value_label_offset)) {
    value_label_offset = 0.05 * diff(range(abs(avg_df[[value_var]])))
  }
  
  if(is.null(nudge_ref_label)) {
    nudge_ref_label = value_label_offset
  }
  
  plot_dot(avg_df,
           by_var = by_var,
           value_var = 'avg',
           x_label = x_label,

           sort_asc = sort_asc,
           sort_by = sort_by, # a column within df

           plot_ci = plot_ci,
           lb_var = 'lb',
           ub_var = 'ub',
           ci_colour = ci_colour,
           ci_size = ci_size,

           ref_line = ref_line,
           ref_label = ref_line,
           nudge_ref_label = nudge_ref_label,
           ref_label_y = ref_label_y, # reference label y-position
           ref_arrow = ref_arrow,
           ref_stroke = ref_stroke,
           ref_colour = ref_colour,

           lollipop = lollipop,
           lollipop_stroke = lollipop_stroke,
           lollipop_colour = lollipop_colour,

           facet_var = facet_var,
           ncol = ncol,
           nrow = nrow,
           scales = scales,

           dot_size = dot_size,
           dot_shape = dot_shape,
           dot_fill_cont = dot_fill_cont,

           label_vals = label_vals,
           label_size = label_size,
           label_colour = label_colour,
           label_digits = label_digits,
           percent_vals = percent_vals,
           value_label_offset = value_label_offset,
           sat_threshold = sat_threshold,

           horiz = horiz,

           file_name = file_name,
           width = width,
           height = height,
           saveBoth = saveBoth,

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
           grey_background = grey_background,
           background_colour = background_colour,
           projector = projector)
}