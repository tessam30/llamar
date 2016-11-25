# Check if works w/ M/F
# redo names?

#' Plots a dot plot 
#' 
#' @examples
#' # generate random data
#' df = data.frame(year = c(rep(2007, 6), rep(2016, 6)), value = sample(1:100, 12), region = rep(letters[1:6], 2), facet = rep(c('group1', 'group2'), 6))
#' 
#' plot_dotplot(df, time_var = 'year', region_var = 'region', value_var = 'value')
#' plot_dotplot(df, time_var = 'year', region_var = 'region', value_var = 'value', include_arrows = FALSE)
#' plot_dotplot(df, time_var = 'year', region_var = 'region', value_var = 'value', sort_by = 'first', fill_value = FALSE, value_y_offset = 0.25, sort_desc = FALSE)
#' plot_dotplot(df, time_var = 'year', region_var = 'region', value_var = 'value', sort_by = 'first', fill_value = FALSE, value_y_offset = 0.25, sort_desc = FALSE)
#'  
#' @export
plot_dotplot = function(df,
                        time_var = 'year',
                        region_var = 'region',
                        value_var = 'avg',
                        
                        sort_desc = TRUE,
                        sort_by = 'diff', # one of: 'diff', 'first', 'last', 'none'
                        
                        facet_var = NA,
                        ncol = NULL,
                        nrow = NULL,
                        scales = 'fixed',
                        
                        include_arrows = TRUE,
                        arrow_arg = arrow(length = unit(0.03, "npc")),
                        connector_length = 0.85, # fraction of total difference
                        dot_size = 6, 
                        dot_shape = c(21, 23, 22, 24),
                        fill_value = TRUE,
                        dot_fill_discrete = c('#D3DEED', '#3288BD'), # first year, second year tuple
                        dot_fill_cont = brewer.pal(9, 'YlGnBu'),
                        connector_stroke = 0.25,
                        connector_colour = grey75K,
                        
                        label_vals = TRUE,
                        label_size = 3,
                        label_colour = grey75K,
                        label_digits = 1,
                        percent_vals = FALSE,
                        value_y_offset = 0,
                        
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
                        grey_background = FALSE,
                        background_colour = grey10K,
                        projector = FALSE){
  
  # -- Check inputs --
  if(!is.list(arrow_arg)){
    if(is.na(arrow)) {
      warning('arrow should be either an arrow object or NULL.  Switching to NULL')
      arrow_arg = NULL
    } else {
      warning('Provide a valid arrow argument (see function "arrow")')
    }
  }
  
  if(include_arrows == FALSE) {
    # make sure the line goes all the way to the dot
    connector_length = 1
    arrow_arg = NULL
  }
  # Assumes data come in tidy form and pre-calculated averages.
  
  # -- find latest year --
  min_time = min(df[[time_var]])
  max_time = max(df[[time_var]])
  
  # -- Spread wide for connector line / sorting --
  df_untidy =  if(is.na(facet_var)) {
    df_untidy = df %>% 
      select_(time_var, region_var, value_var) %>% 
      spread_(time_var, value_var) %>% 
      rename_('time1' = as.name(min_time),
              'time2' = as.name(max_time)) %>% 
      mutate(diff = (time2 - time1),
             pct_diff = diff/time1)
  } else {
    df_untidy = df %>% 
      select_(time_var, region_var, value_var, facet_var) %>% 
      spread_(time_var, value_var) %>% 
      rename_('time1' = as.name(min_time),
              'time2' = as.name(max_time)) %>% 
      mutate(diff = (time2 - time1),
             pct_diff = diff/time1)
  }
  
  
  # -- refactor  y-vars --
  # decide how to order the var
  if(sort_by != 'none') {
    if(sort_by == 'last') {
      facet_order = df %>% 
        filter_(paste0(time_var, '==', max_time))
      
      sort_var = value_var
      
    } else if (sort_by == 'first'){
      facet_order = df %>% 
        filter_(paste0(time_var, '==', min_time))
      
      sort_var = value_var
      
    } else if(sort_by == 'diff'){
      facet_order = df_untidy
      sort_var = 'pct_diff'
      
    } else {
      facet_order = df_untidy
      sort_var = 'pct_diff' 
      
      warning('sorting values by difference')
    }
    
    # sort ascending or descending
    if(sort_desc == TRUE) {
      facet_order = facet_order %>% 
        arrange_(sort_var)
    } else{
      facet_order = facet_order %>% 
        arrange_(paste0('desc(', sort_var, ')'))
    }
    
    # relevel
    df[[region_var]] = factor(df[[region_var]],
                              levels = facet_order[[region_var]])
    
    df_untidy[[region_var]] = factor(df_untidy[[region_var]],
                                     levels = facet_order[[region_var]])
  }
  
  
  # -- PLOT --
  p = ggplot(df) +
    
    # -- bar between dots --
    geom_segment(aes_string(x = 'time1', xend  = 'diff * connector_length + time1',
                            y = region_var, yend = region_var),
                 size = connector_stroke,
                 arrow = arrow_arg,
                 colour = connector_colour,
                 data = df_untidy) +
    
    
    theme_xgrid() +
    scale_shape_manual(values = dot_shape) +
    theme(axis.title.x = element_blank())
  
  # -- scale fill of points --
  if(fill_value == TRUE){
    p = p + 
      geom_point(aes_string(x = value_var, y = region_var,
                            color = paste0('as.factor(', time_var, ')'), 
                            shape = paste0('as.factor(', time_var, ')'), 
                            fill = value_var),
                 size = dot_size, colour = grey90K) +
      scale_fill_gradientn(colours = dot_fill_cont)
    
  } else {
    p = p + 
      geom_point(aes_string(x = value_var, y = region_var,
                            color = paste0('as.factor(', time_var, ')'), 
                            shape = paste0('as.factor(', time_var, ')'), 
                            fill = paste0('as.factor(', time_var, ')')),
                 size = dot_size, colour = grey90K) +
      scale_fill_manual(values = dot_fill_discrete)
  }
  
  # -- flip coords --
  if(horiz == FALSE) {
    p = p + coord_flip()
  }
  
  
  # -- value labels --
  if (label_vals == TRUE) {
    
    # -- calculate y-offset for labels, if needed --
    if (is.na(value_y_offset)) {
      if(is.na(facet_var)) {
        y_offset = 0.05
      } else {
        y_offset = 0.25
      }
      
      
      # set a reasonable y-offset
      value_y_offset = diff(range(df[[value_var]])) * y_offset
    }
    
    if(percent_vals == TRUE) {
      df = df %>%
        mutate_(.dots = setNames(paste0('llamar::percent(', value_var, ', 0)'), 'value_label'))
    } else {
      df = df %>%
        mutate_(.dots = setNames(paste0('llamar::round_exact(', value_var, ',', label_digits, ')'), 'value_label'))
    }
    
    
    if(value_y_offset != 0) {
      # text is above/below the dots
      p = p +
        geom_text(aes_string(x = value_var, 
                             y = region_var,
                             label = 'value_label'),
                  size = label_size,
                  family = font_light,
                  nudge_y = value_y_offset,
                  colour = grey60K,
                  data = df) 
    } else if (fill_value == TRUE) {
      # continuous variable
      p = p + 
        geom_text(aes_string(x = value_var, 
                             y = region_var,
                             label = 'value_label',
                             colour = value_var),
                  size = label_size,
                  family = font_light,
                  nudge_y = value_y_offset,
                  data = df) +
        scale_colour_text(df[[value_var]])
    } else {
      # discrete variable
      p = p +
        geom_text(aes_string(x = value_var, 
                             y = region_var,
                             label = 'value_label',
                             colour = paste0('as.factor(', time_var, ')')),
                  size = label_size,
                  family = font_light,
                  nudge_y = value_y_offset,
                  data = df) +
        scale_colour_manual(values = c(grey90K, 'white'))
    }
  }
  # -- facetting --
  # + facet, single slope graph per facet
  if(!is.na(facet_var)) {
    p = p +
      facet_wrap(as.formula(paste0('~', facet_var)),
                 ncol = ncol, nrow = nrow,
                 scales = scales)
  }
  # -- save plot --
  if(!is.na(file_name)) {
    save_plot(file_name, saveBoth = saveBoth, width = width, height = height)
  }
  
  # -- return --
  return(p)
}