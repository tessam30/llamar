#' Plots a dot plot 
#' 
#' @export
plot_dotplot = function(df,
                        time_var = 'year',
                        region_var = 'region',
                        facet_var = NA,
                        value_var = 'avg',
                        sort_desc = TRUE,
                        
                        arrow_arg = arrow(length = unit(0.03, "npc")),
                        dot_size = 6, 
                        connector_stroke = 0.25,
                        connector_colour = grey75K,
                        
                        label_vals = TRUE,
                        label_colour = grey75K,
                        percent_vals = FALSE,
                        value_y_offset = 0.05,
                        
                        sort_by = 'last', # options: first, last, none, diff
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
  if(is.na(arrow)) {
    warning('arrow should be either an arrow object or NULL.  Switching to NULL')
    arrow = NULL
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
        arrange_(paste0('desc(', sort_var, ')'))
    } else{
      facet_order = facet_order %>% 
        arrange_(sort_var)
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
    geom_segment(aes_string(x = 'time1', xend  = 'diff * 0.9 + time1',
                            y = region_var, yend = region_var),
                 size = connector_stroke,
                 arrow = arrow_arg,
                 colour = connector_colour,
                 data = df_untidy) +
    
    geom_point(aes_string(x = 'avg', y = region_var,
                          color = paste0('as.factor(', time_var, ')'), 
                          shape = paste0('as.factor(', time_var, ')'), 
                          fill = 'avg'),
               # paste0('as.factor(', time_var, ')')),
               size = dot_size, colour = grey90K) +
    

  
    
  theme_xgrid() +
    scale_shape_manual(values = c(21, 23, 22, 24)) +
    scale_x_continuous(labels = percent) +
    scale_fill_gradientn(colours = brewer.pal(9, 'RdPu')) +
    # scale_fill_manual(values = c('2010' = 'white', '2014' = brewer.pal(9, 'Spectral')[1])) +
    theme(axis.title.x = element_blank())
  
  # -- flip coords --
  if(horiz == FALSE) {
    p = p + coord_flip()
  }
  

  # -- value labels --
  # if (label_vals == TRUE) {
  #   if(percent_vals == TRUE) {
  #     df = df %>% 
  #       mutate_(.dots = setNames(paste0('llamar::percent(', value_var, ', 0)'), 'value_label'))
  #   } else {
  #     df = df %>% 
  #       mutate_(.dots = setNames(paste0('round(', value_var, ', 1)'), 'value_label'))
  #   }
  #   
  #   p = p + 
  #     geom_text(aes(label = value_label), 
  #               size = label_size,
  #               family = font_light,
  #               nudge_y = value_y_offset,
  #               data = df)
  # }  
  
  # -- save plot --
  if(!is.na(file_name)) {
    save_plot(file_name, saveBoth = saveBoth, width = width, height = height)
  }
  
  # -- return --
  return(p)
}