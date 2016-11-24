#' Plot a bump chart / slope graph
#' 
#' Plot a bump chart / slope graph to show changes over time
#' 
#' @import ggplot2 dplyr forcats RColorBrewer
#' 
#' @param df Data frame

#' @examples
#' # generate random data
#' df = data.frame(year = c(rep(2007, 6), rep(2016, 6)), value = sample(1:100, 12), region = rep(letters[1:6], 2), facet = rep(c('group1', 'group2'), 6))
#' plot_bump(df, time_var = 'year', value_var = 'value', region_var = 'region', tufte_style = TRUE, facet_var = 'facet')
#' plot_bump(df, time_var = 'year', value_var = 'value', region_var = 'region', facet_var = 'region', sort_desc = FALSE, sort_by = 'first')
#' plot_bump(df, time_var = 'year', value_var = 'value', region_var = 'region', facet_var = 'region', sort_desc = FALSE, sort_by = 'last')
#' plot_bump(df, time_var = 'year', value_var = 'value', region_var = 'region', facet_var = 'region', sort_desc = FALSE, sort_by = 'diff')
#' 
#' @export
#' 
#' 

plot_bump = function(df,
                     time_var = 'year',
                     value_var = 'value',
                     region_var = 'region',
                     
                     facet_var = NA,
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
  
  # -- check inputs are correct --
  if(is.numeric(df[[region_var]])) {
    df= df %>% 
      mutate_(.dots = setNames(paste0('as.factor(', region_var, ')'), region_var))
  }
  
  # -- change stroke around dots --
  if(tufte_style == TRUE) {
    dot_stroke = 2
    stroke_colour = 'white'
    
  } else {
    dot_stroke = 0.1  
    stroke_colour = grey90K
  }
  
  
  # -- calculate y-offset for labels, if needed --
  if (is.na(value_y_offset)) {
    # set a reasonable y-offset
    value_y_offset = diff(range(df[[value_var]])) * 0.05
  }
  
  
  # -- find latest year --
  min_time = min(df[[time_var]])
  max_time = max(df[[time_var]])
  
  
  # -- reshape to get the slope lines --
  if(is.na(facet_var)) {
    df_untidy = df %>% 
      select_(time_var, region_var, value_var) %>% 
      spread_(time_var, value_var) %>% 
      rename_('time1' = as.name(min_time),
              'time2' = as.name(max_time)) %>% 
      mutate(diff = (time2 - time1)/time1)
  } else {
    df_untidy = df %>% 
      select_(time_var, region_var, value_var, facet_var) %>% 
      spread_(time_var, value_var) %>% 
      rename_('time1' = as.name(min_time),
              'time2' = as.name(max_time)) %>% 
      mutate(diff = (time2 - time1)/time1)
  }
  
  # -- refactor facets --
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
      sort_var = 'diff'
      
    } else {
      facet_order = df_untidy
      sort_var = 'diff' 
      
      warning('sorting values by difference')
    }
    
    
    if(sort_desc == TRUE) {
      facet_order = facet_order %>% 
        arrange_(paste0('desc(', sort_var, ')'))
    } else{
      facet_order = facet_order %>% 
        arrange_(sort_var)
    }
    
    df[[region_var]] = factor(df[[region_var]],
                              levels = facet_order[[region_var]])
  }
  
  # -- PLOT --
  p = ggplot(df, aes_string(x = time_var, y = value_var,
                            fill = region_var, colour = region_var, group = region_var)) +
    # -- slope lines --
    geom_segment(aes(x = min_time, xend = max_time,
                     y = time1, yend  = time2),
                 size = line_stroke, data = df_untidy) + 
    
    # -- points --
    geom_point(size = dot_size, shape = dot_shape, 
               colour = stroke_colour, stroke = dot_stroke) +
    
    
    # -- theme --
    theme_ygrid(font_normal = font_normal, font_semi = font_semi, font_light = font_light,
                panel_spacing = panel_spacing, 
                grey_background = grey_background, background_colour = background_colour,
                font_axis_label = font_axis_label, font_axis_title = font_axis_title, 
                font_facet = font_facet, font_title = font_title, font_subtitle = font_subtitle) +
    theme(axis.title.y = element_blank())
  
  # -- x-scale --
  if(is.numeric(df[[time_var]])) {
    p = p + scale_x_continuous(limits = c(min_time, max_time + x_buffer),
                               breaks = c(min_time, max_time))
  } else {
    p = p + scale_x_discrete(breaks = c(min_time, max_time))
  }
  
  # -- y-scale --
  if(percent_vals == TRUE) {
    p = p + scale_y_continuous(labels = percent)
  }
  
  # -- value labels --
  if (label_vals == TRUE) {
    if(percent_vals == TRUE) {
      df = df %>% 
        mutate_(.dots = setNames(paste0('llamar::percent(', value_var, ', 0)'), 'value_label'))
    } else {
      df = df %>% 
        mutate_(.dots = setNames(paste0('round(', value_var, ', 1)'), 'value_label'))
    }
    
    p = p + 
      geom_text(aes(label = value_label), 
                size = label_size,
                family = font_light,
                nudge_y = value_y_offset,
                data = df)
  }
  
  # -- facetting --
  if(!is.na(facet_var)) {
    p = p +
      facet_wrap(as.formula(paste0('~', facet_var)))
  } else {
    p = p +
      # -- labels --
      geom_text(aes_string(label = region_var),
                size = label_size, hjust = 0, nudge_x = label_x_offset,
                family = font_light,
                data = df %>% filter_(paste0(time_var, '==', max_time))
      )
  }
  
  # -- save plot --
  if(!is.na(file_name)) {
    save_plot(file_name, saveBoth = saveBoth, width = width, height = height)
  }
  
  # -- return --
  return(p)
}

# percent axis
# discrete scale

#' @export
plot_slope = function(df,
                      time_var = 'year',
                      value_var = 'value',
                      region_var = 'region',
                      
                      facet_var = NA,
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
  plot_bump(df = df, time_var = time_var, value_var = value_var, region_var = region_var, facet_var = facet_var,
            sort_by = sort_by, sort_desc = sort_desc, line_stroke = line_stroke, dot_size = dot_size, dot_shape = dot_shape, 
            label_size = label_size, label_x_offset = label_x_offset, value_y_offset = value_y_offset, 
            label_vals = label_vals, percent_vals = percent_vals, tufte_style = tufte_style, x_buffer = x_buffer, 
            font_normal =  font_normal, font_semi = font_semi, font_light = font_light, panel_spacing = panel_spacing, 
            font_axis_label = font_axis_label, font_axis_title = font_axis_title, font_facet = font_facet, 
            font_legend_title = font_legend_title, font_legend_label = font_legend_label, font_subtitle = font_subtitle, font_title = font_title, 
            grey_background = grey_background, background_colour = background_colour, projector = projector)
}