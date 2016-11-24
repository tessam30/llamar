#' Plot a bump chart / slope graph
#' 
#' Plot a bump chart / slope graph to show changes over time
#' 
#' @import ggplot2 dplyr forcats RColorBrewer
#' 
#' @param df Data frame

#' @examples
#' # generate random data
#' df = data.frame(year = c(rep(2007, 5), rep(2016, 5)), value = sample(1:100, 10), region = rep(letters[1:5], 2))
#' plot_bump(df, time_var = 'year', value_var = 'value', region_var = 'region', tufte_style = TRUE, facet_var = 'region')
#' 
#' @export
#' 
#' 

plot_bump = function(df,
                     time_var = 'year',
                     value_var = 'value',
                     region_var = 'region',
                     facet_var = NA,
                     sort_by_lastval = TRUE,
                     sort_desc = TRUE,
                     
                     line_stroke = 0.5,
                     dot_size = 5,
                     dot_shape = 21, 
                     label_size = 4, 
                     label_x_offset = 0.3,
                     value_y_offset = NA,
                     
                     label_vals = TRUE,
                     
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
  
  # facet select
  # year names -- segment deciders
  # sorting for facets
  
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
  df_untidy = df %>% 
    select_(time_var, region_var, value_var) %>% 
    spread_(time_var, value_var)
  
  
  # -- refactor facets --
  if(sort_by_lastval == TRUE) {
    if(sort_desc == TRUE) {
      facet_order = df %>% 
        filter_(paste0(time_var, '==', max_time)) %>% 
        arrange_(paste0('desc(', value_var, ')'))
    } else{
      facet_order = df %>% 
        filter_(paste0(time_var, '==', max_time)) %>% 
        arrange_(value_var)
    }
    
    df[[region_var]] = factor(df[[region_var]],
                              levels = facet_order[[region_var]])
  }
  
  # -- PLOT --
  p = ggplot(df, aes_string(x = time_var, y = value_var,
                        fill = region_var, colour = region_var, group = region_var)) +
    # -- slope lines --
    geom_segment(aes(x = 2007, xend = 2016,
                     y = `2007`, yend  = `2016`),
                 size = line_stroke, data = df_untidy) + 
    
    # -- points --
    geom_point(size = dot_size, shape = dot_shape, 
               colour = stroke_colour, stroke = dot_stroke) +
    
    # -- scales --
    scale_x_continuous(limits = c(min_time, max_time + x_buffer),
                       breaks = c(min_time, max_time)) +
    
    
    # -- theme --
    theme_ygrid(font_normal = font_normal, font_semi = font_semi, font_light = font_light,
                panel_spacing = panel_spacing, 
                grey_background = grey_background, background_colour = background_colour,
                font_axis_label = font_axis_label, font_axis_title = font_axis_title, 
                font_facet = font_facet, font_title = font_title, font_subtitle = font_subtitle) +
    theme(axis.title.y = element_blank())
  
  # -- value labels --
  if (label_vals == TRUE) {
    p = p + 
      geom_text(aes_string(label = value_var), 
                size = label_size,
                family = font_light,
                nudge_y = value_y_offset)
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
  
  # -- return --
  return(p)
}