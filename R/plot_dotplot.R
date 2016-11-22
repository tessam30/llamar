
plot_dotplot = function(df,
                        year_var = 'year',
                        group_var = 'lvdzone_name',
                        value_var = 'stunted',
                        dot_size = 6, 
                        sort_on = 'year2', # options: year1, year2, diff
                        horiz = TRUE){
  
  # Assumes data come in tidy form
  
  # -- Calculate mean values --
  df_summary = df %>% 
    filter_(paste0('!is.na(', value_var, ')')) %>% 
    group_by_(group_var, year_var) %>% 
    summarise_(.dots = list(avg = paste0('mean(', value_var, ')')))
  
  # -- Spread wide for connector line / sorting --
  df_wide = df_summary %>% 
    spread(year, avg) %>% #!!
    rename(year1 = `2010`, #!!
           year2 = `2014`) %>% 
    mutate(diff = year2 - year1) %>% 
    arrange_(sort_on)
  
  
  # -- Relevel --
  df_wide[[group_var]] = factor(df_wide[[group_var]],
                                levels = df_wide[[group_var]])
  
  df_summary[[group_var]] = factor(df_summary[[group_var]],
                                levels = df_wide[[group_var]])
  
  ggplot(df_summary) +
    geom_segment(aes_string(x = 'year1', xend  = 'diff * 0.9 + year1',
                     y = group_var, yend = group_var),
                 size = 0.5,
                 arrow = arrow(length = unit(0.03, "npc")),
                 colour = grey60K,
                 data = df_wide) +
    geom_point(aes_string(x = 'avg', y = group_var,
                   color = paste0('as.factor(', year_var, ')'), 
                   shape = paste0('as.factor(', year_var, ')'), 
                   fill = 'avg'),
                     # paste0('as.factor(', year_var, ')')),
               size = dot_size, colour = grey90K) +
    
    # CFSVA
    geom_segment(aes_string(x = 'year1', xend  = 'diff * 0.9 + year1',
                            y = group_var, yend = group_var),
                 size = 0.5,
                 arrow = arrow(length = unit(0.03, "npc")),
                 colour = grey60K,
                 data = stunting_untidy) +
    geom_point(aes_string(x = 'avg', y = group_var,
                          color = paste0('as.factor(', year_var, ')'), 
                          shape = paste0('as.factor(', year_var, ')'), 
                          fill = 'avg'),
               # paste0('as.factor(', year_var, ')')),
               size = dot_size, colour = grey90K,
               data = stunting_comb) +
    
    # geom_text(aes(x = stunting, y = group_var,
    #                color = year, shape = year, fill = year,
    #                label = percent(stunting, 0)),
    #           colour = grey75K,
    #           size = 3) +
    theme_xgrid() +
    scale_shape_manual(values = c(21, 23, 22, 24)) +
    scale_x_continuous(labels = percent) +
    scale_fill_gradientn(colours = brewer.pal(9, 'RdPu')) +
    # scale_fill_manual(values = c('2010' = 'white', '2014' = brewer.pal(9, 'Spectral')[1])) +
    theme(axis.text.y = element_text(size = 10),
          axis.title.x = element_blank())
  
}