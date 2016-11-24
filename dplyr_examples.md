# dyplr programming examples

##mutate_
`pt_est = pt_est %>%
      mutate_(.dots = setNames(var, 'avg'))`
      
` df = df %>% 
    mutate_(.dots = setNames(
      list(paste0('forcats::fct_infreq(
                  factor(', var, ',',
                  'levels = ', list(codebk$code), ',',
                  'labels = ', list(codebk$names),'))'
                  )), new_var 
      ))`
  