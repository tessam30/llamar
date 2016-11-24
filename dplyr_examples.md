# dplyr programming examples
Notes on how to use the dplyr package in programming settings

##mutate_
creates a new variable called 'avg', equal to the values in var
`pt_est = pt_est %>%
mutate_(.dots = setNames(var, 'avg'))`

`df = df %>% 
mutate_(.dots = setNames(
list(paste0('forcats::fct_infreq(factor(', var, ',',
'levels = ', list(codebk$code), ',',
'labels = ', list(codebk$names),'))'
)), new_var))`

##summarise_
`df2 = df %>% 
group_by_(time_var, region_var) %>% 
summarise_(.dots = list(N = 'n()', 
avg = paste0('mean(', var, ')')))`