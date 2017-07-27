#' copy for facetting
#' 

copy4facet = function(df2copy, by_var){
  num_copies = length(by_var)
  num_rows = nrow(df2copy)
  
  df = lapply(by_var, function(x) cbind(df2copy, rep(x, num_rows)))
  
  dplyr::bind_rows(df) %>% 
    rename(by_var = `rep(x, num_rows)`)
}