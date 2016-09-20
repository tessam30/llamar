#' Calculate point estimates and errors
#' 
#' Calculates point estimates and errors using the package `survey` based 
#' on sampling weights from a survey design
#'

#' @author Laura Hughes, laura.d.hughes@gmail.com
#' @param 
#' @examples
#' 

#' 

#' @export
# -- Wrapper to survey::svyby --
# Calculates point estimates with standard errors, weighted by sample weights
calcPtEst = function(var, # What you want to average
                     by_var, # variable 
                     design, # survey design object (containing sampling procedure)
                     df, # main data frame containing raw data
                     omit_NA = TRUE,
                     ci_factor = 2) {
  
  # Calculate point estimate and standard error.
  pt_est = svyby(as.formula(paste0('~', var)), 
                 by = as.formula(paste0('~', by_var)),
                 design = design,
                 svymean,
                 na.rm = omit_NA)
  # Convert to data frame, if not already
  pt_est = as.data.frame(pt_est)
  
  # Calculate CI and upper and lower bounds.
  # Default is to use 95% CI (1.96)
  pt_est = pt_est %>%
    mutate_(.dots = setNames(var, 'avg')) %>% # Create a copy of the average value named 'avg'
    mutate(ci = se * ci_factor,
           ub = avg + ci,
           lb = avg - ci) %>% 
    arrange(desc(avg))
  
  # Calculate sample size and unweighted avg.
  if(omit_NA == TRUE) {
    # Exclude missing values
    n = hh %>% 
      filter_(paste0('!is.na(', var,')')) %>% 
      group_by_(by_var) %>% 
      summarise_(.dots = list(N = 'n()', 
                              unweighted_avg = paste0('mean(', var, ')')))
  } else{
    n = hh %>% 
      group_by_(by_var) %>% 
      summarise_(.dots = list(N = 'n()', 
                              unweighted_avg = paste0('mean(', var, ')')))
  }
  
  # Merge the two together
  pt_est = full_join(pt_est, n)
  
  return(pt_est)
}