#' Calculate point estimates and errors
#' 
#' Calculates point estimates and errors using the package `survey` based 
#' on sampling weights from a survey design
#'
#' 
#' @param df main data frame containing the raw data
#' @param var string containing the variable name you want to average
#' @param by_var (optional) string containing the variable name over which you want to calculate the estimates
#' @param design (optional) svydesign object containing the sample frame
#' @param psu_var (optional) if design isn't specified, string containing the primary sampling unit variable from the survey design (argument `id` in library('survey'))
#' @param strata_var (optional) if design isn't specified, string containing the strata variable from the survey design (argument `strata` in library('survey')
#' @param weight_var (optional)if design isn't specified, string containing the weights variable from the survey design (argument `weights` in library('survey')
#' @param na.rm remove NAs from the mean or not
#' @param ci_factor value to calculate confidence interval; in standard deviations. 1.96 standard deviations --> ~ 95% of the area under a normal gaussian distribution
#'
#' @author Laura Hughes, laura.d.hughes@gmail.com
#'
#' @import dplyr
#' @import survey
#' 
#' @export


# -- Wrapper to survey::svyby --
# Calculates point estimates with standard errors, weighted by sample weights
calcPtEst = function(df, # main data frame containing raw data,
                     var, # What you want to average
                     by_var = NA, # variable 
                     design = NA, # survey design object (containing sampling procedure)
                     psu_var = NA,
                     strata_var = NA,
                     weight_var = NA,
                     na.rm = TRUE,
                     ci_factor = 2) {
  
  # -- define survey design -- 
  # create the survey design object (containing sampling procedure) if not specified
  if (is.na(design)){
    # Check all the correct variables are inputted
    if(is.na(psu_var) | is.na(strata_var) | is.na(weight_var)) {
      stop('Survey design not specified')
    }
    
    design = survey::svydesign(id = as.formula(paste0('~', psu_var)), 
                       strata = as.formula(paste0('~', strata_var)), 
                       weights = as.formula(paste0('~', weight_var)), 
                       data = df)
  }
  
  # -- Calculate weighted averages --
  if(is.na(by_var)) {
    # -- just calculate the survey mean --
    pt_est = survey::svymean(as.formula(paste0('~', var)), 
                     by = as.formula(paste0('~', by_var)),
                     design = design,
                     na.rm = na.rm)
    
    # Convert to data frame, if not already
    # rename columns
    pt_est = as.data.frame(pt_est) %>% 
      rename_('se' = var) %>% 
      rename_(.dots = setNames('mean', var)) %>% 
      mutate(N = nrow(df %>%  filter_(paste0('!is.na(', var,')'))))
    
    
  } else {
    # Calculate point estimate and standard error.
    pt_est = survey::svyby(as.formula(paste0('~', var)), 
                   by = as.formula(paste0('~', by_var)),
                   design = design,
                   svymean,
                   na.rm = na.rm)
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
    if(na.rm == TRUE) {
      # Exclude missing values
      n = df %>% 
        filter_(paste0('!is.na(', var,')')) %>% 
        group_by_(by_var) %>% 
        summarise_(.dots = list(N = 'n()', 
                                unweighted_avg = paste0('mean(', var, ')')))
    } else{
      n = df %>% 
        group_by_(by_var) %>% 
        summarise_(.dots = list(N = 'n()', 
                                unweighted_avg = paste0('mean(', var, ')')))
    }
    
    # Merge the two together
    pt_est = full_join(pt_est, n)
  }
  
  return(pt_est)
}