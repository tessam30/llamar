#' Access Demographic & Health Surveys Data
#' 
#' Accesses information from the Demographic and Health Surveys, via their API.
#'
#' Information is the same as from the DHS StatCompiler
#' @param breakdown one of 'national', 'subnational', 'background', or 'all'

#' @export
#' @examples
#' 
#' 
#' 

orderLevels = function(df, orderVar, factorVar, filtering = FALSE, filterVar = NA, filterValue = NA, 
    descending = TRUE) {
    
    # Arrange the data by var
    orderVars = df %>% # filter_(filterVar == filterValue) %>%
    ungroup() %>% arrange_(orderVar)
    
    orderVars = orderVars[factorVar]
    
    
    # Check if there are duplicate levels
    orderVars = data.frame(lapply(orderVars, as.character), stringsAsFactors = FALSE)
    orderVars = unique(orderVars)
    
    df[factorVar] = factor(df[factorVar], levels = orderVars$Species)
    
}
