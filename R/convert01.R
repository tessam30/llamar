#' Recoding and cleaning of survey data
#' 
#' Function to convert survey data where 2 is no and 1 is yes.
#' 
#' @param df to mutate.
#' @param varName variable to convert 1's and 2's to 1's and 0's
#' @param newVar variable name of new variable
#' @export
#' 
#' @examples
#' df = data.frame(x = c(2, 1, 9, NA), y = sample(100, 4))
#' 
#' df %>% convert01(varName = 'x', newVar = 'x2')
#' 
convert01 = function(df, varName, newVar) {
  # Check if is 2, 1, or another value.
  df2 = df %>% 
    mutate_(.dots = setNames(paste0("ifelse(", varName, " == 2, 0, 
         ifelse(", varName, "== 1, 1, NA))"), newVar))
  
  return(df2)
}
