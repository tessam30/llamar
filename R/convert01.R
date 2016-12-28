#' Recoding and cleaning of survey data
#' 
#' Function to convert survey data where 2 is no and 1 is yes.
#' 
#' @param df to mutate.
#' @param varName variable to convert 1's and 2's to 1's and 0's
#' @param newVar variable name of new variable
#' @param trueVal value(s) to be coded as 1
#' @param falseVal value(s) to be coded as 0
#' @export
#' 
#' @examples
#' df = data.frame(x = c(2, 1, 9, NA), y = sample(100, 4))
#' 
#' df %>% convert01(varName = 'x', newVar = 'x2')
#' 
#' # Change what's TRUE and FALSE
#' df %>% convert01(varName = 'x', newVar = 'x2', trueVal = 9, falseVal = '0:2')
#' df %>% convert01(varName = 'x', newVar = 'x2', trueVal = 'c(2,9)', falseVal = '1')
#' 
convert01 = function(df, 
                     varName, 
                     newVar,
                     trueVal = '1', 
                     falseVal = '2') {
  # Check if is 2, 1, or another value.
  df2 = df %>% 
    mutate_(.dots = setNames(paste0("ifelse(", varName, " %in%", falseVal, ", 0, 
         ifelse(", varName, "%in%", trueVal, ", 1, NA))"), newVar))
  
  return(df2)
}
