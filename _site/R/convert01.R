#' Recoding and cleaning of survey data
#' 
#' Function to convert survey data where 2 is no and 1 is yes.
#' 
#' @param varName variable to convert 1's and 2's to 1's and 0's
#' @export
#' 
convert01 = function(varName) {
    # Check if
    paste0("ifelse(", varName, " == 2, 0, 
         ifelse(", varName, "== 1, 1, NA))")
}
