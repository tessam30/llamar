#' Simple reusable functions to format numbers
#'
#' @name formatNum
NULL
#' @param x numbers to be converted
#' @param ndigits number of digits to which to round the number
#' 
#' @examples 
#' round_mean(mtcars$mpg)
#' round_exact(2, ndigits = 3)
#' round_std(mtcars$mpg)
#' percent(0.157389)

#' @describeIn  formatNum Function to calculate the mean and round to two digits.
# @export
round_mean = function(x, ndigits = 2) {
  round(mean(x, na.rm = TRUE), ndigits)
}

#' @describeIn  formatNum Function to calculate the standard deviation and round to two digits.
# @export
round_std = function(x, ndigits = 2) {
  round(sd(x, na.rm = TRUE), ndigits)
}

#' @describeIn  formatNum Function to convert a decimal to a percent, round to a specific number of digits, and convert to a string with a percent sign.
# @export

percent = function(x, ndigits = 1) {
  paste0(sprintf(paste0("%.", ndigits, "f"), x * 100), "%")
}

#' @describeIn  formatNum Function to round a number w/ an exact number of digits. Returns a string.
# @export

round_exact = function(x, ndigits = 1) {
  sprintf(paste0("%.", ndigits, "f"), x)  
}
