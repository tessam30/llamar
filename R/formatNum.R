#' Simple reusable functions to format numbers
#'
#' @name formatNum
NULL
#> NULL
#' @export
#' 
#' @param x numbers to be converted
#' @param ndigits number of digits to which to round the number
#' 
#' @examples 
#' roundMean(mtcars$mpg)
#' roundStd(mtcars$mpg)
#' percent(0.157389)

#' @describeIn  formatNum Function to calculate the mean and round to two digits.
#' @export
roundMean = function(x, ndigits = 2) {
  round(mean(x, na.rm = TRUE), ndigits)
}

#' @describeIn  formatNum Function to calculate the standard deviation and round to two digits.
#' @export
roundStd = function(x, ndigits = 2) {
  round(sd(x, na.rm = TRUE), ndigits)
}

#' @describeIn  formatNum Function to convert a decimal to a percent, round to a specific number of digits, and convert to a string with a percent sign.
#' @export

percent = function(x, ndigits = 1) {
  paste0(sprintf("%.f", round(x*100, ndigits)), "%")
}
