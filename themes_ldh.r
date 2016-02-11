

# recode variables --------------------------------------------------------
# mutate with if/else dictionary function
source("~/GitHub/Ethiopia/R/code2Cat.r")

convert01 = function(varName) {
  paste0('ifelse(', varName, ' == 2, 0, 
                  ifelse(', varName, '== 1, 1, NA))')
  
  # mutate_(.dots= setNames(list(convert01('w4_04')), ''))
}



# clustering function, from dr. essam -------------------------------------
cl   <- function(dat,fm, cluster){
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2, function(x) tapply(x, cluster, sum));
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL) 
}



# formatting numbers ------------------------------------------------------
roundMean = function(x) {
  round(mean(x, na.rm = TRUE), 2)
}

roundStd = function(x) {
  round(sd(x, na.rm = TRUE), 2)
}

percent = function(x, ndigits = 1) {
  paste0(sprintf("%.f", round(x*100, ndigits)), "%")
}



# clean up environment ----------------------------------------------------
rmExcept = function(x) {
  # x must be a string or a list of strings which encode the var names.
  rm(list=setdiff(ls(), x))
}



# remove attributes from dta import ---------------------------------------

removeAttributes <- function (data) {
  data <- lapply(data, function(x) {attr(x, 'labels') <- NULL; x})
  data <- lapply(data, function(x) {attr(x, 'label') <- NULL; x})
  data <- lapply(data, function(x) {attr(x, 'class') <- NULL; x})
  data <- lapply(data, function(x) {attr(x, 'levels') <- NULL; x})
  data = data.frame(data)
}

pullAttributes <- function (data) {
  label = lapply(data, function(x) attr(x, 'label'))
  
  label = data.frame(label)
  # labels = lapply(data, function(x) attr(x, 'labels'))
  
  # attrs = data.frame(label = label, labels = labels)
}




# themes ------------------------------------------------------------------





