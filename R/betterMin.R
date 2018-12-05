#' @title Better Min
#' @description Find the minimum value of a object that may contain NA's.
#' @param x An object such as a numeric vector, matrix, or data.frame
#' @return The largest value in \code{x}, if any.  Otherwise, NA.
#' @examples
#' betterMin(1:5)
#' betterMin(c(2,3,NA,7))
#' betterMin(c(NA, NA, NA))
#' ex1 = data.frame(X1 = LETTERS, X2 = 1:26, X3 = sample(c(20,NA), 26, T))
#' betterMin(ex1)
betterMin = function(x){
  if(is.data.frame(x)){
    ret = betterMin.data.frame(x)
  } else {
    y = x[!is.na(x)]
    if(length(y)>0){
      ret = min(y)
    } else {
      ret = NA
    } # /if-else there are any non-NA values
  } # /if-else it's a data.frame
  return(ret)
} # /function



betterMin.data.frame = function(x){
  x = as.data.frame(x)
  for(i in 1:ncol(x)){
    x[,i] = as.numeric(x[,i])
  }
  y = betterMin(unlist(lapply(X = x, FUN = betterMin)))
  return(y)
} # /function
