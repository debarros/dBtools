#' Find the maximum value of a object that may contain NA's.
#'
#' @param x An object such as a numeric vector, matrix, or data.frame
#' @return The largest value in \code{x}, if any.  Otherwise, NA.
#' @examples
#' betterMax(1:5)
#' betterMax(c(2,3,NA,7))
#' betterMax(c(NA, NA, NA))
#' ex1 = data.frame(X1 = LETTERS, X2 = 1:26, X3 = sample(c(20,NA), 26, T))
#' betterMax(ex1)
betterMax = function(x){
  if(is.data.frame(x)){
    ret = betterMax.data.frame(x)
  } else {
    y = x[!is.na(x)]
    if(length(y)>0){
      ret = max(y)
    } else {
      ret = NA
    }
  }
  return(ret)
}



betterMax.data.frame = function(x){
  x = as.data.frame(x)
  for(i in 1:ncol(x)){
    x[,i] = as.numeric(x[,i])
  }
  y = betterMax(unlist(lapply(X = x, FUN = betterMax)))
  return(y)
}
