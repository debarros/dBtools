#' @title Better Comparison
#' @description Test two values for equality, where NA is equal to NA
#' @param x A numeric vector of length 1 or 2
#' @param y A numeric vector of length 1 (if /code{x} is atomic) or NULL
#' @return TRUE if the two values are equal or if both are NA, and FALSE if they are not equal or if exactly one is NA
#' @note Instead of two arguments, this function will also accept a single vector of length 2 and compare the the two elements.
#' @examples
#' betterComp(5, 5)
#' betterComp(9, NA)
#' betterComp(5, 6)
#' betterComp(NA, 7)
#' betterComp(NA, NA)
#' valuesToCheck = c(10, 13)
#' betterComp(x = valuesToCheck)
betterComp = function(x,y = NULL){
  if(is.null(y)){
    y = x[2]
    x = x[1]
  }
  q = is.na(x) + is.na(y)
  if(q < 1) return(x == y)
  if(q == 1) return(FALSE)
  return(TRUE)
}
