#' @title Vector Better Min
#' @description Find the element by element min of two vectors
#' @param x A numeric vector
#' @param y A numeric vector of the same length as \code{x} or of length 1 (if y has length 1, it will be repeated to the length of x)
#' @return A numeric vector where each position is the smallest value stored in those positions of the input vectors.  If both input vectors have NA for a particular position, that position in the output will also contain NA.
#' @examples
#' ex1 = 6:10
#' ex2 = (1:5)*3
#' ex3 = c(1, 10, NA, 6, 2)
#' ex4 = c(NA, NA, NA, NA, 5)
#' VbetterMin(ex1, ex2)
#' VbetterMin(ex1, ex3)
#' VbetterMin(ex1, ex4)
#' VbetterMin(ex2, ex3)
#' VbetterMin(ex2, ex4)
#' VbetterMin(ex3, ex4)
#' VbetterMin(ex1, 7)
#' VbetterMin(ex3, 5)
#' VbetterMin(ex3, NA)
VbetterMin = function(x,y){
  if(length(x) != length(y)){
    if(length(y) == 1){
      y = rep(y, times = length(x))
    } else {
      stop("Vectors must be of equal length")
    } # /if-else
  } # /if
  apply(X = cbind(x,y), MARGIN = 1, FUN = betterMin)
} # /function
