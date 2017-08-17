#' @title Vector Better Comparison
#' @description Compare two vectors element by element
#' @param x A numeric vector
#' @param y A numeric vector of the same length as \code{x} or of length 1 (if y has length 1, it will be repeated to the length of x)
#' @return A logical vector where each position indicates whether the values in those positions of the input vectors are equal.  NAs are considered equal.
#' @examples
#' ex1 = 6:10
#' ex2 = (1:5)*2
#' ex3 = c(1, 10, NA, 6, 10)
#' ex4 = c(2, NA, NA, 6, 5)
#' VbetterComp(ex1, ex2)
#' VbetterComp(ex1, ex3)
#' VbetterComp(ex1, ex4)
#' VbetterComp(ex2, ex3)
#' VbetterComp(ex2, ex4)
#' VbetterComp(ex3, ex4)
#' VbetterComp(ex3, NA)
#' VbetterComp(ex3, 10)
VbetterComp = function(x,y){
  if(length(x) != length(y)){
    if(length(y) == 1){
      y = rep(y, times = length(x))
    } else {
      stop("Vectors must be of equal length")
    } # /if-else
  } # /if
  apply(X = cbind(x,y), MARGIN = 1, FUN = betterComp)
} # /VbetterComp
