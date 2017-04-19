#' @title Vector Better Comparison
#' @description Compare two vectors element by element
#' @param x A numeric vector
#' @param y A numeric vector of the same length as \code{x}
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
VbetterComp = function(x,y){
  if(length(x) != length(y)){
    stop("Vectors must be of equal length")
  }
  apply(X = cbind(x,y), MARGIN = 1, FUN = betterComp)
}
