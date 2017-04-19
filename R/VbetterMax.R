#' @title Vector Better Max
#' @description Find the element by element max of two vectors
#' @param x A numeric vector
#' @param y A numeric vector of the same length as \code{x}
#' @return A numeric vector where each position is the largest value stored in those positions of the input vectors.  If both input vectors have NA for a particular position, that position in the output will also contain NA.
#' @examples
#' ex1 = 6:10
#' ex2 = (1:5)*3
#' ex3 = c(1, 10, NA, 6, 2)
#' ex4 = c(NA, NA, NA, NA, 5)
#' VbetterMax(ex1, ex2)
#' VbetterMax(ex1, ex3)
#' VbetterMax(ex1, ex4)
#' VbetterMax(ex2, ex3)
#' VbetterMax(ex2, ex4)
#' VbetterMax(ex3, ex4)
VbetterMax = function(x,y){
  if(length(x) != length(y)){
    stop("Vectors must be of equal length")
  }
  apply(X = cbind(x,y), MARGIN = 1, FUN = betterMax)
}
