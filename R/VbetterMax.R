#' @title Vector Better Max
#' @description Find the element by element max of two vectors
#' @param x A numeric vector
#' @param y A numeric vector of the same length as \code{x} or of length 1 (if y has length 1, it will be repeated to the length of x)
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
#' VbetterMax(ex1, 7)
#' VbetterMax(ex3, 5)
#' VbetterMax(ex3, NA)
VbetterMax = function(x,y){
  if(length(x) != length(y)){
    if(length(y) == 1){
      y = rep(y, times = length(x))
    } else {
      stop("Vectors must be of equal length")
    } # /if-else
  } # /if
  apply(X = cbind(x,y), MARGIN = 1, FUN = betterMax)
}










#' @title Matrix to Vector Better Max
#' @description Collapse a 2d matrix by taking the betterMax of each row or column
#' @param X a numeric matrix
#' @param MARGIN integer of length 1 indicating whether to get the max of each row (1) or column (2)
#' @return A numeric vector where each position contains the largest value stored in that row or column of the input matrix.  If all values are NA for a particular position, that position in the output will also contain NA.
#' @examples
#' ex1 = matrix(c(1:5,NA,1:4,NA,2), 3, 4)
#' ex2 = matrix(c(5:7,NA,NA,NA), 2, 3)
#' M2VbetterMax(ex1, 1)
#' M2VbetterMax(ex1, 2)
#' M2VbetterMax(ex2, 1)
#' M2VbetterMax(ex2, 2)
M2VbetterMax = function(X, MARGIN = 1){
  if(length(dim(X)) != 2){
    stop("X must be a 2d matrix")
  } # /if
  if(!(MARGIN %in% c(1,2))){
    stop("MARGIN must be either 1 or 2")
  }
  apply(X = X, MARGIN = MARGIN, FUN = betterMax)
}
