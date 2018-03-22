#' @title Matrix Better Comparison
#' @description Test two matrices for equality, where NA entries are considered equal
#' @param x A numeric matrix
#' @param y A numeric matrix with the same dimensions as \code{x}
#' @return a logical matrix of the same dimensions as the inputs, showing TRUE if the corresponding input entries have equal values (where NA entries are considered equal and less than -Inf), otherwise FALSE.
#' @examples
#' m1 = matrix(data = c(1:12, 1:12), nrow = 6)
#' m2 = matrix(1:24, nrow = 6)
#' m3 = matrix(c(1:6, rep(NA, 12), 11:6), nrow = 6)
#' m4 = matrix(c(rep(NA, 12), 1:12), nrow = 6)
#' MbetterComp(m1, m2)
#' MbetterComp(m1, m3)
#' MbetterComp(m1, m4)
#' MbetterComp(m2, m3)
#' MbetterComp(m2, m4)
#' MbetterComp(m3, m4)
MbetterComp = function(x,y){
  if(!(identical(dim(x), dim(y)))){
    stop("Matrices must have the same dimensions.")
  }
  apply(X = abind::abind(x,y,along = 3), MARGIN = c(1,2), FUN = betterComp)
} # /function
