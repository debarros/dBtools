#' Test two matrices for equality, where NA entries are considered equal
#'
#' @param x A numeric matrix
#' @param y A numeric matrix
#' @return TRUE if all of the corresponding entries have equal values, where NA entries are considered equal.  Otherwise, FALSE.
MbetterComp = function(x,y){
  if(dim(x) != dim(y)){
    stop("Matrices must have the same dimensions.")
  }
  apply(X = abind(x,y,along = 3), MARGIN = c(1,2), FUN = betterComp)
}
