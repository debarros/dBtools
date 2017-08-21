#' Test two matrices for equality, where NA entries are considered equal
#'
#' @param x A numeric matrix
#' @param y A numeric matrix
#' @return TRUE if all of the corresponding entries have equal values, where NA entries are considered equal.  Otherwise, FALSE.
#' @examples
#' m1 = matrix(data = c(1:12,1:12), nrow = 6)
#' m2 = matrix(1:24,nrow = 6)
#' m3 = matrix(1:24,nrow = 4)
#' m4 = matrix(c(rep(NA,12),1:12)
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
  apply(X = abind(x,y,along = 3), MARGIN = c(1,2), FUN = betterComp)
}
