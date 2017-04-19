#' Find the maximum value of a set that may contain NA's.
#'
#' @param x A numerical vector
#' @return The largest value in \code{x}, if any.  Otherwise, NA.
#' @examples
#' betterMax(1:5)
#' betterMax(c(2,3,NA,7))
#' betterMax(c(NA, NA, NA))
betterMax = function(x){
  y = x[!is.na(x)]
  if(length(y)>0){
    return(max(y))}
  return(NA)
}
