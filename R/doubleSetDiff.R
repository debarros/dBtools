#' @title Double Set Difference
#' @description Compare two sets and return a vector of all items that appear in exactly one of them
#' @param x a vector
#' @param y A vector of the same type as /code{x}
#' @return a vector containing all of the unique elements that are members of x or y, but not of both
doubleSetDiff = function(x,y){
  xy = setdiff(x,y)
  yx = setdiff(y,x)
  difference = union(xy,yx)
  return(difference)
} # /function
