# betterGreater.R

#' @title Better Greater Than
#' @description Test two values to see if one is greater, where NA is equal to NA, and NA < -Inf
#' @param x A numeric vector of length 1 or 2
#' @param y A numeric vector of length 1 (if /code{x} is atomic) or NULL
#' @return TRUE if the the first value is greater (where any number is considered greater than NA), otherwise FALSE
#' @note Instead of two arguments, this function will also accept a single vector of length 2 and compare the the two elements.
#' @examples
#' betterGreater(5, 5)
#' betterGreater(105, -23.6)
#' betterGreater(9, NA)
#' betterGreater(5, 6)
#' betterGreater(NA, 7)
#' betterGreater(NA, NA)
#' betterGreater(-Inf, NA)
#' betterGreater(NA, -Inf)
#' betterGreater(15, -Inf)
#' betterGreater(-Inf, -45)
#' valuesToCheck = c(10, 13)
#' betterGreater(x = valuesToCheck)
betterGreater = function(x, y = NULL){

  # Check the make sure the right number of values were entered
  if(length(x) + length(y) != 2){
    stop("Error!  Submit only 2 values.")
  }

  # If both values came in the first parameter, split them up
  if(is.null(y)){
    y = x[2]
    x = x[1]
  }

  # Check to make sure the values are of the correct type
  if(!(typeof(x) %in%  c("logical", "integer", "double"))){
    stop("Error!  The first parameter must be numeric.")
  }
  if(!(typeof(y) %in%  c("logical", "integer", "double"))){
    stop("Error!  The second parameter must be numeric.")
  }

  # Determine the output
  if(is.na(x)){
    ret = FALSE
  } else if(is.na(y)){
    ret = TRUE
  } else {
    ret = (x > y)
  }

  return(ret)
} # /function







#' @title Vector Better Greater Than
#' @description Compare two vectors for element by element to see if the first is greater than the second
#' @param x A numeric vector
#' @param y A numeric vector of the same length as \code{x} or of length 1 (if y has length 1, it will be repeated to the length of x)
#' @return A logical vector where each position indicates whether the values in those positions of the first input vector is greater than that of the second. NAs are considered equal to each other, and less than -Inf.
#' @examples
#' ex1 = 6:10
#' ex2 = (1:5)*2
#' ex3 = c(1, 10, NA, 6, 10)
#' ex4 = c(2, NA, NA, 6, 5)
#' VbetterGreater(ex1, ex2)
#' VbetterGreater(ex1, ex3)
#' VbetterGreater(ex1, ex4)
#' VbetterGreater(ex2, ex3)
#' VbetterGreater(ex2, ex4)
#' VbetterGreater(ex3, ex4)
#' VbetterGreater(ex3, NA)
#' VbetterGreater(ex3, 10)
VbetterGreater = function(x,y){
  if(length(x) != length(y)){
    if(length(y) == 1){
      y = rep(y, times = length(x))
    } else {
      stop("Vectors must be of equal length")
    } # /if-else
  } # /if
  apply(X = cbind(x,y), MARGIN = 1, FUN = betterGreater)
} # /function







#' @title Matrix Better Greater Than
#' @description Test two matrices element by element to see if the first is greater than the second, where NA entries are considered equal and less than -Inf
#' @param x A numeric matrix
#' @param y A numeric matrix with the same dimensions as \code{x}
#' @return a logical matrix of the same dimensions as the inputs, showing TRUE if the corresponding entry of the first matrix is greater than that of the second (where NA entries are considered equal and less than -Inf), otherwise FALSE.
#' @examples
#' m1 = matrix(data = c(1:12, 1:12), nrow = 6)
#' m2 = matrix(1:24, nrow = 6)
#' m3 = matrix(c(1:6, rep(-Inf, 12), 24:19), nrow = 6)
#' m4 = matrix(c(rep(NA, 12), 1:12), nrow = 6)
#' MbetterGreater(m2, m1)
#' MbetterGreater(m1, m3)
#' MbetterGreater(m1, m4)
#' MbetterGreater(m2, m3)
#' MbetterGreater(m2, m4)
#' MbetterGreater(m3, m4)
MbetterGreater = function(x, y){
  if(!(identical(dim(x), dim(y)))){
    stop("Matrices must have the same dimensions.")
  }
  apply(X = abind::abind(x, y, along = 3), MARGIN = c(1,2), FUN = betterGreater)
} # /function

