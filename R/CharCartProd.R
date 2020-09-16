#' @title Character Cartesian Product
#' @description Create a character vector of the cartesian product of two character vectors
#' @param x A character vector
#' @param y A character vector
#' @param groupBy either 1 or 2.  Determines which variable gets grouped together in the results.
#' @return A character vector where each element is a concatenation of one element from x and one element from y
#' @examples
#' vecOne = letters[1:5]
#' vecTwo = LETTERS[6:8]
#' CharCartProd(x = vecOne, y = vecTwo)
#' CharCartProd(vecTwo, vecOne)
#' CharCartProd(vecOne, vecTwo, 2)
#' CharCartProd(vecTwo, vecOne, 2)
CharCartProd = function(x, y, groupBy = 1){
  if(groupBy == 1){
    stuff = expand.grid(x,y)
  } else if (groupBy == 2){
    stuff = expand.grid(y,x)
    stuff = stuff[,c(2,1)]
  } else {
    stop("groupBy must be either 1 or 2")
  }
  stuff = apply(stuff, 1, paste0, collapse = "")
  return(stuff)
} #/function
