#' @title Character Cartesian Product
#' @description Create a character vector of the cartesian product of two character vectors
#' @param x A character vector
#' @param y A character vector
#' @return A character vector where each element is a concatenation of one element from x and one element from y
#' @examples
#' vecOne = letters[1:5]
#' vecTwo = LETTERS[6:8]
#' CharCartProd(x = vecOne, y = vecTwo)
#' CharCartProd(vecTwo, vecOne)
CharCartProd = function(x, y){
  stuff = expand.grid(x,y)
  stuff = apply(stuff, 1, paste0, collapse = "")
  return(stuff)
}
