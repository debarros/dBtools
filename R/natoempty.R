#' @title NA to Empty
#' @description Converts all NA's in a character vector to empty values
#' @param x A character vector
#' @return A character vector contain all of non NA elements of \code{x}
#' @examples
#' ex1 = LETTERS[1:10]
#' ex2 = c(NA, letters, NA)
#' ex3 = c(NA, NA, NA, NA, "A")
#' na.to.empty(ex1)
#' na.to.empty(ex2)
#' na.to.empty(ex3)
na.to.empty = function(x){
  x[which(is.na(x))] = ""
  return(x)
}



