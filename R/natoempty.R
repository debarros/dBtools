#' @title NA to Empty
#' @description Converts all NA's in a character vector to empty values
#' @param x A character vector
#' @return A numeric vector where each position is the largest value stored in those positions of the input vectors.  If both input vectors have NA for a particular position, that position in the output will also contain NA.
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
