# ShortFunctions.R


#' @title Intersect Several
#' @description Find intersection of several sets
#' @param ... Any number of vectors of the same mode
#' @return A vector of the same mode as the inputs containing elements that appear in all of the input sets.  If there are none, it will return an empty vector.
#' @examples
#' ex1 = 6:10
#' ex2 = (1:5)*3
#' ex3 = c(1, 10, NA, 6, 2)
#' ex4 = c(NA, NA, NA, 6, 5)
#' intersectSeveral(ex1, ex2)
#' intersectSeveral(ex1, ex3)
#' intersectSeveral(ex1, ex4)
#' intersectSeveral(ex2, ex3)
#' intersectSeveral(ex2, ex4)
#' intersectSeveral(ex3, ex4)
#' intersectSeveral(ex1, 7)
#' intersectSeveral(ex3, 5)
#' intersectSeveral(ex3, NA)
intersectSeveral <- function(...) {
  Reduce(intersect, list(...))
} # /function



#' @title Set Length
#' @description Set the length of a vector
#' @param x a vector
#' @param y integer of length 0
#' @return The vector x truncated if y < length(x) or padded with NA if y > length(x)
#' @examples
#' ex1 = 6:10
#' ex2 = LETTERS[1:10]
#' ex3 = c(1, 10, NA, 6, 2)
#' ex4 = c(NA, NA, NA, 6, 5)
#' setlength(ex1, 7)
#' setlength(ex2, 5)
#' setlength(ex2, 15)
#' setlength(ex3, 4)
#' setlength(ex4, 50)
setlength <- function(x,y){
  length(x) <- y
  return(x)
} # /function




#' @title One Drop
#' @description Drops all elements of a character vector that have fewer than 2 characters
#' @param x a character vector
#' @param keep.na logical vector of length 1 indicating whether to keep NA elements
#' @return A vector containing the elements of \code{x} that are longer than 1 character
#' @examples
#' ex1 = LETTERS[1:10]
#' ex2 = paste0(c("A","B","C", "D"), c("","ABC","AB","A"))
#' ex3 = c(NA, NA, NA, "1 2 3", "6", "591", "A ")
#' one.drop(ex1)
#' one.drop(ex2)
#' one.drop(ex3)
#' one.drop(ex3, TRUE)
one.drop = function(x, keep.na = F){
  x = x[nchar(x) > 1]
  if(!keep.na){
    x = x[!is.na(x)]
  }
  return(x)
}




#' @title Sort by Length
#' @description Sort a character vector by the lengths of its elements
#' @param x a character vector
#' @param decreasing logical vector of length 1 indicating whether to keep NA elements.  Defaults to TRUE
#' @param na.last for controlling the treatment of NAs. If TRUE, missing values in the data are put last; if FALSE, they are put first; if NA, they are removed.
#' @return A vector containing the elements of \code{x} sorted by length
#' @examples
#' ex1 = c("Green", "Tech", "High", "Charter", "School")
#' ex2 = c(NA, NA, NA, "1 2 3", "6", "591", "A ")
#' SortLength(ex1)
#' SortLength(ex1, F)
#' SortLength(ex2)
#' SortLength(ex2, F)
#' SortLength(ex2, na.last = T)
#' SortLength(ex2, na.last = F)
#' SortLength(ex2, F, na.last = T)
#' SortLength(ex2, F, na.last = F)
SortLength = function(x, decreasing = TRUE, na.last = NA){
  x[sort.int(nchar(x), na.last = na.last, decreasing = decreasing, index.return = TRUE)[[2]]]
}







