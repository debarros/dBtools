#betterGrep.R

#### betterGrep ####

#' @title Better grep
#' @description Search for pattern matches, where trying to match to NA comes back as FALSE
#' @param pattern character string containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector. Coerced by as.character to a character string if possible. If a character vector of length 2 or more is supplied, the first element is used with a warning.
#' @param x a character vector where matches are sought, or an object which can be coerced by as.character to a character vector. Long vectors are supported.
#' @param ignore.case logical.  if FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching.
#' @param fixed logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @return Numeric vector holding the indices of elements of \code{x} that had matches
#' @examples
#' x = c("Alpha", "Beta", "Bet", NA_character_, "B")
#' betterGrep(pattern = "a", x)
#' betterGrep(pattern = NA, x)
#' betterGrep(pattern = NA, x, ignore.case = F)
#' betterGrep("Beta", x)
#' betterGrep("B", x)
#' betterGrep("X", x)
betterGrep = function(pattern, x, ignore.case = T, fixed = T){
  y = grep(pattern, x, ignore.case = ignore.case)
  if(sum(is.na(y))>0){
    y[is.na(y)] = 0
    y = as.logical(y)
    y = which(y)
  }
  return(y)
}




#### VbetterGrep ####
VbetterGrep = Vectorize(FUN = betterGrep, vectorize.args = "pattern")
