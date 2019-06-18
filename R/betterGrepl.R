#betterGrepl.R

#### betterGrepl ####
#


#' @title Better grepl
#' @description This function behaves similarly to grepl, except that an NA pattern or NA element of \code{x} returns FALSE, but
#'   NA matched to NA returns TRUE
#' @param pattern character string containing a regular expression (or character string for fixed = TRUE) to be matched in the
#'   given character vector. Coerced by as.character to a character string if possible. If a character vector of length 2 or more
#'   is supplied, the first element is used with a warning.
#' @param x a character vector where matches are sought, or an object which can be coerced by as.character to a character vector.
#'   Long vectors are supported.
#' @param ignore.case logical.  if FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching.
#' @param fixed logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @return Logical vector of the same length as \code{x} indicating which elements had matches
#' @examples
#' x = c("Alpha", "Beta", "Bet", NA_character_, "B")
#' betterGrepl(pattern = "a", x)
#' betterGrepl(pattern = NA_character_, x)
#' betterGrepl(pattern = NA_character_, x, ignore.case = F)
#' betterGrepl("Beta", x)
#' betterGrepl("B", x)
#' betterGrepl("X", x)
betterGrepl = function(pattern, x, ignore.case = T, fixed = T){

  y = grepl(pattern, x, ignore.case = ignore.case)

  if(sum(is.na(y))>0){
    if(is.na(pattern)){
      y[is.na(x)] = TRUE
    }
    y[is.na(y)] = FALSE

    y = as.logical(y)
  }
  gc()

  return(y)
}



#### betterGrepl.any ####

#' @title Better grepl any
#' @description This function behaves similarly to betterGrepl, except that it returns only one logical value
#' @param pattern character string containing a regular expression (or character string for fixed = TRUE) to be matched in the
#'   given character vector. Coerced by as.character to a character string if possible. If a character vector of length 2 or more
#'   is supplied, the first element is used with a warning.
#' @param x a character vector where matches are sought, or an object which can be coerced by as.character to a character vector.
#'   Long vectors are supported.
#' @param ignore.case logical.  if FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching.
#' @param fixed logical. If TRUE, pattern is a string to be matched as is. Overrides all conflicting arguments.
#' @return Logical vector indicating whether any elements had matches
#' @examples
#' x = c("Alpha", "Beta", "Bet", NA_character_, "B")
#' betterGrepl.any(pattern = "a", x)
#' betterGrepl.any(pattern = NA, x)
#' betterGrepl.any(pattern = NA, x, ignore.case = F)
#' betterGrepl.any("Beta", x)
#' betterGrepl.any("beta", x)
#' betterGrepl.any("beta", x, ignore.case = F)
#' betterGrepl.any("B", x)
#' betterGrepl.any("X", x)
betterGrepl.any = function(pattern, x, ignore.case = T, fixed = T){
  y = betterGrepl(pattern = pattern, x = x, ignore.case = ignore.case, fixed = fixed)
  return(any(y))
}




#### VbetterGrepl ####

#' @title Vector Better grepl
#' @description This function behaves similarly to betterGrepl, except that it accepts a vector of \code{pattern}s
#' @param pattern character vector containing regular expressions (or character strings for fixed = TRUE) to be matched in the
#'   given character vector \code{x}. Coerced by as.character to a character string if possible.
#' @param x a character vector where matches are sought, or an object which can be coerced by as.character to a character vector.
#'   Long vectors are supported.
#' @param ignore.case logical.  if FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching.
#' @param fixed logical. If TRUE, \code{pattern}s are strings to be matched as is. Overrides all conflicting arguments.
#' @return Logical matrix with one row for each element of \code{x} and one column for each element of \code{pattern}.  The
#'   entries indicate whether a math was found between the relevant pattern and character string.
#' @examples
#' x = c("Alpha", "Beta", "Bet", NA_character_, "B")
#' patterns = c("a", NA, "b", "c", "e")
#' VbetterGrepl(pattern = patterns, x = x)
#' VbetterGrepl(pattern = patterns, x = x, ignore.case = F)
#' VbetterGrepl(pattern = patterns, x = x, fixed = F)
#' VbetterGrepl(pattern = patterns, x = x, ignore.case = F, fixed = F)
VbetterGrepl = Vectorize(FUN = betterGrepl, vectorize.args = "pattern")



#### VbetterGrepl.any ####

VbetterGrepl.any = function(pattern, x, ignore.case = T, fixed = T, collapse = "both"){

  y = VbetterGrepl(pattern = pattern, x = x, ignore.case = ignore.case, fixed = fixed)

  if(!(collapse %in% c("columns", "rows", "both"))){
    stop("The collapse argument must be one of columns, rows, or both.")
  }

  if(collapse == "columns"){
    y = apply(y, 2, any)
  }

  if(collapse == "rows"){
    y = apply(y, 1, any)
  }

  if(collapse == "rows"){
    y = any(y)
  }

  return(y)
}




