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



#' @title Data Frame NA to Empty
#' @description Converts all NA's in the character and/or factor columns of a data.frame vector to empty values
#' @param x A data.frame
#' @param coltypes Character of length 1 indicating what types of columns to affect.  Acceptable values are "character", "factor", and "both".
#' @return A data.frame with the same dimensions and names as \code{x}
#' @examples
#' V1 = c(LETTERS[1:8], NA, NA)
#' V2 = c(NA, 1:8, NA)
#' V3 = factor(c(NA,rep(c("A","B"), times = 4),NA))
#' ex1 = data.frame(V1 = V1, V2 = V2, V3 = V3, stringsAsFactors = F)
#' DFna.to.empty(ex1)
DFna.to.empty = function(x, coltypes = "both"){
  for(i in 1:ncol(x)){                # for each column
    y = unlist(x[,i])                 # get a vector of the current column
    if(is.character(y)){              # if the column is character
      y = na.to.empty(y)              # run na.to.empty
    } else if(is.factor(y)){          # if y is a factor variable
      yLevels = levels(y)             # grab the levels of y
      if(any(is.na(y))){              # if y has any NA's
        if(!("" %in% yLevels)){       # if y needs a blank level
          levels(y) = c(yLevels, "")  # add a blank level to y
        } # /if y needs a blank level
        y[which(is.na(y))] = ""       # make NA's blank
      }
    } # /if y is factor
    x[,i] = y                       # load the values back in the data.frame
  } # /for each column
  return(x)                         # return the data.frame
} # /function
