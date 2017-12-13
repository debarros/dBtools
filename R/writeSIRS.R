#' @title Write SIRS Extract
#' @description Write a csv file in the manner needed for SIRS extracts
#' @param x the object to be written, preferably a matrix or data frame. If not, it is attempted to coerce x to a data frame.
#' @param filename a character string naming a file
#' @return nothing is returned
write.SIRS = function(x, filename){
  write.table(x = x,
              file = filename,
              row.names = F,
              col.names = F,
              sep = ",",
              dec = ".")
} # /function
