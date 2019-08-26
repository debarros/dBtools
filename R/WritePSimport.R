# WritePSimport.R

#' @title Write PowerSchool Import File
#' @description This is a wrapper for write.table that produces a file formatted to be imported into PowerSchool
#' @param x the object to be written, preferably a matrix or data frame. If not, it is attempted to coerce x to a data frame.
#' @param file a character string naming a file
#' @param type either "quick" (if using the quick import function) or "manager" (if using the data import manager).  Defaults to "quick".
#' @param messageLevel integer of length 1 indicating the level of messaging to print
#' @return Nothing
write.psimport <- function (x, file, type = "quick", messageLevel = 0) {

  # Error Handling
  if(!(is.matrix(x) | is.data.frame(x))){
    if(length(x) != 1){
      stop("x must have length 1")
    }
  }
  if(!is.character(file)){
    stop("file must be of type character")
  }
  if(length(file) != 1){
    stop("file must have length 1")
  }
  if(type == "quick"){
    eol = "\r\n"
  } else if(type == "manager"){
    eol = "\n"
  } else {
    stop("type must be either 'quick' or 'manager'")
  }
  # /Error Handling

  write.table(x = x, file = file, quote = F, sep = "\t", eol = eol, na = "", row.names = F)

} # /function



