#' @title Copy Clipboard
#' @description Copy text from the clipboard as-is
#' @param allowEscapes Logical, should escape characters be interpreted?
#' @param strip.white Logical, should whitespace be stripped from the beginning and end of the clipboard contents?
#' @param sep Single character by which to split the string
#' @param single Logical, should the clipboard contents be returned as a single string, no matter what?
#' @return Character vector with the clipboard contents
CopyClipboard = function(allowEscapes = F, strip.white = F, sep = ">", single = T){
  clipboard = file(description = "clipboard")
  copied = dBtools::SWSM(scan(file = clipboard, what = "character", allowEscapes = allowEscapes, strip.white = strip.white, sep = sep, quiet = T))
  close(clipboard)
  if(single){
    copied = paste0(copied, collapse = ">")
  } # /if
  return(copied)
} # /function



