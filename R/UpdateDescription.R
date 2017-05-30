#' @title Update Description
#' @description Update the Date and Version fields in the DESCRIPTION file
#' @param date Date object, defaults to current date
#' @param path File path, defaults to working directory
#' @return Nothing is returned
UpdateDescription = function(date = Sys.Date(), path = getwd()){
  desc <- readLines(paste0(path,"/DESCRIPTION")) # read in the DESCRIPTION file
  vLine <- grep("^Version\\:", desc) # version number line

  # split version number
  vNum <- strsplit(gsub("^Version\\:\\s*", "", desc[vLine]), "\\.")[[1]]

  # part of version number to keep
  vNumKeep <- paste(vNum[1:(length(vNum) - 1)], sep = "", collapse = ".")

  # updated last part of version number
  vNumUpdate <- as.integer(vNum[length(vNum)]) + 1

  # reassembled new version number
  vFinal <- paste(vNumKeep, vNumUpdate, sep = ".")

  desc[vLine] <- paste0("Version: ", vFinal) # updated DESCRIPTION file object
  dLine <- grep("^Date\\:", desc) # date line
  desc[dLine] <- paste0("Date: ", date) # updated DESCRIPTION file object
  writeLines(desc, "DESCRIPTION") # output DESCRIPTION file
}

