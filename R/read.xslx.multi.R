#' @title Read Multiple Excel workbooks
#' @description Read all the xlsx files in a folder than have filenames matching a given pattern and return a single data.frame
#' @param folder character of length 1 with the address of a folder
#' @param sheet integer or character of length 1 indicating the worksheet to use
#' @param pattern	character string containing a regular expression to be matched in the given character vector. Coerced by as.character to a character string if possible. If a character vector of length 2 or more is supplied, the first element is used with a warning.
#' @param dupVar character of length 1 with the name of a variable for which duplicates should be dropped
#' @param orderBy character of length 1 with the name of a variable by which to sort the result
#' @param decreasing logical, should ordering be done by decreasing values?
#' @param messageLevel integer, the level of messages that should be printed to the console
#' @return data.frame
read.xlsx.multi = function(folder,
                          sheet = 1,
                          pattern = NULL,
                          dupVar = NULL,
                          orderBy = NULL,
                          decreasing = T,
                          messageLevel = 0){

  if(messageLevel > 0){ print("running read.xlsx.multi") }

  # Get all the filenames
  if(is.null(pattern)){
    filenames = list.files(folder, full.names = T)
  } else {
    filenames = grep(pattern = pattern, x = list.files(folder, full.names = T), ignore.case = T, value = T)
  }
  filenames = grep(pattern = "xlsx", x = filenames, ignore.case = T, value = T)

  # Check to see if there are any files
  if(length(filenames) == 0){
    stop("No files in the folder have names that match the pattern.")
  }

  # Set up a list to hold the data.frames
  ret = vector(mode = "list", length = length(filenames))

  # Read the files into data.frames and put them in the list
  for(i in 1:length(filenames)){
    if(messageLevel > 1){ print(i) }
    ret[[i]] = openxlsx::read.xlsx(xlsxFile = filenames[i], sheet = sheet)
  }

  # Bind the data.frames into a single data.table
  ret = data.table::rbindlist(l = ret)
  ret = as.data.frame(ret)

  # Reorder
  if(!is.null(orderBy)){
    ret = ret[order(ret[,orderBy], decreasing = decreasing),]
    rownames(ret) = NULL
  }

  # Remove duplicates
  if(!is.null(dupVar)){
    ret = ret[!duplicated(ret[,dupVar]),]
    rownames(ret) = NULL
  }

  return(ret)
} # /function
