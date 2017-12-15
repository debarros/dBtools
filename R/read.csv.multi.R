#' @title Read Multiple CSV's
#' @description Read all the csv's in a folder than have filenames matching a given pattern and return a single data.frame
#' @param folder character of length 1 with the address of a folder
#' @param header	a logical value indicating whether the file contains the names of the variables as its first line. If missing, the value is determined from the file format: header is set to TRUE if and only if the first row contains one fewer field than the number of columns.
#' @param sep	the field separator character. Values on each line of the file are separated by this character. If sep = "" (the default for read.table) the separator is ‘white space’, that is one or more spaces, tabs, newlines or carriage returns.
#' @param quote	the set of quoting characters. To disable quoting altogether, use quote = "". Quoting is only considered for columns read as character, which is all of them unless colClasses is specified.
#' @param dec	the character used in the file for decimal points.
#' @param fill	logical. If TRUE then in case the rows have unequal length, blank fields are implicitly added. See ‘Details’.
#' @param trim  logical.  Should excess columns be trimmed (TRUE) or should missing columns be filled with NA's (FALSE)
#' @param comment.char	a character vector of length one containing a single character or an empty string. Use "" to turn off the interpretation of comments altogether.
#' @param stringsAsFactors Should strings be converted to factors?  Default is FALSE.  This should almost never be set to TRUE.
#' @param pattern	character string containing a regular expression to be matched to file names.  Coerced by as.character to a character string if possible. If a character vector of length 2 or more is supplied, the first element is used with a warning.
#' @param dupVar character of length 1 with the name of a variable for which duplicates should be dropped
#' @param orderBy character of length 1 with the name of a variable by which to sort the result
#' @param decreasing logical, should ordering be done by decreasing values?
#' @param idcol Generates an index column. Default (NULL) is not to. If idcol=TRUE then the column is auto named .id. Alternatively the column name can be directly provided, e.g., idcol = "id". If input is a named list, ids are generated using them, else using integer vector from 1 to length of input list.
#' @param messageLevel integer, the level of messages that should be printed to the console
#' @return data.table
read.csv.multi = function(folder,
                          header = TRUE,
                          sep = ",",
                          quote = "\"",
                          dec = ".",
                          fill = TRUE,
                          trim = TRUE,
                          comment.char = "",
                          stringsAsFactors = F,
                          pattern = NULL,
                          dupVar = NULL,
                          orderBy = NULL,
                          decreasing = T,
                          idcol = NULL,
                          messageLevel = 0){

  if(messageLevel > 0){ print("running read.csv.multi") }

  # Get all the filenames
  if(messageLevel > 1){ print("getting file names") }
  if(is.null(pattern)){
    filenames = list.files(folder, full.names = T)
  } else {
    filenames = grep(pattern = pattern, x = list.files(folder, full.names = T), ignore.case = T, value = T)
  }
  filenames = grep(pattern = "csv", x = filenames, ignore.case = T, value = T)

  # Check to see if there are any files
  if(length(filenames) == 0){
    stop("Either there are no csv files in the folder, or there are no csv files in the folder have names that match the pattern.")
  }

  # Set up a list to hold the data.frames
  ret = vector(mode = "list", length = length(filenames))

  # Read the csv's into data.frames and put them in the list
  if(messageLevel > 1){ print("reading files") }
  for(i in 1:length(filenames)){
    if(messageLevel > 1){ print(i) }
    ret[[i]] = read.csv(file = filenames[i], header = header, sep = sep, quote = quote, dec = dec,
                        fill = fill, comment.char = comment.char, stringsAsFactors = stringsAsFactors)
  }

  # Bind them csv's into a single data.table
  ret = data.table::rbindlist(l = ret, idcol = idcol, fill = trim)

  # Reorder
  if(!is.null(orderBy)){
    if(messageLevel > 1){ print("ordering the results") }
    ret = ret[order(ret[,orderBy], decreasing = decreasing),]
    rownames(ret) = NULL
  }

  # Remove duplicates
  if(!is.null(dupVar)){
    if(messageLevel > 1){ print("removing duplicates") }
    ret = ret[!duplicated(ret[,dupVar]),]
    rownames(ret) = NULL
  }

  return(ret)
} # /function

