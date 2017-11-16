#' @title Get Nice Column Names
#' @description Get the column names of a particular SIRS export
#' @param sheet character vector of length 1 with the exact name of a sheet in the template workbook
#' @param templates an openxlsx workbook object hold the template workbook
#' @return A character vector with the names of the columns in a SIRS export with all of the non-alphanumeric characters removed
GetNiceColumnNames = function(sheet, templates){
  Columns = openxlsx::read.xlsx(templates, sheet = sheet, startRow = 2)[,3] # get the column names
  for(i in 1:length(Columns)){                                              # fix the column names
    Columns[i] = gsub("[^[:alnum:]]", "", Columns[i])
  }
  return(Columns)
} # /function

