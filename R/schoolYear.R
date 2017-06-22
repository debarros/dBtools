#' @title School Year
#' @description Determine the current school year
#' @param x type of information to be returned, defaults to "year"
#' @param y date, defaults to the current system date
#' @return integer - the year or term id
#' @examples
#' schoolYear()
#' schoolYear("TermID")
#' schoolYear(y = as.Date("2015-09-13"))
#' schoolYear(x = "TermID", y = as.Date("2015-02-13"))
schoolYear = function(x = "year", y = Sys.Date()){
  x = tolower(x)
  year = as.integer(format(y, "%Y"))
  schoolyearend = as.Date(paste0(year,"-06-30"))
  if(y <= schoolyearend) year = year - 1 # adjust for the Spring
  if(x == "year") return(year)
  if(x %in% c("termid", "term id")) return(100 * (year - 1990))
}
