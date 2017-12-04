#' @title School Year
#' @description Determine the current school year
#' @param x type of information to be returned, defaults to "year".  Other acceptable values include termID and end
#' @param y date, defaults to the current system date
#' @return integer - the year, term id, or date of the end of the school year
#' @examples
#' schoolYear()
#' schoolYear("TermID")
#' schoolYear(y = as.Date("2015-09-13"))
#' schoolYear(x = "TermID", y = as.Date("2015-02-13"))
schoolYear = function(x = "year", y = Sys.Date()){
  x = tolower(x)
  year = as.integer(format(y, "%Y"))
  schoolyearend = as.Date(paste0(year,"-06-30"))
  if(y <= schoolyearend) year = year - 1             # adjust for the Spring
  schoolyearend = as.Date(paste0(year + 1,"-06-30")) # the actual date of the end of the school year

  # Figure out what to return
  ret = NA
  if(x == "year") ret = year
  if(x %in% c("termid", "term id")) ret = 100 * (year - 1990)
  if(x == "end") ret = schoolyearend

  # Return
  return(ret)
}


#' @title BEDS Date
#' @description Determine the date of BEDS day in a school year (first Wednesday in October)
#' @param year integer - the 4 digit year at the beginning of the school year.  Defaults to current school year.
#' @return date - the date of BEDS day for the given school year
#' @examples
#' BedsDate()
#' BedsDate(2013)
BedsDate = function(year = schoolYear()){
  BedsDay = as.Date(paste0(year,"-10-01"))
  weekday = weekdays(BedsDay)
  while(weekday != "Wednesday"){
    BedsDay = BedsDay + 1
    weekday = weekdays(BedsDay)
  }
  return(BedsDay)
} # /function


