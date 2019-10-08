#' @title School Year
#' @description Determine the current school year
#' @param x type of information to be returned, defaults to "year".  Other acceptable values include termID, start, end, brief,
#'   and full
#' @param y date, defaults to the current system date
#' @return If \code{y} is year or termid, an integer is returned.  If \code{y} is start or end, a date is returned.  If \code{y}
#'   is brief or full, a character string is returned.
#' @examples
#' schoolYear()
#' schoolYear("TermID")
#' schoolYear(y = as.Date("2015-09-13"))
#' schoolYear(x = "TermID", y = as.Date("2015-02-13"))
#' schoolYear(x = "end")
#' schoolYear("end", as.Date("2015-09-13"))
#' schoolYear("end", as.Date("2015-02-13"))
#' schoolYear(x = "start")
#' schoolYear("start", as.Date("2015-09-13"))
#' schoolYear("start", as.Date("2015-02-13"))
#' schoolYear("full")
#' schoolYear("brief")
schoolYear = function(x = "year", y = Sys.Date()){
  x = tolower(x)
  year = as.integer(format(y, "%Y"))
  if(y <= as.Date(paste0(year, "-06-30"))) year = year - 1 # adjust for the Spring
  schoolyearend = as.Date(paste0(year + 1, "-06-30"))      # date of the end of the school year
  schoolyearstart = as.Date(paste0(year, "-07-01"))        # date of the start of the school year

  # Figure out what to return
  ret = NA
  if(x == "year") ret = year
  if(x %in% c("termid", "term id")) ret = 100 * (year - 1990)
  if(x == "end") ret = schoolyearend
  if(x == "start") ret = schoolyearstart
  if(x == "full") ret = paste0(year, "-", year + 1)
  if(x == "brief") ret = paste0(year %% 100, "-", (year + 1) %% 100) # The %% operator give the remainder after division

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






#' @title October 1st
#' @description Determine the date of October 1st in a school year
#' @param year integer - the 4 digit year at the beginning of the school year.  Defaults to current school year.
#' @return date - the date of October 1st day for the given school year
#' @examples
#' Oct1()
#' Oct1(2013)
Oct1 = function(year = schoolYear()){
  OctoberFirst = as.Date(paste0(year,"-10-01"))
  return(OctoberFirst)
} # /function






