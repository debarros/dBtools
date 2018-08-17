# Geocode.R

#' @title Get School District
#' @description Takes a one line address (USA only) and returns the school district serving that address using the first match from the US Census API
#' @param addr Character vector of length 1 containing an address
#' @param halt logical of length 1 indicating whether to halt if no school district is found
#' @param messageLevel integer of length 1 indicating the level of messaging to print
#' @return Character vector of length 1 containing the name of the school district or NA
#' @examples
#' getSchoolDistrict("1 MAIN ST, ALBANY, NY, 12204")
#' getSchoolDistrict("1234 MAIN Blvd, ALBANY, NY, 12204")
#' getSchoolDistrict("1234 MAIN Blvd, ALBANY, NY, 12204", halt = F)
getSchoolDistrict <- function (addr, halt = T, messageLevel = 0) {

  # Error Handling
  if(length(addr) != 1){
    stop("addr must have length 1")
  }
  if(!is.character(addr)){
    stop("addr must be of type character")
  }
  if(length(halt) != 1){
    stop("halt must have length 1")
  }
  if(!is.logical(halt)){
    stop("halt must be of type logical")
  }
  # /Error Handling

  # Clean the address using the character codes that go in URLs
  addr <- stringr::str_replace_all(string = addr, pattern = ",", replacement = "%2C")
  addr <- stringr::str_replace_all(string = addr, pattern = " ", replacement = "+")
  addr <- stringr::str_replace_all(string = addr, pattern = "#", replacement = "%23")

  # Remove parentheses because the census API doesn't like them.
  addr <- stringr::str_replace_all(string = addr, pattern = "(", replacement = "")
  addr <- stringr::str_replace_all(string = addr, pattern = ")", replacement = "")

  # Make the request URL and query the API
  a <- paste0("https://geocoding.geo.census.gov/geocoder/geographies/onelineaddress?address=",
              addr,
              "&benchmark=4&vintage=4&layers=Unified+School+Districts")
  resp <- httr::GET(url = a)

  # Check whether a match was returned
  if (length(httr::content(resp)$result$addressMatches) > 0) {
    return(httr::content(resp)$result$addressMatches[[1]]$geographies$`Unified School Districts`[[1]]$NAME)
  } else {
    if(halt){
      stop("No district found")
    } else {
      return(NA_character_)
    }
  } # /if-else there is a match
} # /function




#' @title Vector Get School District
#' @description Takes a vector of one line addresses (USA only) and returns the school district serving those address using the first matches from the US Census API
#' @param addrVec Character vector containing addresses
#' @param halt logical of length 1 indicating whether to halt if no school district is found
#' @param na.rm logical of length 1 indicating whether to remove NA's from the returned vector (ignored if \code{halt} is TRUE)
#' @param messageLevel integer of length 1 indicating the level of messaging to print
#' @return Character vector containing the names of the school districts or NA's
#' @examples
#' addrVecExample = c("1 MAIN ST, ALBANY, NY, 12204", "1234 MAIN Blvd, ALBANY, NY, 12204", "1234 MAIN Blvd, ALBANY, NY, 12204")
#' VgetSchoolDistrict(addrVecExample)
#' VgetSchoolDistrict(addrVecExample, halt = F)
#' VgetSchoolDistrict(addrVecExample, halt = F, na.rm = T)
VgetSchoolDistrict = function(addrVec, halt = T, na.rm = F, messageLevel = 0){
  ret = rep(NA_character_, times = length(addrVec))
  for(i in 1:length(addrVec)){
    ret[i] = getSchoolDistrict(addr = addrVec[i], halt = halt, messageLevel = messageLevel - 1)
  }
  if(na.rm){
    ret = ret[!is.na(ret)]
  }
  return(ret)
} # /function
