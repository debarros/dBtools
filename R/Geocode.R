# Geocode.R

#' @title Get School District
#' @description Takes a one line address (USA only) and returns the school district serving that address using the first match from the US Census API
#' @param addr Character vector of length 1 containing an address
#' @param halt logical of length 1 indicating whether to halt if no school district is found
#' @return Character vector of length 1 containing the name of the school district or NA
#' @examples
#' getSchoolDistrict("1 MAIN ST, ALBANY, NY, 12204")
#' getSchoolDistrict("1234 MAIN Blvd, ALBANY, NY, 12204")
#' getSchoolDistrict("1234 MAIN Blvd, ALBANY, NY, 12204", halt = F)
getSchoolDistrict <- function (addr, halt = T) {
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
  addr <- stringr::str_replace_all(string = addr, pattern = ",", replacement = "%2C")
  addr <- stringr::str_replace_all(string = addr, pattern = " ", replacement = "+")
  addr <- stringr::str_replace_all(string = addr, pattern = "#", replacement = "%23")
  a <- paste0("https://geocoding.geo.census.gov/geocoder/geographies/onelineaddress?address=",
              addr,
              "&benchmark=4&vintage=4&layers=Unified+School+Districts")
  resp <- httr::GET(url = a)
  if (length(httr::content(resp)$result$addressMatches) > 0) {
    return(httr::content(resp)$result$addressMatches[[1]]$geographies$`Unified School Districts`[[1]]$NAME)
  }
  else {
    if(halt){
      stop("No district found")
    } else {
      return(NA_character_)
    }
  }
}





VgetSchoolDist = function(addrVec, halt = T, na.rm = F){
  ret = rep(NA_character_, times = length(addrVec))
  for(i in 1:length(addrVec)){
    ret[i] = getSchoolDistrict(addr = addrVec[i], halt = halt)
  }
  if(na.rm){
    ret = ret[!is.na(ret)]
  }
  return(ret)
}
