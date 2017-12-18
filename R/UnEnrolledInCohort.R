#' @title Unenrolled Students In The Cohort
#' @description Finds all students who are no longer enrolled but still in the graduation cohort
#' @param WB data.frame of the accountability workbook
#' @param cohorts integer - how many cohorts should be analyzed?
#' @param returnvars character vector with the names of the columns to be included in the return
#' @param messageLevel integer - what degree of messaging should be printed to the console?
#' @return a data.frame with all of the students who are in their grad cohort but not enrolled
UnEnrolledInCohort = function(WB,
                              cohorts,
                              returnvars = c("Local.ID.(optional)",
                                             "Last.Name",
                                             "First.Name",
                                             "Cohort.Year.(year.1st.entered.9th)",
                                             "Date.left.GTH",
                                             "Discharge.Reason"),
                              messageLevel = 0){
  firstyear = schoolYear() - cohorts + 1                                        # determine the earliest cohort to use
  WB.InCo = WB[toupper(WB$`Included.in.Graduation.Cohort?`) == "YES",]          # limit to those in the grad cohort
  WB.InCo.UnEnr = WB.InCo[toupper(WB.InCo$`Still.Enrolled?`) == "NO",]          # limit to those no longer enrolled
  x = VbetterComp(toupper(WB.InCo.UnEnr$Discharge.Reason), "GRADUATED")         # find graduates
  WB.Prob = WB.InCo.UnEnr[!x,]                                                  # remove graduates
  WB.Prob = WB.Prob[WB.Prob$`Cohort.Year.(year.1st.entered.9th)` >= firstyear,] # Limit to relevant cohorts
  WB.Prob = WB.Prob[,returnvars]                                                # limit to relevant columns
  return(WB.Prob)
} # /function
