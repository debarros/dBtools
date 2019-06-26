# SummerSchoolFunctions.R


#' @title Summer School 1
#' @description Add columns to table of grades
#' @param grades data.frame of grades
#' @param courses data.frame of course information
#' @param regentsScores data.frame of regents scores
#' @return data.frame based on the \code{grades} input
SummerSchool1 = function(grades, courses, regentsScores){

  # Add computed columns to grades data.frame
  grades$passingScore = courses$Passing[match(grades$Course, courses$School.Year.Course)]
  grades$SummerCourse = courses$Equivalent.Summer.Course[match(grades$Course, courses$School.Year.Course)]
  grades$PointsOwed = grades$Pct - grades$passingScore
  grades$EligibleForSummer = "Passed"
  grades$EligibleForSummer[grades$PointsOwed < 0] = "Yes"
  grades$EligibleForSummer[grades$PointsOwed < -10] = "No"
  grades$SummerCourseOffered = courses$`Summer.Course.Offered?`[match(grades$Course, courses$School.Year.Course)]
  grades$OnPlato = courses$`Equivalent.Course.on.PLATO?`[match(grades$Course, courses$School.Year.Course)]
  grades$Critical = courses$`Critical?`[match(grades$Course, courses$School.Year.Course)]
  grades$AssociatedRegents = courses$Associated.Regents[match(grades$Course, courses$School.Year.Course)]

  # Add regents scores where appropriate
  grades$RegentsScore = 0
  for(i in 1:nrow(grades)){
    ID = grades$Student.Number[i]
    currentExam = grades$AssociatedRegents[i]
    currentScoreSet = regentsScores$Score[regentsScores$CountsFor == currentExam & regentsScores$StudentNumber == ID]
    if(length(currentScoreSet) > 0){
      grades$RegentsScore[i] = betterMax(currentScoreSet)
    }
  }

  # Indicate whether regents was passed
  grades$PassedRegents = grades$RegentsScore >= 65

  # Select a plan for each entry in the grades data.frame
  grades$ThePlan = "passed"
  for(i in 1:nrow(grades)){
    if(grades$EligibleForSummer[i] == "Passed"){        # If the student passed, leave the plan as "passed"

    } else {                                            # If the student failed
      if(grades$EligibleForSummer[i] == "Yes"){
        if(grades$SummerCourseOffered[i] != "No"){
          grades$ThePlan[i] = "Possibly Summer"
        } else {                                        # if the course is not being offered this summer
          if(grades$OnPlato[i] == "Yes"){
            grades$ThePlan[i] = "Summer on Plato"
          } else {                                      # if it's not on plato
            grades$ThePlan[i] = "Retake next year"
          } # if-else course is on Plato
        } # if-else summer course is offered

      } else {                                          # if it's not eligible for summer school
        if(grades$OnPlato[i] == "Yes"){
          if(grades$Critical[i] == "Yes"){
            if(grades$PassedRegents[i]){
              grades$ThePlan[i] = "Retake on Plato"
            } else {                                    # if the student didn't pass the regents
              grades$ThePlan[i] = "Retake next year"
            } # /if-else student passed regents
          } else {                                      # if it's not critical
            grades$ThePlan[i] = "Retake on Plato"
          } # /if-else course is critical
        } else {                                        # if it's not on Plato
          grades$ThePlan[i] = "Retake next year"
        } # /if-else course is on Plato
      } # /if-else courses is eligible for summer school
    } # /if-else student passed or failed
  } # /for each grade

  return(grades)
} # /SummerSchool1 function





#' @title Summer School 2
#' @description Create the students data.frame
#' @param grades data.frame of grades
#' @return data.frame of unique students with counts of various different plan types
SummerSchool2 = function(grades){
  students = grades[!duplicated(grades$Student.Number),c("Student.Number", "Student", "Class")]
  colnames(students)[colnames(students) == "Class"] = "Grade_Level"

  # Count the summer needs for each student
  students$SummerNeeds = 0
  students$CriticalSummerNeeds = 0
  students$NonCriticalSummerNeedsAvailableOnPlato = 0
  for(i in 1:nrow(students)){
    ID = students$Student.Number[i]
    students$SummerNeeds[i] = sum(grades$Student.Number == ID & grades$ThePlan == "Possibly Summer")
    students$CriticalSummerNeeds[i] = sum(grades$Student.Number == ID &
                                            grades$ThePlan == "Possibly Summer" &
                                            grades$Critical == "Yes")
    students$NonCriticalSummerNeedsAvailableOnPlato[i] = sum(
      grades$Student.Number == ID &
        grades$ThePlan == "Possibly Summer" &
        grades$Critical == "No" &
        grades$OnPlato == "Yes")
  }

  return(students)
} # /SummerSchool2 function







#' @title Summer School 3
#' @description Resolve most Possibly Summer situations
#' @param grades data.frame of grades
#' @param students data.frame of students
#' @param maxInClass the maximum number of Summer School In Class a student can take
#' @param maxAllSummer the maximum number of Summer School In Class or Summer On Plato courses (this currently does nothing)
#' @return data.frame based on the \code{grades} input
SummerSchool3 = function(grades, students, maxInClass = 2, maxAllSummer = NA){
  # This is the function where the rule that a student can only take 2 courses in summer school is applied

  for(i in 1:nrow(students)){

    ID = students$Student.Number[i]                                       # Student ID
    SumNeed = students$SummerNeeds[i]                                     # No. classes needed that are offered inclass
    CritSumNeed = students$CriticalSummerNeeds[i]                         # No. critical classes needed that are inclass only
    NonCritPlatoable = students$NonCriticalSummerNeedsAvailableOnPlato[i] # No. noncritical classes needed that are platoable
    NonCritNonPlatoable = SumNeed - CritSumNeed - NonCritPlatoable        # No. noncritical classes needed that are only inclass

    if(SumNeed %in% 1:maxInClass){                                        # If they need inclass courses no more than the max,
      grades$ThePlan[grades$ThePlan == "Possibly Summer" &                # give them everything.
                       grades$Student.Number == ID] = "Definitely Summer"
    } else if(SumNeed > maxInClass){                                      # If they need more than the max inclass courses

      if(CritSumNeed %in% 1:(maxInClass - 1)){                            # If critical course inclass needed but can have others
        grades$ThePlan[grades$Student.Number == ID &                      # Assign all critical courses inclass
                         grades$ThePlan == "Possibly Summer" &
                         grades$Critical == "Yes"] = "Definitely Summer"
        if(CritSumNeed + NonCritNonPlatoable %in% 1:(maxInClass)){        # If they can still fit all nonplatoable courses
          grades$ThePlan[grades$Student.Number == ID &                    # assign them inclass
                           grades$ThePlan == "Possibly Summer" &
                           grades$Critical == "No" &
                           grades$OnPlato == "No"] = "Definitely Summer"
          if(CritSumNeed + NonCritNonPlatoable == maxInClass){            # If no room left after critical and nonplatoable courses,
            grades$ThePlan[grades$Student.Number == ID &                  # make noncritical platoable courses on plato
                             grades$ThePlan == "Possibly Summer" &
                             grades$Critical == "No" &
                             grades$OnPlato == "Yes"] = "Summer on Plato"
          }
        }

      } else if(CritSumNeed == maxInClass){                               # If all inclass spots must be allocated to critical classes
        grades$ThePlan[grades$Student.Number == ID &                      # Assign critical inclass
                         grades$ThePlan == "Possibly Summer" &
                         grades$Critical == "Yes"] = "Definitely Summer"
        grades$ThePlan[grades$Student.Number == ID &                      # Assign noncrit platoable to plato
                         grades$ThePlan == "Possibly Summer" &
                         grades$Critical == "No" &
                         grades$OnPlato == "Yes"] = "Summer on Plato"
        grades$ThePlan[grades$Student.Number == ID &                      # Assign noncrit nonplatoable to next year
                         grades$ThePlan == "Possibly Summer" &
                         grades$Critical == "No" &
                         grades$OnPlato == "No"] = "Retake next year"

      } else if(CritSumNeed == 0){                                        # For students who have too many summer needs but none are critical
        if(NonCritNonPlatoable > 0){                                      # If they have any that can only be taken in class
          if(NonCritNonPlatoable < maxInClass){                           # if can assign all nonplatoable and some platoable courses
            grades$ThePlan[grades$Student.Number == ID &                  # assign all nonplatoable courses; leave platoable courses as options
                             grades$ThePlan == "Possibly Summer" &
                             grades$Critical == "No" &
                             grades$OnPlato == "No"] = "Definitely Summer"
          } else if(NonCritNonPlatoable == maxInClass){                   # If they have exactly enough room for nonplatoable courses
            grades$ThePlan[grades$Student.Number == ID &                  # Assign all nonplato courses to inclass,
                             grades$ThePlan == "Possibly Summer" &
                             grades$Critical == "No" &
                             grades$OnPlato == "No"] = "Definitely Summer"
            grades$ThePlan[grades$Student.Number == ID &                  # and make platoable courses be on plato
                             grades$ThePlan == "Possibly Summer" &
                             grades$Critical == "No" &
                             grades$OnPlato == "Yes"] = "Summer on Plato"
          }
        }
      } # /if-else-else number of criticalSummerNeeds

    } # /if student needs more classes than he can have
  } # /for each student

  return(grades)
} # /SummerSchool3 function







#' @title Summer School 4
#' @description Determine the Summer Courses
#' @param grades data.frame of grades
#' @param courses data.frame of course information
#' @return data.frame of summer courses
SummerSchool4 = function(courses, grades){

  SummerCourses = courses[!duplicated(courses$Equivalent.Summer.Course),
                          c("Equivalent.Summer.Course", "Summer.Course.Offered?", "Subject")]
  rownames(SummerCourses) = NULL
  SummerCourses = SummerCourses[SummerCourses$`Summer.Course.Offered?` != "No",]
  rownames(SummerCourses) = NULL

  # Count how many students will be in each summer course
  SummerCourses$StudentCount = 0
  for(i in 2:nrow(SummerCourses)){
    curSumCrse = SummerCourses$Equivalent.Summer.Course[i]
    SummerCourses$StudentCount[i] = sum(grades$ThePlan == "Definitely Summer" & grades$SummerCourse == curSumCrse)
  }

  # Determine how many sections of each summer course to offer
  SummerCourses$SectionCount = ceiling(SummerCourses$StudentCount/12)
  SummerCourses$SectionCount[SummerCourses$StudentCount < 4] = 0

  return(SummerCourses)
} # /SummerSchool4 function




#' @title Summer School 5
#' @description Add plan counts to the students data.frame
#' @param students data.frame of students
#' @param RevisedGrades data.frame of grades
#' @return data.frame based on the \code{students} input
SummerSchool5 = function(students, RevisedGrades){

  students$SummerCount = 0
  students$SummerPlatoCount = 0
  students$RetakeNextYear = 0
  students$RetakeOnPlato = 0
  students$totalRedo = 0
  for(i in 1:nrow(students)){
    curID = students$Student.Number[i]
    students$SummerCount[i] = sum(RevisedGrades$Student.Number == curID & RevisedGrades$ThePlan == "Definitely Summer")
    students$SummerPlatoCount[i] = sum(RevisedGrades$Student.Number == curID & RevisedGrades$ThePlan == "Summer on Plato")
    students$RetakeNextYear[i] = sum(RevisedGrades$Student.Number == curID & RevisedGrades$ThePlan == "Retake next year")
    students$RetakeOnPlato[i] = sum(RevisedGrades$Student.Number == curID & RevisedGrades$ThePlan == "Retake on Plato")
    students$totalRedo[i] = sum(students[i,c("SummerCount", "SummerPlatoCount", "RetakeNextYear", "RetakeOnPlato")])
  }

  return(students)
} # /SummerSchool5 function










