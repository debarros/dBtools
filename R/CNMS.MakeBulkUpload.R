# singleGender = "M"
# xlsxFile = file.choose()

#' @title Make Bulk Upload for NYSSIS/Child Nutrition Management System
#' @description Based on an export from the Student Management System, generate a text file that can be uploaded to NYSSIS (using the CN role) to generate matches to SNAP and Medicaid records for direct certification of free lunch eligibility
#' @param xlsxFile An xlsx file, Workbook object, or URL to an xlsx file
#' @param BedsCode The 12 digit BEDS code for the LEA (Local Education Agency, usually a district)
#' @param singleGender Indicator of single gender LEA.  Acceptable values are "M", "F", and NULL
#' @param uploadfilename Name of the output file.  Should end in ".txt".  Defaults to "bulkupload.txt".
#' @param messageLevel Numeric of length 1 indicating the level of messages to print
#' @return Nothing is returned
CNMS.MakeBulkUpload = function(xlsxFile,
                               BedsCode = "010100860907",
                               singleGender = NULL,
                               uploadfilename = "bulkupload.txt",
                               messageLevel = 0){

  # Read in the student data export from PowerSchool
  CheckStuData = openxlsx::read.xlsx(xlsxFile = xlsxFile)
  CheckStuData$DOB = xlDate(x = CheckStuData$DOB)
  if(!is.null(singleGender)){
    if(singleGender %in% c("M", "F")){
      CheckStuData$Gender = singleGender
    } else {
      stop("singleGender must be either 'M' or 'F'.")
    }
  }
  for(i in 1:ncol(CheckStuData)){
    CheckStuData[,i] = as.character(CheckStuData[,i])
    CheckStuData[,i] = dBtools::na.to.empty(CheckStuData[,i])
  }

  # Establish a full guardian name from the first and last names
  CheckStuData$guardian2 = paste0(CheckStuData$StudentCoreFields.guardian_fn," ", CheckStuData$StudentCoreFields.guardian_ln)
  CheckStuData$guardian2[nchar(CheckStuData$guardian2) == 1] = ""

  CheckStuData$guardian3 = paste0(CheckStuData$Guardian_FN," ", CheckStuData$Guardian_LN)
  CheckStuData$guardian3[nchar(CheckStuData$guardian2) == 1] = ""

  # Define the output variables
  outputVars = c("BedsCode","StudentID","LastName","FirstName","DOB","Gender",
                 "Address1","Address2","City","State","ZIP","Phone","Guardian1","Guardian2")

  # Determine which variables in the input are associated with each field in the output
  zipVars = c("U_STUDENT_ENROLLMENT_PAGE.MAILING_2_ZIP","Zip","Mailing_Zip", "2nd_mailing_zip")
  fnVars = c("First_name")
  lnVars = "Last_Name"
  dobVars = "DOB"
  addrVars = c("U_STUDENT_ENROLLMENT_PAGE.MAILING_2_STREET", "Mailing_Street","Street", "2nd_mailing_street")
  cityVars = c("Mailing_City", "City", "U_STUDENT_ENROLLMENT_PAGE.MAILING_2_CITY", "2nd_mailing_city")
  phonVars = c("StudentCoreFields.guardiandayphone", "StudentCoreFields.father_home_phone", "U_STUDENT_ENROLLMENT_PAGE.stepdayphone",
               "U_STUDENT_ENROLLMENT_PAGE.STEP_HOME_PHONE", "U_STUDENT_ENROLLMENT_PAGE.step_cell_phone", "U_STUDENT_ENROLLMENT_PAGE.mother_cell_phone",
               "U_STUDENT_ENROLLMENT_PAGE.father_cell_phone", "U_STUDENT_ENROLLMENT_PAGE.EMERG_WORK_PHONE_3", "U_STUDENT_ENROLLMENT_PAGE.EMERG_WORK_PHONE_2",
               "U_STUDENT_ENROLLMENT_PAGE.emerg_work_phone_1", "U_STUDENT_ENROLLMENT_PAGE.EMERG_CELL_PHONE_3", "U_STUDENT_ENROLLMENT_PAGE.EMERG_CELL_PHONE_2",
               "U_STUDENT_ENROLLMENT_PAGE.emerg_cell_phone_1", "StudentCoreFields.motherdayphone", "StudentCoreFields.mother_home_phone",
               "StudentCoreFields.emerg_3_phone", "Emerg_Phone_2", "Emerg_Phone_1", "Home_Phone")
  guardVars = c("Father", "StudentCoreFields.guardian", "Mother", "guardian2", "guardian", "guardian3", "U_STUDENT_ENROLLMENT_PAGE.stepparent", "StepParent")
  guardComponentColumns = c("StudentCoreFields.guardian_fn", "StudentCoreFields.guardian_ln", "Guardian_FN", "Guardian_LN")


  columnsNotUsed = setdiff(colnames(CheckStuData), c(zipVars, fnVars, lnVars, dobVars, addrVars, cityVars, phonVars, guardVars, guardComponentColumns))
  columnsNotFound = setdiff(c(zipVars, fnVars, lnVars, dobVars, addrVars, cityVars, phonVars, guardVars, guardComponentColumns), colnames(CheckStuData))

  if(length(columnsNotUsed) > 0){
    print("The following columns are in the import file, but will not be used:")
    print(columnsNotUsed)
  }

  if(length(columnsNotFound) > 0){
    print("The following columns are missing from the import file!")
    print(columnsNotFound)
    stop()
  }


  # Establish a list to hold the student information in list format
  studentList = vector(mode = "list", length = nrow(CheckStuData))

  # Generate one list entry for each student
  for(i in 1:length(studentList)){
    x = vector(mode = "list", length = length(outputVars))
    names(x) = outputVars
    x$BedsCode = BedsCode
    x$StudentID = CheckStuData$Student_number[i]
    x$Gender = CheckStuData$Gender[i]
    x$State = "NY"
    x$LastName = unlist(unique(c(CheckStuData[i,lnVars])))
    x$FirstName = unlist(unique(c(CheckStuData[i,fnVars])))
    x$DOB = unlist(unique(c(CheckStuData[i,dobVars])))
    x$Address1 = unlist(unique(c(CheckStuData[i,addrVars])))
    x$City = unlist(unique(c(CheckStuData[i,cityVars])))
    x$ZIP = unlist(unique(c(CheckStuData[i,zipVars])))
    x$Phone = unlist(unique(c(CheckStuData[i,phonVars])))
    x$Guardian1 = unlist(unique(c(CheckStuData[i,guardVars])))
    for(j in 1:length(x)){
      if(!(names(x)[j] %in% c("Guardian2","Address2"))){
        allofem = x[[j]]
        allofem = allofem[nchar(allofem) > 0]
        x[[j]] = allofem
      }
    }
    x$Guardian2 = ""
    x$Address2 = ""
    studentList[[i]] = x
  }

  # Establish a list to hold the student information in data.frame format
  studentFrames = vector(mode = "list", length = length(studentList))

  # Generate a data.frame for each student with all the combinations of info
  for(i in 1:length(studentFrames)){
    studentFrames[[i]] = expand.grid(studentList[[i]])
  }

  # Combine the student info into a single data.table
  studentFrames = data.table::rbindlist(studentFrames)

  # Generate the output
  write.table(studentFrames, file = uploadfilename, row.names = F, col.names = F, sep = "|", dec = ".", quote = F)

} # /function
