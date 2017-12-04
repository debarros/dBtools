

CNMS.MakeBulkUpload = function(xlsxFile,
                               BedsCode = "010100860907",
                               singleGender = NULL,
                               uploadfilename = "bulkupload.txt",
                               messageLevel = 0){

  # Read in the student data export from PowerSchool
  CheckStuData = read.xlsx(xlsxFile = xlsxFile)
  CheckStuData$DOB = as.Date(CheckStuData$DOB, origin = "1899-12-30")
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
  CheckStuData$guardian2 = paste0(CheckStuData$Guardian_FN," ", CheckStuData$Guardian_LN)
  CheckStuData$guardian2[nchar(CheckStuData$guardian2) == 1] = ""

  # Define the output variables
  outputVars = c("BedsCode","StudentID","LastName","FirstName","DOB","Gender",
                 "Address1","Address2","City","State","ZIP","Phone","Guardian1","Guardian2")

  # Determine which variables in the input are associated with each field in the output
  zipVars = c("2nd_mailing_zip","Zip","Mailing_Zip")
  fnVars = c("First_Name")
  lnVars = "Last_Name"
  dobVars = "DOB"
  addrVars = c("2nd_mailing_street", "Mailing_Street","Street")
  cityVars = c("Mailing_City", "City", "2nd_mailing_city")
  phonVars = c("Home_Phone","Emerg_Phone_1","Emerg_Phone_2","StudentCoreFields.emerg_3_phone","StudentCoreFields.father_home_phone",
               "StudentCoreFields.guardiandayphone","StudentCoreFields.fatherdayphone","StudentCoreFields.mother_home_phone",
               "StudentCoreFields.motherdayphone","U_STUDENT_ENROLLMENT_PAGE.emerg_cell_phone_1","U_STUDENT_ENROLLMENT_PAGE.EMERG_CELL_PHONE_2",
               "U_STUDENT_ENROLLMENT_PAGE.EMERG_CELL_PHONE_3","U_STUDENT_ENROLLMENT_PAGE.emerg_work_phone_1",
               "U_STUDENT_ENROLLMENT_PAGE.EMERG_WORK_PHONE_2","U_STUDENT_ENROLLMENT_PAGE.EMERG_WORK_PHONE_3",
               "U_STUDENT_ENROLLMENT_PAGE.father_cell_phone","U_STUDENT_ENROLLMENT_PAGE.mother_cell_phone",
               "U_STUDENT_ENROLLMENT_PAGE.step_cell_phone","U_STUDENT_ENROLLMENT_PAGE.STEP_HOME_PHONE","U_STUDENT_ENROLLMENT_PAGE.stepdayphone")
  guardVars = c("Father", "guardian", "Mother", "guardian2")


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
