#' @title Create Comparison Matrix
#' @description Given two numeric matrices with equal dimensions, create a matrix of max values of corresponding cells and apply row and column names.  NA's are only returned if both matrices have NA in the same cell.
#' @param studentlist A vector of row names
#' @param Exams A vector of column names
#' @param matrix1 A numeric matrix
#' @param matrix2 A numeric matrix with the same dimensions as  \code{matrix1}
#' @return A numeric matrix with the same dimensions as the input matrices
#' @examples
#' s1 = LETTERS[1:5]
#' E1 = c("Math", "Science", "History")
#' m1 = matrix(sample(100, 15, F), 5)
#' m2 = matrix(c(NA, 7*(1:14)), 5)
#' m3 = matrix(c(NA, NA, 100 - 7*(1:13)), 5)
#' CreateCompareMatrix(s1, E1, m1, m2)
#' CreateCompareMatrix(s1, E1, m1, m3)
#' CreateCompareMatrix(s1, E1, m2, m3)
CreateCompareMatrix = function(studentlist, Exams, matrix1, matrix2){
  n = length(studentlist)
  nExam = length(Exams)
  CompareMatrix = matrix(integer(0), n, nExam)       # set up a matrix that will hold the best scores
  rownames(CompareMatrix) = studentlist              # in the matrix, name the rows according to the student ID
  colnames(CompareMatrix) = Exams                    # in the matrix, name the columns according to the exam or category name
  CompArray = abind::abind(matrix1, matrix2, along = 3)     # bind the two matrices of scores into a 3-d array
  CompareMatrix = MbetterMax(CompArray)              # for each student/exam intersection, pick the better of the two scores
  return(CompareMatrix)
}

