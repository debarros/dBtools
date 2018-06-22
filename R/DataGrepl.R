#DataGrepl.R

#### DataGrepl ####

# This function does the following:
#  1) takes two dimensions of an array
#  2) pulls their dimnames
#  3) matches those to the rows names of two separate data.frames
#  4) pulls data values from the selected rows using specified variables
#  5) uses regular expressions to match patterns from each selected value from the first data.frame to each selected value of the second
#  6) outputs a logical matrix showing which patterns from the first data.frame found matches in which values from the second

DataGrepl = function(CompArray, row.data, col.data, firstdim, seconddim, dimensions = c(1,2), messageLevel = 0){
  
  if(messageLevel > 0) message("starting DataGrepl function")
  
  if(messageLevel > 1) message("get row info")
  row.info = row.data[,firstdim]
  
  if(messageLevel > 1) message("get col info")
  col.info = col.data[,seconddim]
  
  if(messageLevel > 1) message("make q")
  q = row.info[match(x = unlist(dimnames(CompArray)[dimensions[1]]), table = rownames(row.data))]
  
  if(messageLevel > 1) message("make r")
  r = col.info[match(x = unlist(dimnames(CompArray)[dimensions[2]]), table = rownames(col.data))]
  
  if(messageLevel > 1) message("call Vgrepl to make output")
  output = t(Vgrepl(pattern = q, x = r))
  
  gc()
  
  if(messageLevel > 1) message("set output dimnames")
  dimnames(output) = dimnames(CompArray)[dimensions]
  
  gc()
  
  if(messageLevel > 0) message("Ending DataGrepl function")
  
  return(output)
}
