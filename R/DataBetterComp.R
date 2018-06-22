#DataComp.R

#Compare  values of row.data$firstdim to col.data$seconddim and create a T/F matrix of matches
#"dimensions" is used to select the dimensions of CompArray
#The dimnames of the selected dimenions are extracted and form the dimensions of the output
#The first set of dimnames are matched to the rownames in row.data
#The second set of dimnames are matched to the rownames in col.data
#Each entry in the output matrix is a comparison of the data values from the corresponding cells in row.data and col.data

DataComp = function(CompArray, row.data, col.data, firstdim, seconddim, dimensions = c(1,2)){
  row.info = row.data[,firstdim]
  col.info = col.data[,seconddim]
  q = row.info[match(x = unlist(dimnames(CompArray)[dimensions[1]]), table = rownames(row.data))]
  r = col.info[match(x = unlist(dimnames(CompArray)[dimensions[2]]), table = rownames(col.data))]
  output = VbetterComp(q, r)
  dimnames(output) = dimnames(CompArray)[dimensions]
  
  gc()
  
  return(output)
}