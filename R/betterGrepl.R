#VGrepl.R

#### betterGrepl ####
# This function behaves similarly to grepl,
# except that, if the character vector has missing values,
# betterGrepl returns FALSE instead of NA for that element

betterGrepl = function(pattern, x, ignore.case = T){
  
  y = grepl(pattern, x, ignore.case = ignore.case)
  
  rm(list = c("x", "pattern"))
  gc()
  
  if(sum(is.na(y))>0){
    y[is.na(y)] = FALSE
  }
  gc()
  
  return(y)
}





#### Vgrepl ####
Vgrepl = Vectorize(FUN = betterGrepl, vectorize.args = "pattern")