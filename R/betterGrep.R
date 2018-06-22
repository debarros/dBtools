#betterGrep.R

#### betterGrep ####
betterGrep = function(pattern, x, ignore.case = T){
  y = grep(pattern, x, ignore.case = ignore.case)
  if(sum(is.na(y))>0){
    y[is.na(y)] = 0
    y = as.logical(y)
  }
  return(y)
}




#### VbetterGrep ####
VbetterGrep = Vectorize(FUN = betterGrep, vectorize.args = "pattern")
