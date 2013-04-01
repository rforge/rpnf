# This function returns the upper bound for a given boxnumber
.box2upper <- function(boxnumber, boxsize=1, log=FALSE) {
  if(sum(floor(boxnumber)!=boxnumber)>0)
    stop("Error: Only integer values allowed as boxnumber!")
  if (log==TRUE) {
    myexp <- function(x){
      exp(x)
    }
  } else {
    myexp <- function(x){
      x
    }
  }
  
  myexp((boxnumber+1)*boxsize)
}