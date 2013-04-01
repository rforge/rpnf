# This function converts a given stock quote into an integer boxnumber
# This function transforms a given quote into an unique integer box number
.quote2box <- function(quote, boxsize=1, log=FALSE) {
  if (log & min(quote)<= 0)
    stop("Error: quote must be greater than 0, if log=TRUE!")
  
  if (log==TRUE) {
    mylog <- function(x) {
      log(x)
    }
  } else {
    mylog <- function(x) {
      x
    }
  }
  
  as.integer(floor(mylog(quote)/boxsize))
}