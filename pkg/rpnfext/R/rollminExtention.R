
#' Determines the minimum values of a given xts object for the next kmax days.
#'
#' @param x An xts-object, in particular an object returned by quantmod::getSymbols. 
#' @param kmax Number of (working) days to look forward for minimum determination
#'
#' @return An xts-object with minimum values for 1 to kmax days in columns.
#' @export
#'
#' @examples FIXME
#' x <- xts(x = 100 + cumsum(rnorm(500,0.1,1)),order.by = seq.Date(from=Sys.Date()-500,by=1,length=500))
#' xmin <- rollminExtention(x,kmax=50)
rollminExtention <- function(x, kmax=200) {
  if (!is.xts(x)) {
    stop("Parameter x is not an xts object!")
  }
  if (!is.numeric(kmax) | kmax<2) {
    stop("Parameter kmax has to be a numeric greater equal 2!")
  }
  # initialize result
  result <- xts(x,order.by = index(x))
  names(result) <- paste0(names(result),".Min",1,"d")
  # generate additional columns with longer lookahead
  for (k in 2:kmax) {
    temp <- rollmin.xts(x = x,k = k,fill = NA,align = "left")
    names(temp) <- paste0(names(temp),".Min",k,"d")
    result <- merge(result,temp)
  }
  return(result)  
}
