#' Determine an appropriate boxsize, if you want to use logarithmic scale.
#'
#' This function returns an appropriate boxsize if you want to do your point and figure analysis with an logarithmic scale.
#' 
#' @param percent a numeric value defining the percent 
#' @return a numeric value which is equivalent to the percental change given on a logarithmic scale
#' @export
#' @examples
#' # return appropriate value for 1% boxsize
#' getLogBoxsize(percent=1)
#' 
#' # apply it with pnfprocessor
#' library(rpnf) # Load rpnf library
#' data(GDAXI) # Load some example data
#' 
#' pnfprocessor(
#'  high=GDAXI$High,
#'  low=GDAXI$Low,
#'  date=GDAXI$Date,
#'  boxsize=getLogBoxsize(percent=1),
#'  log=TRUE)
#'
getLogBoxsize <- function(percent) {
  if (class(percent)!="numeric")
    stop("percent value has to be numeric!")
  return (log(100+percent)-log(100))
}
