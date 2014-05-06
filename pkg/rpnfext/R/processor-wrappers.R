#' Wrapper for using \link{pnfprocessor} directly with an xts-object.
#' 
#' By default the second column as parameter 'high',
#' the third column as parameter 'low' and the index as parameter date 
#' of the xts object will be used.
#' 
#' @param xts xts object, e.g. quotes returned by quantmod's getSymbol()
#' @param columns columns to be used as high and low quotes series
#' @param ... additional parameters of function pnfprocessor (e.g. reversal, log, style, etc.)
#' @inheritParams pnfprocessor
#' @return returns a data table with all point and figure information in it
#' @export
#' @seealso \code{\link{pnfprocessor}}
#' @references \url{http://rpnf.r-forge.r-project.org}
#' @import xts
#' @examples
#' library(rpnf) # Load rpnf library
#' library(xts) # Load xts library
#' data(GDAXI) # Load some example data
#' xts <- xts(x=cbind(GDAXI$High,GDAXI$Low),order.by=GDAXI$Date) # Convert to xts object
#' pnfdata <- pnfprocessor.xts(xts=xts, columns = c(1,2), boxsize=100L,log=FALSE)
#' tail(pnfdata)
#' pnfplottxt(pnfdata,boxsize=100L,log=FALSE)
#' pnfplot(pnfdata)
pnfprocessor.xts <- function(xts, columns = c(2,3),...) {
  ### Check parameters
  if (!is.xts(xts))
    stop("Parameter xts has to be of type xts!")
  if (!is.numeric(columns))
    stop("Parameter columns has to be numeric!")
  if (length(columns)!=2)
    stop("Parameter columns has to have exactly two values ('High'-column and 'Low'-column number)!")
  if (!(all(range(columns) %in% c(1:ncol(xts)))))
    stop(paste0("Parameter columns should be within 1:",ncol(xts)," for given xts object!"))
  
  ### Execute pnfprocessor
  pnfprocessor(high=xts[,columns[1]],
               low=xts[,columns[2]],
               date=index(xts),
               ...)
}
