#' Generate all point and figure informations for a given time series.
#' 
#' Please ensure that high, low and date are all ordered according to the Date column.
#' 
#' @param high a vector containing the high quotes
#' @param low a (optional) vector containing the low quotes
#' @param date a vector of dates the quotes belong
#' @param reversal number of boxes needed to make a reversal 
#' @param boxsize the boxsize to be used
#' @param log should we do the calculations on a logarithmic scale
#' @param style the style the pnfprocessor is working with. Can be \{xo,rs,bp\}.
#' @return returns a data table with all point and figure information in it
#' @export
#' @seealso \code{\link{pnfplot}}
#' @seealso \code{\link{pnfplottxt}}
#' @references \url{http://rpnf.r-forge.r-project.org}
#' @examples
#' library(rpnf) # Load rpnf library
#' data(GDAXI) # Load some example data
#' pnfdata <- pnfprocessor(
#'   high=GDAXI$High,
#'   low=GDAXI$Low,
#'   date=GDAXI$Date,
#'   boxsize=100L,
#'   log=FALSE)  
#' tail(pnfdata)
#' pnfplottxt(pnfdata,boxsize=100L,log=FALSE)
#' pnfplot(pnfdata)
pnfprocessor <- function(
  high,
  low=high,
  date,
  reversal=3L, 
  boxsize=1, 
  log=FALSE,
  style="xo") {
  # check for proper style selection
  if (!style %in% c("xo","rs","bp")) {
    stop("Select a proper chart style: 'xo', 'rs' or'bp'!")
  }
  # first execute basic time series processing
  result <- xo.processor(high,low,date,reversal,boxsize,log)
  # now check for style to enhance the result
  if (style == "xo") {
    result <- xo.signalprocessor(result,reversal)
    result <- .xo.trendline.processor(result)
    result <- .xo.priceobjective.processor(result,reversal,boxsize,log)
  } else if (style == "bp") {
    result <- .bp.signalprocessor(result)
  } else if (style == "rs") {
    result <- .rs.signal.processor(result)
  } else {
    warning("Unknown style detected! No chart enhancements were made!")
  }
  result
}


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
