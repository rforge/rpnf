#' Generate a modern point and figure plot
#' 
#' @param data a data frame object containing point and figure informations to be plotted
#' @param ... any additional options for the plot command
#' @seealso \code{\link{pnfprocessor}}
#' @seealso \code{\link{pnfplottxt}}
#' @references \url{http://rpnf.r-forge.r-project.org}
#' @export
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

pnfplot <- function(data,reversal=3,boxsize=1,log=FALSE,...) {
  if (length(intersect(names(data),c("date","nextO","nextX","status.xo","status.bs","high","low")))!=7)
    stop(paste("input data frame must contain columns ",c("date","nextO","nextX","status.xo","status.bs","high","low"),"!"))
  # determine boundaries
  xlim <- c(min(data$date),max(data$date))
  ylim <- c(min(data$nextO),max(data$nextX))
  plot(NULL,NULL,xlim=xlim,ylim=ylim,...)
  # plot X values with pch=4
  index.XB <- data$status.xo=="X"&data$status.bs=="Buy"
  index.XS <- data$status.xo=="X"&data$status.bs=="Sell"
  index.OB <- data$status.xo=="O"&data$status.bs=="Buy"
  index.OS <- data$status.xo=="O"&data$status.bs=="Sell"
  # for X-columns plot highs
  points(data$date[index.XB],data$high[index.XB],pch=4,col="green",cex=0.75)
  points(data$date[index.XS],data$high[index.XS],pch=4,col="red",cex=0.75)
  # plot nextX as green line
  lines(data$date,data$nextX,col="green",lwd=1)
  # for O-columns plot lows with pch=1
  points(data$date[index.OB],data$low[index.OB],pch=1,col="green",cex=0.75)
  points(data$date[index.OS],data$low[index.OS],pch=1,col="red",cex=0.75)
  # plot nextO as red line
  lines(data$date,data$nextO,col="red",lwd=1)
  # plot optional bullish support and bearish resistance lines
  if ("tl.brl.boxnumber" %in% names(data)) {
    mybsl <- rpnf:::.box2lower(boxnumber=data[,"tl.bsl.boxnumber"],boxsize=boxsize,log=log)
    lines(data$date,mybsl,col="dark green",lwd=2)
  }
  if ("tl.bsl.boxnumber" %in% names(data)) {
    mybrl <- rpnf:::.box2upper(boxnumber=data[,"tl.brl.boxnumber"],boxsize=boxsize,log=log)
    lines(data$date,mybrl,col="dark red",lwd=2)
  }
}


