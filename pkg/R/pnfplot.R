#' Generate a modern point and figure plot
#' 
#' @param data a data frame object containing point and figure informations to be plotted
#' @param ... any additional options for the plot command
#' @export
pnfplot <- function(data,...) {
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
  points(data$date[index.XB],data[index.XB,2],pch=4,col="green",cex=0.75)
  points(data$date[index.XS],data[index.XS,2],pch=4,col="red",cex=0.75)
  # plot nextX as green line
  lines(data$date,data$nextX,col="dark green",lwd=2)
  # for O-columns plot lows with pch=1
  points(data$date[index.OB],data[index.OB,3],pch=1,col="green",cex=0.75)
  points(data$date[index.OS],data[index.OS,3],pch=1,col="red",cex=0.75)
  # plot nextO as red line
  lines(data$date,data$nextO,col="dark red",lwd=2)
}

