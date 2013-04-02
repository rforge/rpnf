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

#' Generate a classical TXT point and figure plot.
#' 
#' THIS FUNCTION IS STILL UNDER HEAVY DEVELOPMENT,
#' THEREFORE IT IS MASKED AND SUBJECT TO CHANGE!
#' 
#' @param data a data frame object containing point and figure informations to be plotted
#' @param ... any additional options for the plot command
.pnfplot.txt <- function(data,reversal=3,boxsize=1,log=FALSE,...) {
  # create connection object
  con <- file(description="testplot.txt",)
  # cat to connection object
  for (mybox in max(data$boxnumber):min(data$boxnumber)) {
    ### iterate over every line
    # write boxnumber, later we should better write lower bound on box  (TODO)
    #cat(round(rpnf:::.box2lower(mybox,boxsize=boxsize,log=log),digits=2))
    cat(format(rpnf:::.box2lower(mybox,boxsize=boxsize,log=log),width=8))
    cat("|")
    # iterate through columns
    for (column in min(data$column):max(data$column)) {
      status <- as.character(unique(data$status.xo[data$column==column]))
      mymin <- min(data$boxnumber[data$column==column])
      mymax <- max(data$boxnumber[data$column==column])
      # correct mymin and mymax, if necessary
      if (column>min(data$column)) {
        if (status=="X") 
          mymin <- min(data$boxnumber[data$column==column-1])+1
        else
          mymax <- max(data$boxnumber[data$column==column-1])-1
      }
      # decide on plot
      if (mymin<=mybox & mybox<=mymax)
        cat(status)
      else
        cat(" ")
    } # end column loop
    # write line feed
    cat("\n")
  }
}