#' Generate a classical TXT point and figure plot.
#' 
#' THIS FUNCTION IS STILL UNDER HEAVY DEVELOPMENT,
#' THEREFORE IT IS SUBJECT TO CHANGE!
#' 
#' @param data a data frame object containing point and figure informations to be plotted
#' @param reversal number of boxes used in pnfprocessor
#' @param boxsize the boxsize used in pnfprocessor
#' @param log are calculations done in logarithmic mode
#' @param main a string used as a main title of the chart
#' @param sub a string used as a sub title of the chart
#' @seealso \code{\link{pnfprocessor}}
#' @seealso \code{\link{pnfplot}}
#' @export
pnfplottxt <- function(data,reversal=3,boxsize=1,log=FALSE,main=NULL,sub=NULL) {
  ## local function definiton: plot seperation line
  plotSeperationLine <- function(numOfColumns) {
    # iterate through columns
    cat("--------+")
    for (column in 1:numOfColumns) 
      cat("-")    
    cat("\n")
  }
  ## Write main and sub title
  if (!is.null(main))
    cat(paste(main,"\n"))
  if (!is.null(sub))
    cat(paste(sub,"\n"))
  # plot seperation line
  plotSeperationLine(max(data$column)-min(data$column)+1)
  # cat to connection object
  for (mybox in max(data$boxnumber):min(data$boxnumber)) {
    ### iterate over every line
    # write boxnumber, later we should better write lower bound on box  (TODO)
    #cat(round(rpnf:::.box2lower(mybox,boxsize=boxsize,log=log),digits=2))
    cat(format(round(rpnf:::.box2lower(mybox,boxsize=boxsize,log=log),2),width=8))
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
  # plot seperation line
  plotSeperationLine(max(data$column)-min(data$column)+1)
  ## write date lines, but vertical
  for (pos in 1:10) {
    if (pos %in% c(1,2,3,4))
      cat("       Y|")
    else if (pos %in% c(6,7))
      cat("       M|")
    else if (pos %in% c(9,10))
      cat("       D|")
    else
      cat("        |")
    for (column in min(data$column):max(data$column)) {
      if (pos == 5 | pos == 8)
        cat(" ")
      else
        cat(substr(as.character(min(data$date[data$column==column])),start=pos,stop=pos))
    }
    cat("\n")
  }
}