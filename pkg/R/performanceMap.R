library(colorRamps)

#' This function calculates an upper triangle performance map for a given time 
#' series.
#' 
CalcPerformanceMap <- function(quotes, column = "close", mode = c("abs","rel")) {
  s <- matrix(data=rep(x=as.numeric(quotes[,column]),
                       times=length(quotes[,column])),
              nrow=length(quotes[,column]),
              ncol=length(quotes[,column]))
  absdiff <- t(s)-s
  absdiff[lower.tri(absdiff,diag=T)] <- 0
  # modifiy result
  if (mode == "abs")
    result <- absdiff
  else if (mode == "rel") 
    result <- absdiff%*%diag(1/as.numeric(quotes[,column]))
  else # default fallback to abs
    result <- absdiff
  # efficiently remove lower part of matrix
  result[lower.tri(result,diag=T)] <- NA
  rownames(result)<-as.character(index(quotes))
  colnames(result)<-as.character(index(quotes))
  # return result
  result
}

#' This function plots a performance map as an heatmap.
#'
#' TODO: Create a proper colormap centered at 0 (perhaps via rescaling)
#' TODO: Label axes with date instead of numbers
#' @seealso \url{http://www.nytimes.com/interactive/2011/01/02/business/20110102-metrics-graphic.html}
PlotPerformanceMap <- function(quotes, column = "close", ...) {
  # determine performance map data
  map_abs <- CalcPerformanceMap(quotes,column,mode="abs")
  map_rel <- CalcPerformanceMap(quotes,column,mode="rel")
  # prepare plot
  oldpar<-par()
  par(mfrow=c(2,1))
  image(index(quotes),index(quotes),-t(map_abs),col=green2red(10),
        main="Absolute return of investment",
        xlab="End of investment",
        ylab="Start of investment")
  image(index(quotes),index(quotes),-t(map_rel),col=green2red(10),
        main="Relative return of investment",
        xlab="End of investment",
        ylab="Start of investment")
  par(oldpar)  
}