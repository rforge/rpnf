#' This function calculates an upper triangle performance map for a given time 
#' series.
#' 
CalcPerformanceMap <- function(quotes, column = "close") {
  start <- as.Date(index(quotes))
  end <- as.Date(index(quotes))
  s <- matrix(data=rep(x=as.numeric(data.xts[,column]),
                       times=length(data.xts[,column])),
              nrow=length(data.xts[,column]),
              ncol=length(data.xts[,column]))
  absdiff <- t(s)-s
  #absdiff<-colnames(start)
  #absdiff<-rownames(end)
  # efficiently remove lower part of matrix
  absdiff[lower.tri(absdiff,diag=T)] <- 0
  #View(absdiff)
  absdiff
}

# This function plots a performance map as an heatmap.
PlotPerformanceMap <- function(quotes, column = "close") {
  # determine performance map data
  map <- CalcPerformanceMap(quotes,column)
  # prepare plot
  oldpar<-par()
  par(mfrow=c(2,1))
  image(rownames(map),colnames(map),map)
  plot(index(quotes),quotes$close)
  lines(index(quotes),quotes$close)
  par(oldpar)  
}