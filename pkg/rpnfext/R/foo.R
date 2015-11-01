#' This function calculates an upper triangle performance map for a given time 
#' series.
CalcPerformanceMap <- function(quotes, column = "close") {
  start <- as.Date(index(quotes))
  end <- as.Date(index(quotes))
  s <- matrix(data=rep(x=as.numeric(quotes[,column]),
                       times=length(quotes[,column])),
              nrow=length(quotes[,column]),
              ncol=length(quotes[,column]))
  absdiff <- t(s)-s
  colnames(absdiff) <- as.character(index(quotes))
  rownames(absdiff) <- as.character(index(quotes))
  #absdiff<-colnames(start)
  #absdiff<-rownames(end)
  # efficiently remove lower part of matrix
  absdiff[lower.tri(absdiff,diag=T)] <- NA
  #View(absdiff)
  absdiff
}

CalcPerformanceMap2 <- function(quotes, column = "close") {
  start <- as.Date(index(quotes))
  end <- as.Date(index(quotes))
  s <- matrix(data=rep(x=as.numeric(quotes[,column]),
                       times=length(quotes[,column])),
              nrow=length(quotes[,column]),
              ncol=length(quotes[,column]))
  absdiff <- t(s)-s
  colnames(absdiff) <- as.character(index(quotes))
  rownames(absdiff) <- as.character(index(quotes))
  #absdiff<-colnames(start)
  #absdiff<-rownames(end)
  # efficiently remove lower part of matrix
  absdiff[lower.tri(absdiff,diag=T)] <- NA
  #View(absdiff)
  t(absdiff)
}

#' This function plots a performance map as an heatmap.
PlotPerformanceMap <- function(quotes, column = "close") {
  # determine performance map data
  map <- CalcPerformanceMap2(quotes,column)
  # prepare plot
  oldpar<-par()
  par(mfrow=c(2,1))
  color <- RColorBrewer::brewer.pal(n=11,name="RdYlGn")
  Einstieg <- as.Date(rownames(map))
  Ausstieg <- as.Date(colnames(map))
  image(x = Ausstieg, y=Einstieg, z = map, 
        zlim=c(-max(abs(map),na.rm=T),max(abs(map),na.rm=T)),
        col = color,
        )
  plot(index(quotes),quotes$close)
  lines(index(quotes),quotes$close)
  par(oldpar)  
}