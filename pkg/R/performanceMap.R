library(colorRamps)

#' This function calculates an upper triangle performance map for a given time 
#' series.
#' @export
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

RescalePerformanceMap <- function(data, rescaling = c("binary","absolute maximum","signum maximum")) {
  if (sum(rescaling == "binary")>0) {
    data[!is.na(data) & data>0] <- 1
    data[!is.na(data) & data<0] <- -1
  } else if (sum(rescaling == "absolute maximum")>0) {
    max.value <- max(abs(data),na.rm=T)
    data[!is.na(data)] <- data[!is.na(data)]/max.value
  } else if (sum(rescaling == "signum maximum")>0) {
    data[!is.na(data) & data>0] <- data[!is.na(data) & data>0]/max(data,na.rm=T)
    data[!is.na(data) & data<0] <- -data[!is.na(data) & data<0]/min(data,na.rm=T)
  }
  data
}

#' This function plots a performance map as an heatmap.
#'
#' TODO: Create a proper colormap centered at 0 (perhaps via rescaling)
#' TODO: Label axes with date instead of numbers
#' @seealso \url{http://www.nytimes.com/interactive/2011/01/02/business/20110102-metrics-graphic.html}
PlotPerformanceMap <- function(quotes, column = "close", rescaling = "binary",  ...) {
  # determine performance map data
  map_abs <- CalcPerformanceMap(quotes,column,mode="abs")
  map_rel <- CalcPerformanceMap(quotes,column,mode="rel")
  # prepare plot
  oldpar<-par()
  par(mfrow=c(2,2))
  image(index(quotes),index(quotes),-t(RescalePerformanceMap(map_abs,rescaling)),col=green2red(7),
        sub="Absolute return of investment",
        xlab="End of investment",
        ylab="Start of investment")
  hist(map_abs)
  image(index(quotes),index(quotes),-t(RescalePerformanceMap(map_rel,rescaling)),col=green2red(7),
        sub="Relative return of investment",
        xlab="End of investment",
        ylab="Start of investment")
  hist(map_rel)
  par(oldpar)  
}