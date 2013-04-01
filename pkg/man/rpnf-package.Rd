\name{rpnf-package}
\alias{rpnf-package}
\alias{rpnf}
\docType{package}
\title{
Point & Figure Library
}
\description{
The R Point & Figure Library is a tool-set to create and analyze Point & Figure Charts for given time series or data frame objects.}
\details{
\tabular{ll}{
Package: \tab rpnf\cr
Type: \tab Package\cr
Version: \tab 0.0.1\cr
Date: \tab 2013-03-31\cr
License: \tab GPL-3\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Sascha M. Herrmann

Maintainer: Sascha M. Herrmann <sascha.martin.herrmann@gmail.com>
}
\references{
Dorsey, Thomas J. Point and Figure Charting: The Essential Application for Forecasting and Tracking Market Prices. 3rd ed. Wiley Trading. Hoboken, N.J: John Wiley & Sons, 2007.

German version, which is the base for the package:
Dorsey, Thomas. Sicher anlegen mit point & figure: klare Signale mit einfachen Methoden. München: FinanzBuch-Verl., 2000.
}
\keyword{ package }
\seealso{
#~~ Optional links to other man pages, e.g. ~~
#~~ \code{\link[<quantmod>:<quantmod>-package]{<quantmod>}} ~~
}
\examples{
## download some recent data with help of package quantmod
# library("quantmod")  #Load the quantmod Library
# stockData <- new.env() #Make a new environment for quantmod to store data in
# getSymbols(c("^DJI"), env = stockData, src = "yahoo", from = as.Date(Sys.Date()-3*365), to = as.Date(Sys.Date())) #Download the stock history (for all tickers)
# data <- OHLC(stockData$DJI) # retrieve data

# library(rpnf)
## Determine PNF-Informations for a linear Chart with boxsize 100 points
# symbol.pnf <- pnfprocessor(high=data$DJI.High,low=data$DJI.Low,date=index(data),boxsize=100,log=FALSE)  
# pnfplot(symbol.pnf,main="P&F Plot DJI (linear)")

## Determine PNF-Informations for a linear Chart with boxsize 100 points
# symbol.pnf <- pnfprocessor(high=data$DJI.High,low=data$DJI.Low,date=index(data),boxsize=getLogBoxsize(1),log=TRUE)  
# pnfplot(symbol.pnf,main="P&F Plot DJI (log)")
}
