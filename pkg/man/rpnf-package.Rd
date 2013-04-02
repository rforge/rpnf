\name{rpnf-package}
\alias{rpnf-package}
\alias{rpnf}
\docType{package}
\title{
Point and Figure Library
}
\description{
The R Point & Figure Library is a tool-set to create and analyze Point & Figure Charts for given time series or data frame objects.
}
\details{
\tabular{ll}{
Package: \tab rpnf\cr
Type: \tab Package\cr
Version: \tab 0.0.1\cr
Date: \tab 2013-03-31\cr
License: \tab GPL-3\cr
Built: \tab R 2.15.3; ; 2013-04-02 07:37:11 UTC; windows\cr
}

Index:
\preformatted{
rpnf-package            Point and Figure Library
}
~~ TODO An overview of how to use the package, including the most ~~
~~ important functions ~~
}
\author{
Sascha M. Herrmann

Maintainer: Sascha M. Herrmann <sascha.martin.herrmann@gmail.com>
}
\references{
Dorsey, Thomas J. Point and Figure Charting: The Essential Application for Forecasting and Tracking Market Prices. 3rd ed. Wiley Trading. Hoboken, N.J: John Wiley & Sons, 2007.

German version, which is the base for the package:
Dorsey, Thomas. Sicher anlegen mit point & figure: klare Signale mit einfachen Methoden. Munich: FinanzBuch-Verl., 2000.
}
\keyword{ package }
\examples{
library(rpnf)
data(GDAXI)
# Determine PNF-Informations for a linear Chart with boxsize 100 points
symbol.pnf <- pnfprocessor(high=GDAXI$High,low=GDAXI$Low,date=GDAXI$Date,boxsize=100,log=FALSE)  
pnfplot(symbol.pnf,main="P&F Plot GDAXI (linear)")

# Determine PNF-Informations for a linear Chart with boxsize 100 points
symbol.pnf <- pnfprocessor(high=GDAXI$High,low=GDAXI$Low,date=GDAXI$Date,boxsize=getLogBoxsize(1),log=TRUE)  
pnfplot(symbol.pnf,main="P&F Plot GDAXI (log)")
}
