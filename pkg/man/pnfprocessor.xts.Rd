\name{pnfprocessor.xts}
\alias{pnfprocessor.xts}
\title{Wrapper for using \link{pnfprocessor} directly with an xts-object.}
\usage{
  pnfprocessor.xts(xts, columns = c(2, 3), ...)
}
\arguments{
  \item{xts}{xts object, e.g. quotes returned by quantmod's
  getSymbol()}

  \item{columns}{columns to be used as high and low quotes
  series}

  \item{...}{additional parameters of function pnfprocessor
  (e.g. reversal, log, style, etc.)}
}
\value{
  returns a data table with all point and figure
  information in it
}
\description{
  By default the second column as parameter 'high', the
  third column as parameter 'low' and the index as
  parameter date of the xts object will be used.
}
\examples{
library(rpnf) # Load rpnf library
library(xts) # Load xts library
data(GDAXI) # Load some example data
xts <- xts(x=cbind(GDAXI$High,GDAXI$Low),order.by=GDAXI$Date) # Convert to xts object
pnfdata <- pnfprocessor.xts(xts=xts, columns = c(1,2), boxsize=100L,log=FALSE)
tail(pnfdata)
pnfplottxt(pnfdata,boxsize=100L,log=FALSE)
pnfplot(pnfdata)
}
\references{
  \url{http://rpnf.r-forge.r-project.org}
}
\seealso{
  \code{\link{pnfprocessor}}
}

