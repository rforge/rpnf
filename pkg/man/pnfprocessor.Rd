\name{pnfprocessor}
\alias{pnfprocessor}
\title{Generate all point and figure informations for a given time series.}
\usage{
  pnfprocessor(high, low = high, date, reversal = 3L,
    boxsize = 1, log = FALSE, style = "xo")
}
\arguments{
  \item{high}{a vector containing the high quotes}

  \item{low}{a (optional) vector containing the low quotes}

  \item{date}{a vector of dates the quotes belong}

  \item{reversal}{number of boxes needed to make a
  reversal}

  \item{boxsize}{the boxsize to be used}

  \item{log}{should we do the calculations on a logarithmic
  scale}

  \item{style}{the style the pnfprocessor is working with.
  Can be \{xo,rs,bp\}.}
}
\value{
  returns a data table with all point and figure
  information in it
}
\description{
  Please ensure that high, low and date are all ordered
  according to the Date column.
}
\examples{
library(rpnf) # Load rpnf library
data(GDAXI) # Load some example data
pnfdata <- pnfprocessor(
  high=GDAXI$High,
  low=GDAXI$Low,
  date=GDAXI$Date,
  boxsize=100L,
  log=FALSE)
tail(pnfdata)
pnfplottxt(pnfdata,boxsize=100L,log=FALSE)
pnfplot(pnfdata)
}
\references{
  \url{http://rpnf.r-forge.r-project.org}
}
\seealso{
  \code{\link{pnfplot}}

  \code{\link{pnfplottxt}}
}

