\name{pnfplottxt}
\alias{pnfplottxt}
\title{Generate a classical TXT point and figure plot.}
\usage{
  pnfplottxt(data, reversal = 3, boxsize = 1, log = FALSE,
    main = NULL, sub = NULL)
}
\arguments{
  \item{data}{a data frame object containing point and
  figure informations to be plotted}

  \item{reversal}{number of boxes used in pnfprocessor}

  \item{boxsize}{the boxsize used in pnfprocessor}

  \item{log}{are calculations done in logarithmic mode}

  \item{main}{a string used as a main title of the chart}

  \item{sub}{a string used as a sub title of the chart}
}
\description{
  THIS FUNCTION IS STILL UNDER HEAVY DEVELOPMENT, THEREFORE
  IT IS SUBJECT TO CHANGE!
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
  \code{\link{pnfprocessor}}

  \code{\link{pnfplot}}
}

