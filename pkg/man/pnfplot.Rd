\name{pnfplot}
\alias{pnfplot}
\title{Generate a modern point and figure plot}
\usage{
  pnfplot(data, ...)
}
\arguments{
  \item{data}{a data frame object containing point and
  figure informations to be plotted}

  \item{...}{any additional options for the plot command}
}
\description{
  Generate a modern point and figure plot
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

  \code{\link{pnfplottxt}}
}

