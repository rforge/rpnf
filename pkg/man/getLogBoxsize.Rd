\name{getLogBoxsize}
\alias{getLogBoxsize}
\title{Determine an appropriate boxsize, if you want to use logarithmic scale.}
\usage{
  getLogBoxsize(percent)
}
\arguments{
  \item{percent}{a numeric value defining the percent}
}
\value{
  a numeric value which is equivalent to the percental
  change given on a logarithmic scale
}
\description{
  This function returns an appropriate boxsize if you want
  to do your point and figure analysis with an logarithmic
  scale.
}
\examples{
# return appropriate value for 1\% boxsize
getLogBoxsize(percent=1)

# apply it with pnfprocessor
library(rpnf) # Load rpnf library
data(GDAXI) # Load some example data

pnfprocessor(
 high=GDAXI$High,
 low=GDAXI$Low,
 date=GDAXI$Date,
 boxsize=getLogBoxsize(percent=1),
 log=TRUE)
}

