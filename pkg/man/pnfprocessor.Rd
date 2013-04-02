\name{pnfprocessor}
\alias{pnfprocessor}
\title{Generate all point and figure informations for a given time series.}
\usage{
  pnfprocessor(high, low = high, date = NULL, reversal = 3,
    boxsize = 1, log = FALSE)
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
}
\value{
  returns a data table with all point and figure
  information in it
}
\description{
  Please ensure that high, low and date are all order a
}

