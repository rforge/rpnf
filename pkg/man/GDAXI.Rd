\name{GDAXI}
\alias{GDAXI}
\docType{data}
\title{3-years of historic quotes for german DAX30 index.}
\description{
This data set contains the high and low quotes for 2-years of historic quotes for the german DAX30 stock index.
}
\usage{data(GDAXI)}
\format{
  A data frame with 770 observations of highs and lows for the german DAX30 stock index from May 2010 until March 2013.
  \describe{
    \item{\code{Date}}{a Date vector}
    \item{\code{High}}{a numeric vector containing highest value per day}
    \item{\code{Low}}{a numeric vector containing lowest value per day}
  }
}
\examples{
data(GDAXI)
## maybe str(GDAXI) ; plot(GDAXI) ...
}
\keyword{datasets}
