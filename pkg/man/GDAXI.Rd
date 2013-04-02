\name{GDAXI}
\alias{GDAXI}
\docType{data}
\title{2-years of historic quotes for german DAX30 index.}
\description{
This data set contains the high and low quotes for 2-years of historic quotes for the german DAX30 stock index.
}
\usage{data(GDAXI)}
\format{
  A data frame with 770 observations on the following 3 variables.
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
