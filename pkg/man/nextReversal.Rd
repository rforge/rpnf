\name{nextReversal}
\alias{nextReversal}
\title{Determine the next reversal frontier for current quote(s) given a recent XO-status.}
\usage{
  nextReversal(quote, status, reversal = 3, boxsize = 1,
    log = FALSE)
}
\arguments{
  \item{quote}{A single quote or a vector of quotes.}

  \item{status}{A single character indicating the current
  XO-status.}

  \item{boxsize}{A single numeric value, indicating the
  boxsize to be considered.}

  \item{log}{TRUE, if logarithmic scales should be used.}
}
\description{
  Determine the next reversal frontier for current quote(s)
  given a recent XO-status.
}

