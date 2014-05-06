#' Determine an appropriate boxsize, if you want to use logarithmic scale.
#'
#' This function returns an appropriate boxsize if you want to do your point and figure analysis with an logarithmic scale.
#' 
#' @param percent a numeric value defining the percent 
#' @return a numeric value which is equivalent to the percental change given on a logarithmic scale
#' @export
#' @examples
#' # apply it with pnfprocessor
#' library(rpnf) # Load rpnf library
#' data(GDAXI) # Load some example data
#' 
#' # return appropriate value for 1% boxsize
#' getLogBoxsize(percent=1)
#' 
#' pnfprocessor(
#'  high=GDAXI$High,
#'  low=GDAXI$Low,
#'  date=GDAXI$Date,
#'  boxsize=getLogBoxsize(percent=1),
#'  log=TRUE)
#'
getLogBoxsize <- function(percent) {
  if (!is.numeric(percent)) {
    stop("Argument percent has to be numeric!")
  }
  if (length(percent)>1) {
    warning("Argument percent for function getLogBoxsize() is a vector, and will return a vector!")
  }
  if (min(percent)<=0) {
    if (min(percent)<=-100) {
      warning("Argument percent contains values less equal than -100, this will introduce NaN's and strange results!")
    } else {
      warning("Argument percent contains values less equal than zero, this will introduce strange results!")
    }
  }
  return (log(100+percent)-log(100))
}

#' Converts a single or a vector of quotes into integer boxnumbers for P&F-Analysis.
#' 
#' @param quote a single quote, or a vector of quotes
#' @param boxsize single numeric value, used as the boxsize
#' @param log TRUE, if logarithmic scales should be used
#' @return a single or a vector of integer boxnumbers
#' This function transforms a given quote into an unique integer box number
quote2box <- function(quote, boxsize=1, log=FALSE) {
  if (!is.numeric(quote)) {
    stop("Argument quote has to be numeric!")
  }
  if (!is.numeric(boxsize)) {
    stop("Argument boxsize has to be numeric!")
  }
  if (!is.logical(log)) {
    stop("Argument log has to be logical")
  }
  if (log & min(quote)<=0) {
    stop("Argument quotes must be greater than zero, if log=TRUE!")
  }
  if (length(boxsize)>1){
    stop("Argument boxsize is vector of length greater than 1. This is not supported yet!")
  }
  
  if (log==TRUE) {
    mylog <- function(x) {
      log(x)
    }
  } else {
    mylog <- function(x) {
      x
    }
  }
  result <- as.integer(floor(mylog(quote)/boxsize))
  result
}

#' Returns the lower bound value for a given boxnumber
#' @param boxnumber An integer boxnumber 
#' @param boxsize single numeric value, used as the boxsize
#' @param log TRUE, if logarithmic scales should be used
box2lower <- function(boxnumber, boxsize=1, log=FALSE) {
  # FIXME improve sanity checks
  if(sum(floor(boxnumber)!=boxnumber,na.rm=T)>0)
    stop("Error: Only integer values allowed as boxnumber!")
  if (log==TRUE) {
    myexp <- function(x){
      exp(x)
    }
  } else {
    myexp <- function(x){
      x
    }
  }
  
  myexp(boxnumber*boxsize)
}

#' Returns the upper bound value for a given boxnumber
#' @param boxnumber An integer boxnumber 
#' @param boxsize single numeric value, used as the boxsize
#' @param log TRUE, if logarithmic scales should be used
box2upper <- function(boxnumber, boxsize=1, log=FALSE) {
  # FIXME improve sanity checks
  if(sum(floor(boxnumber)!=boxnumber,na.rm=T)>0)
    stop("Error: Only integer values allowed as boxnumber!")
  if (log) {
    myexp <- function(x){
      exp(x)
    }
  } else {
    myexp <- function(x){
      x
    }
  }
  
  myexp((boxnumber+1)*boxsize)
}

#' Determine the next box frontier for current quote(s) given a recent XO-status.
#' 
#' Note: offset should only be used for reversal calculation
#' @param quote  A single quote or a vector of quotes.
#' @param status A single character indicating the current XO-status.
#' @param boxsize A single numeric value, indicating the boxsize to be considered.
#' @param log TRUE, if logarithmic scales should be used.
#' @param offset A numeric value 
nextBox <- function(quote,status, boxsize=1, log=FALSE, offset=0) {
  if (!(is.numeric(quote))) {
    stop("Argument quote has to be numeric!")
  }
  if (!(is.character(status) & nchar(status)==1 )) {
    stop("Argument status has to be a character and of length 1!")
  }
  if (!(is.numeric(boxsize) & length(boxsize)==1)) {
    stop("Argument boxsize has to be numeric and of length 1!")
  }
  if(!(is.logical(log) & length(log))) {
    stop("Argument log has to be logical and of length 1!")
  }
  if (!(is.numeric(offset) & length(offset)==1)) {
    stop("Argument offset has to be numeric and of length 1!")
  }
  
  if (status == "X")
    box2upper(quote2box(quote=quote,boxsize=boxsize,log=log)+offset,boxsize=boxsize,log=log)
  else 
    box2lower(quote2box(quote=quote,boxsize=boxsize,log=log)+offset,boxsize=boxsize,log=log)
}

#' Determine the next reversal frontier for current quote(s) given a recent XO-status.
#' @param quote  A single quote or a vector of quotes.
#' @param status A single character indicating the current XO-status.
#' @param reversal number of boxes needed to make a reversal 
#' @param boxsize A single numeric value, indicating the boxsize to be considered.
#' @param log TRUE, if logarithmic scales should be used.
nextReversal <- function(quote,status, reversal=3, boxsize=1, log=FALSE) {
  if (status == "X")
    return (nextBox(quote, "O", boxsize=boxsize,log=log, offset=-reversal+1))
  else
    return (nextBox(quote, "X", boxsize=boxsize,log=log, offset=reversal-1))
}


#' Determine the XO development of a given time series.
#' 
#' This is the main PNF-Workhorse, which transforms a given time series into X and O's.
#' Furthermore it provides already simple Buy/Sell signals, based on checking if the second last colum.
#' @param high
#' @param low
#' @param date
#' @param reversal
#' @param boxsize A single numeric value, indicating the boxsize to be considered.
#' @param log TRUE, if logarithmic scales should be used.
#' @export
#' @keywords internal
#' @examples
#' library(rpnf) # Load rpnf library
#' data(GDAXI) # Load some example data
#' 
#' xo.processor(
#'  high=GDAXI$High,
#'  low=GDAXI$Low,
#'  date=GDAXI$Date,
#'  reversal=3L,
#'  boxsize=1,
#'  log=TRUE)
xo.processor <- function(high,low=high, date, reversal=3L, boxsize=1, log=FALSE) {
  if (!(is.numeric(high) & length(high)>=1)) {
    stop("Argument high has to be numeric and at least of length 1!")
  }
  if (!(is.numeric(low) & length(low)>=1)) {
    stop("Argument low has to be numeric and at least of length 1!")
  }
  # FIXME this is not working yet
  # if (!(is.numeric.Date(date) & length(date)>=1)) {
  #   stop("Argument date has to be a Date object and has at least of length 1!")
  # }
  if (!(length(high)==length(low))) {
    stop("Arguments high and low have to have the same length!")
  }
  if (!(length(high)==length(date))) {
    stop("Arguments high and date have to have the same length!")
  }
  if (!(is.numeric(reversal) & length(reversal)==1)) {
    stop("Argument reversal has to be numeric, and of length 1 exactly!")
  }
  if (!(is.integer(reversal) & min(reversal)>0)) {
    stop("Argument reversal has to be integer greater than zero!")
  }
  if (!(is.numeric(boxsize) & min(boxsize)>0)) {
    stop("Argument boxsize has to be numeric greater than zero, and of length 1 exactly!")
  }
  if (!(is.logical(log) & length(log)==1)) {
    stop("Argument log has to be numeric, and of length 1 exactly!")
  }

  # add additional columns
  status.xo <- rep(NA,length.out=length(high))  # current state of X/O signal
  boxnumber <- rep(NA,length.out=length(high))  # current integer box number (used for internal processes)
  status.bs <- rep(NA,length.out=length(high))  # current state of buy/sell signal
  nextX <- rep(NA,length.out=length(high))      # current nextX
  lastNextX <- rep(NA,length.out=length(high))  # remember last known nextX, necessary to identify buy/sell changes
  nextO <- rep(NA,length.out=length(high))      # current nextO
  lastNextO <- rep(NA,length.out=length(high))  # remember last known nextO, necessary to identify buy/sell changes
  column <- rep(NA,length.out=length(high))     # counter for current P&F column
  
  # TODO improve initialization
  status.xo[1] = "X"
  boxnumber[1] = rpnf:::quote2box(quote=high[1],boxsize=boxsize,log=log)
  status.bs[1] = "Buy"
  nextX[1] = high[1]
  lastNextX[1] = high[1]
  nextO[1] = low[1]
  lastNextO[1] = low[1]
  column[1] = 1
  
  # pnfprocessor
  for (i in 2:length(high)) {
    if (status.xo[i-1] == "X") {
      # we are in X-mode
      if (high[i] > nextX[i-1]) {
        # we made a new X
        status.xo[i] <- "X"
        boxnumber[i] = rpnf:::quote2box(quote=high[i],boxsize=boxsize,log=log)
        nextX[i] <- nextBox(high[i],"X", boxsize=boxsize,log=log)
        lastNextX[i] <- lastNextX[i-1]
        nextO[i] <- nextReversal(high[i],"X", boxsize=boxsize,log=log)
        lastNextO[i] <- lastNextO[i-1]
        column[i] = column[i-1]
        if (high[i] > lastNextX[i-1])
          status.bs[i] <- "Buy"
        else
          status.bs[i] <- status.bs[i-1]
      } else if (low[i] < nextO[i-1]) {
        # we made a reversal to O
        status.xo[i] <- "O"
        boxnumber[i] = rpnf:::quote2box(quote=low[i],boxsize=boxsize,log=log)
        nextX[i] <- nextReversal(low[i],"O", boxsize=boxsize,log=log)
        lastNextX[i] <- nextX[i-1]
        nextO[i] <- nextBox(low[i],"O", boxsize=boxsize,log=log)
        lastNextO[i] <- lastNextO[i-1]
        column[i] = column[i-1]+1
        if (low[i] < lastNextO[i-1])
          status.bs[i] <- "Sell"
        else
          status.bs[i] <- status.bs[i-1]
      } else {
        # nothing new happened
        status.xo[i] <- status.xo[i-1]
        boxnumber[i] = rpnf:::quote2box(quote=high[i],boxsize=boxsize,log=log)
        status.bs[i] <- status.bs[i-1]
        nextX[i] <- nextX[i-1]
        lastNextX[i] <- lastNextX[i-1]
        nextO[i] <- nextO[i-1]
        lastNextO[i] <- lastNextO[i-1]
        column[i] = column[i-1]
      }
    } else {
      # we are in O-mode
      if (low[i] < nextO[i-1]) {
        # we made a new O
        status.xo[i] <- "O"
        boxnumber[i] = rpnf:::quote2box(quote=low[i],boxsize=boxsize,log=log)
        nextO[i] <- nextBox(low[i],"O", boxsize=boxsize,log=log)
        lastNextO[i] <- lastNextO[i-1]
        nextX[i] <- nextReversal(low[i],"O", boxsize=boxsize,log=log)
        lastNextX[i] <- lastNextX[i-1]
        column[i] = column[i-1]
        if (low[i] < lastNextO[i-1])
          status.bs[i] <- "Sell"
        else
          status.bs[i] <- status.bs[i-1]        
      } else if (high[i] > nextX[i-1]) {
        # we made a reversal to X
        status.xo[i] <- "X"
        boxnumber[i] = rpnf:::quote2box(quote=high[i],boxsize=boxsize,log=log)
        nextO[i] <- nextReversal(high[i],"X", boxsize=boxsize,log=log)
        lastNextO[i] <- nextO[i-1]
        nextX[i] <- nextBox(high[i],"X", boxsize=boxsize,log=log)
        lastNextX[i] <- lastNextX[i-1]
        column[i] = column[i-1]+1
        if (high[i] > lastNextX[i-1])
          status.bs[i] <- "Buy"
        else
          status.bs[i] <- status.bs[i-1]
      } else {
        # nothing new happened
        status.xo[i] <- status.xo[i-1]
        boxnumber[i] = rpnf:::quote2box(quote=low[i],boxsize=boxsize,log=log)
        status.bs[i] <- status.bs[i-1]
        nextX[i] <- nextX[i-1]
        lastNextX[i] <- lastNextX[i-1]
        nextO[i] <- nextO[i-1]
        lastNextO[i] <- lastNextO[i-1]
        column[i] = column[i-1]
      }
    }
  }
  return (data.frame(date,high,low,boxnumber,column,status.xo,nextX,nextO,status.bs,lastNextX,lastNextO))
}
