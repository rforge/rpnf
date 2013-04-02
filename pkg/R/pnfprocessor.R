#' Generate all point and figure informations for a given time series.
#' 
#' Please ensure that high, low and date are all order a
#' 
#' @param high a vector containing the high quotes
#' @param low a (optional) vector containing the low quotes
#' @param date a vector of dates the quotes belong
#' @param reversal number of boxes needed to make a reversal 
#' @param boxsize the boxsize to be used
#' @param log should we do the calculations on a logarithmic scale
#' @return returns a data table with all point and figure information in it
#' @export
pnfprocessor <- function(high,low=high,date=NULL,reversal=3, boxsize=1, log=FALSE) {
  result <- .xo.processor(high,low,date,reversal,boxsize,log)
  result2 <- .xo.signalprocessor(result,reversal)
  result2
}

# determine pnf series for given data
.xo.processor <- function(high,low=high, date=NULL, reversal=3, boxsize=1, log=FALSE) {
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
  boxnumber[1] = rpnf:::.quote2box(quote=high[1],boxsize=boxsize,log=log)
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
        boxnumber[i] = rpnf:::.quote2box(quote=high[i],boxsize=boxsize,log=log)
        nextX[i] <- .nextBox(high[i],"X", boxsize=boxsize,log=log)
        lastNextX[i] <- lastNextX[i-1]
        nextO[i] <- .nextReversal(high[i],"X", boxsize=boxsize,log=log)
        lastNextO[i] <- lastNextO[i-1]
        column[i] = column[i-1]
        if (high[i] > lastNextX[i-1])
          status.bs[i] <- "Buy"
        else
          status.bs[i] <- status.bs[i-1]
      } else if (low[i] < nextO[i-1]) {
        # we made a reversal to O
        status.xo[i] <- "O"
        boxnumber[i] = rpnf:::.quote2box(quote=low[i],boxsize=boxsize,log=log)
        nextX[i] <- .nextReversal(low[i],"O", boxsize=boxsize,log=log)
        lastNextX[i] <- nextX[i-1]
        nextO[i] <- .nextBox(low[i],"O", boxsize=boxsize,log=log)
        lastNextO[i] <- lastNextO[i-1]
        column[i] = column[i-1]+1
        if (low[i] < lastNextO[i-1])
          status.bs[i] <- "Sell"
        else
          status.bs[i] <- status.bs[i-1]
      } else {
        # nothing new happened
        status.xo[i] <- status.xo[i-1]
        boxnumber[i] = rpnf:::.quote2box(quote=high[i],boxsize=boxsize,log=log)
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
        boxnumber[i] = rpnf:::.quote2box(quote=low[i],boxsize=boxsize,log=log)
        nextO[i] <- .nextBox(low[i],"O", boxsize=boxsize,log=log)
        lastNextO[i] <- lastNextO[i-1]
        nextX[i] <- .nextReversal(low[i],"O", boxsize=boxsize,log=log)
        lastNextX[i] <- lastNextX[i-1]
        column[i] = column[i-1]
        if (low[i] < lastNextO[i-1])
          status.bs[i] <- "Sell"
        else
          status.bs[i] <- status.bs[i-1]        
      } else if (high[i] > nextX[i-1]) {
        # we made a reversal to X
        status.xo[i] <- "X"
        boxnumber[i] = rpnf:::.quote2box(quote=high[i],boxsize=boxsize,log=log)
        nextO[i] <- .nextReversal(high[i],"X", boxsize=boxsize,log=log)
        lastNextO[i] <- nextO[i-1]
        nextX[i] <- .nextBox(high[i],"X", boxsize=boxsize,log=log)
        lastNextX[i] <- lastNextX[i-1]
        column[i] = column[i-1]+1
        if (high[i] > lastNextX[i-1])
          status.bs[i] <- "Buy"
        else
          status.bs[i] <- status.bs[i-1]
      } else {
        # nothing new happened
        status.xo[i] <- status.xo[i-1]
        boxnumber[i] = rpnf:::.quote2box(quote=low[i],boxsize=boxsize,log=log)
        status.bs[i] <- status.bs[i-1]
        nextX[i] <- nextX[i-1]
        lastNextX[i] <- lastNextX[i-1]
        nextO[i] <- nextO[i-1]
        lastNextO[i] <- lastNextO[i-1]
        column[i] = column[i-1]
      }
    }
  }
  if (is.null(date))
    date <- as.Date(rownames(data.frame(high)))
  return (data.frame(date,high,low,boxnumber,column,status.xo,nextX,nextO,status.bs,lastNextX,lastNextO))
}

.xo.signalprocessor <- function(data, reversal=3) {
  # check for needed columns
  if (!"boxnumber" %in% names(data))
    stop("column 'boxnumber' is missing!")
  if (!"column" %in% names(data))
    stop("column 'column' is missing!")
  if (!"status.xo" %in% names(data))
    stop("column 'status.xo' is missing!")
  if (!"date" %in% names(data))
    stop("column 'date' is missing!")
  if ("signal.bs" %in% names(data))
    warning("column 'signal.bs' already exists, will be overriden!")
  
  # define locally needed functions
  
  # returns the maximum box number in given column
  maxBox <- function(redData,column){
    # TODO we have to make a difference between X and O columns!
    # max will be inaccurate for O columns
    max(redData$boxnumber[redData$column==column])
  }
  # returns the minimum box number in given column
  minBox <- function(redData,column){
    # TODO we have to make a difference between X and O columns!
    # min will be inaccurate for X columns
    min(redData$boxnumber[redData$column==column])
  }
  # returns true if given column c exceeds previous column of same type (this is always column c-2)
  raisingTop <- function(redData,column) {
    if (column-2>=1)
      return (maxBox(redData,column)>maxBox(redData,column-2))
    else
      return (FALSE)
  }
  # returns true if given column c matches exactly previous column of same type (this is always column c-2)
  doubleTop <- function(redData,column) {
    if (column-2>=1)
      return (maxBox(redData,column)==maxBox(redData,column-2))
    else
      return (FALSE)
  }
  # returns true if given column c drops below previous column of same type (this is always column c-2)
  fallingTop <- function(redData,column) {
    if (column-2>=1)
      return (maxBox(redData,column)<maxBox(redData,column-2))
    else
      return (FALSE)
  }
  # returns true if given column c drops below prevois column of same type (this is always column c-2)
  fallingBottom <- function(redData,column) {
    if (column-2>=1)
      return (minBox(redData,column)<minBox(redData,column-2))
    else
      return (FALSE)
  }
  # returns true if given column c matches exactly previous column of same type (this is always column c-2)
  doubleBottom <- function(redData,column) {
    if (column-2>=1)
      return (minBox(redData,column)==minBox(redData,column-2))
    else
      return (FALSE)
  }
  # returns true if given column c exceeds prevois column of same type (this is always column c-2)
  raisingBottom <- function(redData,column) {
    if (column-2>=1)
      return (minBox(redData,column)>minBox(redData,column-2))
    else
      return (FALSE)
  }
  
  #
  # initialize decision tree and first signals
  data$signal.bs[1] <- "UNDEFINED"
  
  #
  # For every row in data go through decision tree
  for (i in 2:nrow(data)){
    if (data$status.xo[i] == "X") {
      # check X-branch of decision tree
      if (raisingTop(data[data$date<=data$date[i],],data$column[i])) {
        # we have at least DOUBLE TOP
        data$signal.bs[i] <- "DOUBLE TOP"
        if (raisingBottom(data[data$date<=data$date[i],],data$column[i]-1)) {
          data$signal.bs[i] <- "BULLISH SIGNAL"
          if (raisingTop(data[data$date<=data$date[i],],data$column[i]-2)) {
            data$signal.bs[i] <- "TRIPLE BULLISH SIGNAL"
            if ((doubleBottom(data[data$date<=data$date[i],],data$column[i]-3) & 
                   doubleTop(data[data$date<=data$date[i],],data$column[i]-4))) {
              data$signal.bs[i] <- "BULLISH CATAPULT"
            } 
          } else if (fallingTop(data[data$date<=data$date[i],],data$column[i]-2)) {
            data$signal.bs[i] <- "BULLISH TRIANGLE"
          }
        } else if (doubleTop(data[data$date<=data$date[i],],data$column[i]-2)) {
          data$signal.bs[i] <- "TRIPLE TOP"
        } else if ((fallingBottom(data[data$date<=data$date[i],],data$column[i]-1) &
                      fallingTop(data[data$date<=data$date[i],],data$column[i]-2) &
                      fallingBottom(data[data$date<=data$date[i],],data$column[i]-3))) {
          data$signal.bs[i] <- "BEARISH SIGNAL REVERSED"
        } 
        # TODO low pole is a tricky one!!
        #       } else if (((data$column[i]>=4) & 
        #                   (minBox(data[data$date<=data$date[i],],data$column[i]-3)-reversal<=minBox(data[data$date<=data$date[i],],data$column[i]-1)) &
        #                   (2*(maxBox(data[data$date<=data$date[i],],data$column[i])-minBox(data[data$date<=data$date[i],],data$column[i]-2))>(maxBox(data[data$date<=data$date[i],],data$column[i]-1)-minBox(data[data$date<=data$date[i],],data$column[i]-1))))) {
        #         data$signal.bs[i] <- "LOW POLE"
      } else if (FALSE) {
        # TODO insert condtions for BEAR TRAP
        data$signal.bs[i] <- "BEAR TRAP"
      } else {
        # default case: previous signal is still valid
        data$signal.bs[i] <- data$signal.bs[i-1]
      }
    } else {
      # check O-branch of decision tree
      if (fallingBottom(data[data$date<=data$date[i],],data$column[i])) {
        # we have at least DOUBLE BOTTOM
        data$signal.bs[i] <- "DOUBLE BOTTOM"
        if (fallingTop(data[data$date<=data$date[i],],data$column[i]-1)) {
          data$signal.bs[i] <- "BEARISH SIGNAL"
          if (fallingBottom(data[data$date<=data$date[i],],data$column[i]-2)) {
            data$signal.bs[i] <- "TRIPLE BEARISH SIGNAL"
            if ((doubleTop(data[data$date<=data$date[i],],data$column[i]-3) &
                   doubleBottom(data[data$date<=data$date[i],],data$column[i]-4))) {
              data$signal.bs[i] <- "BEARISH CATAPULT"
            }
          } else if (raisingBottom(data[data$date<=data$date[i],],data$column[i]-2)) {
            data$signal.bs[i] <- "BEARISH TRIANGLE"
          }
        } else if (doubleBottom(data[data$date<=data$date[i],],data$column[i]-2)) {
          data$signal.bs[i] <- "TRIPLE BOTTOM"
        } else if ((raisingTop(data[data$date<=data$date[i],],data$column[i]-1) & 
                      raisingBottom(data[data$date<=data$date[i],],data$column[i]-2) &
                      raisingTop(data[data$date<=data$date[i],],data$column[i]-3))) {
          data$signal.bs[i] <- "BULLISH SIGNAL REVERSED"
        }
      } else if (FALSE) {
        # TODO insert condtions for HIGH POLE, this is a tricky one!!!
        data$signal.bs[i] <- "HIGH POLE"
      } else if (FALSE) {
        # TODO insert condtions for BULL TRAP
        data$signal.bs[i] <- "BULL TRAP"
      } else {
        # default case: previous signal is still valid
        data$signal.bs[i] <- data$signal.bs[i-1]
      } 
    }
    data$signal.bs[i]
  } # end for (i in 2:nrow(data))
  data
}

# This function identifies chart signals in an [0,100]-Points Bullish Percent Chart
.bp.signalprocessor <- function(data) {
  for (i in 1:nrow(data)) {
    if (data$status.xo[i]=="X") {
      # we are in x-mode
      if (data$status.bs[i]=="Buy") {
        # we have a buy signal
        data$signal[i] <- "Bull Confirmed"
      } else {
        # we have a sell signal
        if (data$high[i] <= 30 | (i>1 & data$signal[i-1]=="Bull Alert")) {
          data$signal[i] <- "Bull Alert"
        } else {
          data$signal[i] <- "Bear Correction"
        }
      }
    } else {
      # we are in O-mode
      if (data$status.bs[i]=="Sell") {
        # we have a sell signal
        data$signal[i] <- "Bear Confirmed"
      } else {
        # we have a buy signal
        if (data$high[i] >= 70 | (i>1 & data$signal[i-1]=="Bear Alert")) {
          data$signal[i] <- "Bear Alert"
        } else {
          data$signal[i] <- "Bull Correction"
        }
      }
    }
  }
  data
}

# analyze transitions of signal states
.signalanalyzer <- function(signal,probability=TRUE) {
  t2<-table(signal[1:(length(signal)-1)],signal[2:length(signal)])
  t2m<-as.matrix(t2)
  for (i in 1:nrow(t2m)) t2m[i,i]=0
  if (probability)
    for (i in 1:nrow(t2m)) t2m[i,]=t2m[i,]/sum(t2m[i,])
  # replace 0 with NA
  for (i in 1:length(t2m))
    if (t2m[i]==0)
      t2m[i]<-NA
  t2m
}
