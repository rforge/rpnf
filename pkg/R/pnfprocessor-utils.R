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
box2lower <- function(boxnumber, boxsize=1, log=FALSE) {
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
box2upper <- function(boxnumber, boxsize=1, log=FALSE) {
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
  # FIXME add check for string length == 1
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

# determine next reversal boundary for given value and status
nextReversal <- function(quote,status, reversal=3, boxsize=1, log=FALSE) {
  if (status == "X")
    return (nextBox(quote, "O", boxsize=boxsize,log=log, offset=-reversal+1))
  else
    return (nextBox(quote, "X", boxsize=boxsize,log=log, offset=reversal-1))
}

# returns the maximum box number in given column
maxBox <- function(redData,column) {
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

# returns true if given column c drops below prevois column of same type (this is always column c-2)
fallingBottom <- function(redData,column) {
  if (column-2>=1)
    return (minBox(redData,column)<minBox(redData,column-2))
  else
    return (FALSE)
}

# returns true if given column c exceeds previous column of same type (this is always column c-2)
raisingTop <- function(redData,column) {
  if (column-2>=1)
    return (maxBox(redData,column)>maxBox(redData,column-2))
  else
    return (FALSE)
}

# define locally needed functions

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

# determine pnf series for given data
.xo.processor <- function(high,low=high, date, reversal=3L, boxsize=1, log=FALSE) {
  # TODO implement sanity checks
  if (typeof(high)!="double")
    stop("ERROR: Typeof of parameter high must be double!")
  if (typeof(low)!="double")
    stop("ERROR: Typeof of parameter low must be double!")
  if (class(date)!="Date")
    stop("ERROR: Class of parameter date must be Date!")
  if (class(reversal)!="integer")
    stop("ERROR: Class of parameter reversal must be integer!")
  if (reversal <= 0)
    stop("ERROR: Value of parameter reversal must be a positive integer!")
  if (class(log)!="logical" | is.na(log))
    stop("ERROR: Value of parameter log must be either TRUE or FALSE!")
  if (length(high)!=length(low))
    stop("ERROR: length of high's must be as long as length of low's")
  if (!is.null(date) & length(high)!=length(date))
    stop("ERROR: length of high's must be as long as length of low's")
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

# This function analyzes a (preliminary) P&F Chart for Bullish Support Line and Bearish Resistance Line
# 
# Finding the appropriate trendlines is explained very good at \url{http://stockcharts.com/school/doku.php?id=chart_school:chart_analysis:pnf_charts:pnf_trendlines}.
# 
# @seealso \url{http://stockcharts.com/school/doku.php?id=chart_school:chart_analysis:pnf_charts:pnf_trendlines}
.rs.signal.processor <- function(data) {
  warning("This function is not implemented yet!")
  data
  # TODO implement function
}

# This function analyzes a (preliminary) P&F Chart for Bullish Support Line and Bearish Resistance Line
# 
# Finding the appropriate trendlines is explained very good at \url{http://stockcharts.com/school/doku.php?id=chart_school:chart_analysis:pnf_charts:pnf_trendlines}.
# 
# @seealso \url{http://stockcharts.com/school/doku.php?id=chart_school:chart_analysis:pnf_charts:pnf_trendlines}
.xo.trendline.processor <- function(data, slope=1) {
  # define local function which determines the slope of the trend lines
  trend.offset <- function(start.column,recent.column) {
    floor((recent.column-start.column)*slope)
  }
  data$tl.brl.boxnumber <- rep(x=NA,times=nrow(data)) # initialize bearish resistance line 
  data$tl.bsl.boxnumber <- rep(x=NA,times=nrow(data)) # initialize bullsih support line 
  data$tl.status <- rep(x=NA,times=nrow(data)) # initialize trendline indicator
  # as all charts start with X-column by default, we start with a bearish resistance line
  trend.start.column <- min(data$column,na.rm=T) # column number, where trendline starts
  trend.start.boxnumber <- max(data$boxnumber[data$column <= trend.start.column],na.rm=T)+1 # boxnumber, where current trend starts
  trend.status.bullish <- FALSE # current trendline is bullish
  
  for (i in 1:nrow(data)) {
    # processing may start erliest at second column
    if (data$column[i]>min(data$column)) {
      if (trend.status.bullish) {
        trend.current.boxnumber <- trend.start.boxnumber + trend.offset(trend.start.column,data$column[i])
        # check if current trendline is still valid, else start new bearish trend
        if (data$boxnumber[i] >= trend.current.boxnumber) {
          data$tl.bsl.boxnumber[i] <- trend.current.boxnumber 
          data$tl.status[i] <- "BULLISH"
        } else {
          # trend was broken, create new bearish trend
          trend.status.bullish <- FALSE
          trend.start.column <- data$column[i]
          trend.start.boxnumber <- max(data$boxnumber[data$column == trend.start.column-1],na.rm=T)
          data$tl.brl.boxnumber[i] <- trend.start.boxnumber
          data$tl.status[i] <- "BEARISH"
        }
      } else {
        trend.current.boxnumber <- trend.start.boxnumber - trend.offset(trend.start.column,data$column[i])
        # check if current trendline is still valid, else start new bullish trend
        if (data$boxnumber[i] <= trend.current.boxnumber) {
          data$tl.brl.boxnumber[i] <- trend.current.boxnumber
          data$tl.status[i] <- "BEARISH"
        } else {
          # trend was broken, create new bullish trend
          trend.status.bullish <- TRUE
          trend.start.column <- data$column[i]
          trend.start.boxnumber <- min(data$boxnumber[data$column == trend.start.column-1],na.rm=T)
          data$tl.bsl.boxnumber[i] <- trend.start.boxnumber
          data$tl.status[i] <- "BULLISH"
        }
      }
    }
  }
  # return result
  data
}

# Identifiy for a given P&F Table the current vertical price objective 
# triggered by the last signal reversal.
# 
# 
.currentVPOBreakoutMethod <- function(data,reversal,boxsize,log) {
  price.objective <- list(boxnumber=NA,price=NA)
  if (nrow(data)>=2) {
    # select only reversal days in data
    mydata <- data[c(FALSE,data$status.bs[1:(nrow(data)-1)]!=data$status.bs[2:nrow(data)]),]
    if (nrow(mydata)>0) {
      # identify current status.bs
      current.status <- data$status.bs[nrow(data)]
      # find latest reversal column
      reversal.column <- max(mydata$column,na.rm=T)
      # find minimum and maximum boxnumber of price objective column in original data
      min.boxnumber <- NA
      max.boxnumber <- NA
      if (current.status=="Buy") {
        max.boxnumber <- max(data$boxnumber[data$column==reversal.column],na.rm=T)
        min.boxnumber <- min(data$boxnumber[data$column==reversal.column-1],na.rm=T)+1      
      } else if (current.status=="Sell") {
        min.boxnumber <- min(data$boxnumber[data$column==reversal.column],na.rm=T)
        max.boxnumber <- max(data$boxnumber[data$column==reversal.column-1],na.rm=T)-1        
      } 
      # determine extension estimate
      # extension.estimate.in.boxes <- (max.boxnumber-min.boxnumber)*reversal
      # determine price objective box
      boxnumber <- NA
      price <- NA
      if (current.status=="Buy") {
        boxnumber <- min.boxnumber + (max.boxnumber-min.boxnumber+1)*reversal
        # translate price.objective.box into real number
        price <- rpnf:::box2lower(boxnumber=boxnumber,boxsize=boxsize,log=log)
      } else if (current.status=="Sell") {
        boxnumber <- max.boxnumber - (max.boxnumber-min.boxnumber+1)*(reversal-1)
        # translate price.objective.box into real number
        price <- rpnf:::box2upper(boxnumber=boxnumber,boxsize=boxsize,log=log)
      } else {
        # should not happen
        stop("Internal error in .currentVerticalPriceObjective()!")
      }
      price.objective <- list(boxnumber=boxnumber,price=price)
    }
  } 
  price.objective
}

# Identifiy for a given P&F Table the current vertical price objective 
# triggered by the last signal reversal.
# 
# 
.currentVPOReversalMethod <- function(data,reversal,boxsize,log) {  
  # define local function to identify price objective column
  getPriceObjectiveColumn <- function(data) {
    price.obj.column <- NA
    if (data$column[nrow(data)]-data$column[1]>=3) {
      if (data$status.bs[nrow(data)]=="Buy") {
        column.offset <- 0
        if (data$status.xo[nrow(data)]=="X")
          column.offset <- 1
        columns.to.be.checked <- seq(from=data$column[nrow(data)]-column.offset, to=data$column[1], by=-2)
        for (c in columns.to.be.checked) {
          if (fallingBottom(redData=data,column=c)) {
            price.obj.column <- c+1
            break
          }
        }
      } else if (data$status.bs[nrow(data)]=="Sell") {
        column.offset <- 0
        if (data$status.xo[nrow(data)]=="O")
          column.offset <- 1
        columns.to.be.checked <- seq(from=data$column[nrow(data)]-column.offset, to=data$column[1], by=-2)
        for (c in columns.to.be.checked) {
          if (raisingTop(redData=data,column=c)) {
            price.obj.column <- c+1
            break
          }
        }
      } else {
        stop("Invalid internal status detected!")
      }
    }
    price.obj.column
  }
  
  getPriceObjective <- function(data,min.boxnumber,max.boxnumber,reversal,log) {
    boxnumber <- NA
    price <- NA
    if (data$status.bs[nrow(data)]=="Buy") {
      boxnumber <- min.boxnumber + (max.boxnumber-min.boxnumber+1)*reversal
      # translate price.objective.box into real number
      price <- box2lower(boxnumber=boxnumber,boxsize=boxsize,log=log)
    } else if (data$status.bs[nrow(data)]=="Sell") {
      boxnumber <- max.boxnumber - (max.boxnumber-min.boxnumber+1)*(reversal-1)
      # translate price.objective.box into real number
      price <- box2upper(boxnumber=boxnumber,boxsize=boxsize,log=log)
    } else {
      # should not happen
      stop("Internal Error!")
    }
    list(boxnumber=boxnumber,price=price)
  }
  
  price.objective <- list(boxnumber=NA,price=NA)
  
  ### identify price.obj.column
  price.obj.column <- getPriceObjectiveColumn(data)
  
  if (!is.na(price.obj.column)) {
    # find minimum and maximum boxnumber of price objective column in original data
    min.boxnumber <- NA
    max.boxnumber <- NA
    if (data$status.bs[nrow(data)]=="Buy") {
      max.boxnumber <- max(data$boxnumber[data$column==price.obj.column],na.rm=T)
      min.boxnumber <- min(data$boxnumber[data$column==price.obj.column-1],na.rm=T)+1      
    } else if (data$status.bs[nrow(data)]=="Sell") {
      min.boxnumber <- min(data$boxnumber[data$column==price.obj.column],na.rm=T)
      max.boxnumber <- max(data$boxnumber[data$column==price.obj.column-1],na.rm=T)-1        
    } 
    
    ### determine price objective
    price.objective <- getPriceObjective(data,min.boxnumber,max.boxnumber,reversal,log)
  }
  price.objective
}

# This function adds Vertical Price Objectives calculated with the
# Bullish Breakout and Bearish Breakdown Method (BM) to an P&F Table.
# 
# Finding the appropriate price objectives is explained very good at 
# \url{http://stockcharts.com/school/doku.php?id=chart_school:chart_analysis:point_and_figure_pri}.
# The function adds columns vpo_bm_boxnumber and vpo_bm_price to the given
# P&F Table. vpo_bm_bonumber contains the boxnumber of the price objective,
# while vpo_bm_price contains the real price objective.
# 
# @seealso \url{http://stockcharts.com/school/doku.php?id=chart_school:chart_analysis:point_and_figure_pri}
.xo.priceobjective.processor <- function(data,reversal,boxsize,log) {
  warning("This function xo.priceobjective.processor() is not fully tested yet!")
  
  # add new column to store vertical price objective for breakout method (BM)
  data$vpo_bm_boxnumber <- rep(NA,times=nrow(data))
  data$vpo_bm_price <- rep(NA,times=nrow(data))
  # add new column to store vertical price objective for reversal method (RM)
  data$vpo_rm_boxnumber <- rep(NA,times=nrow(data))
  data$vpo_rm_price <- rep(NA,times=nrow(data))
  # loop over every in set
  for (i in 1:nrow(data)) {
    # subset current data
    mydata <- data[1:i,]
    # determine vertical price objective with breakout method
    vert.obj <-  .currentVPOBreakoutMethod(data=mydata,
                                           reversal=reversal,
                                           boxsize=boxsize,
                                           log=log) 
    data$vpo_bm_boxnumber[i] <- vert.obj$boxnumber
    data$vpo_bm_price[i] <- vert.obj$price
    # determine vertical price objective with breakout method
    vert.obj <-  .currentVPOReversalMethod(data=mydata,
                                           reversal=reversal,
                                           boxsize=boxsize,
                                           log=log) 
    data$vpo_rm_boxnumber[i] <- vert.obj$boxnumber
    data$vpo_rm_price[i] <- vert.obj$price
  }
  
  data
  # TODO implement function
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
