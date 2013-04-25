#' This example explains how to create Point & Figure Charts for all commodities of a given composite index, e.g. the GDAXI
#' Moreover relative strength and bullish percent charts are created, too.
#' 
### Initialize libraries
library(rpnf) # Load rpnf library
library(quantmod) # Load quantmod library
stockData <- new.env() #Make a new environment for quantmod to store data in

### Define wrapper for quantmod download function
downloadDatas <- function(symbols=c("GOOG")) {
  startDate = as.Date(Sys.Date()-3*365) #Specify period of time we are interested in
  endDate = as.Date(Sys.Date()+1) # End date is today
  getSymbols(symbols, env = stockData, src = "yahoo", from = startDate, to = endDate)   #Download the stock history (for all tickers)
  ### extract stock quotes from enviorment, rename them and store to data.frame 
  symbolQuotes <- data.frame()
  for (s in symbols) {
    symbolData <- eval(parse(text=paste("OHLC(stockData$",sub("^\\^","",s),")",sep="")))
    names(symbolData)<-c("open","high","low","close")
    date <- index(symbolData)
    #symbolData$symbol <- rep(s,length.out=nrow(symbolData))
    symbol<-rep(s,length.out=nrow(symbolData))
    symbolQuotes <- rbind(symbolQuotes,cbind(as.data.frame(symbolData),symbol,date))    
  }
  symbolQuotes
}

### Define wrapper for getting an appropriate symbol table
getSymbolTable <- function(index) {
  yahoo.url <- "http://finance.yahoo.com/d/quotes.csv?s="
  download.file(url=paste(yahoo.url,"@",index,"&f=snbaopl1&e=.csv",sep=""),destfile="temp.csv")
  # read temp.csv file
  symbol <- read.csv("temp.csv", header=F)[,1]
  # return result
  data.frame(index,symbol)
}

### The example code starts
boxsize <- getLogBoxsize(percent=2.5)
log <- TRUE
# Define (yahoo) index symbol (with "^") to be processed
index <- "^GDAXI" # e.g. GDAXI, DJI, see http://finance.yahoo.com for more
# Get list of composite symbols
symbolTable <- getSymbolTable(index)
# download stock quotes for index
indexQuotes <- downloadDatas(c(index))
indexPnf <- pnfprocessor(indexQuotes$high,indexQuotes$low,indexQuotes$date,boxsize=boxsize,log=log)
# download stock quotes for composite symbols
symbolQuotes <- downloadDatas(as.character(symbolTable$symbol))
# generate point and figure information for composite index
symbolPnf <- data.frame()
symbolRS <- data.frame()
rs <- data.frame()
for (symbol in symbolTable[,2]) {
  print(paste("processing symbol: ",symbol))
  data <- symbolQuotes[symbolQuotes$symbol==symbol,]
  rs <- merge(data,indexQuotes,by="date")
  symbolPnf <- rbind(symbolPnf, cbind(symbol,pnfprocessor(data$high,data$low,date=as.Date(data$date),boxsize=boxsize,log=log)))
  symbolRS <- rbind(symbolRS, cbind(symbol,pnfprocessor(rs$close.x/rs$close.y,date=as.Date(rs$date),boxsize=boxsize,log=log,style="rs")))
}
# generate bullish percent chart
bptable<-table(symbolPnf$date,symbolPnf$status.bs)
bp<-bptable[,"Buy"]/(bptable[,"Buy"]+bptable[,"Sell"])
bpPnf <- pnfprocessor(high=bp,date=as.Date(names(bp)),boxsize=0.02,log=FALSE,style="bp")
# generate ascending percent chart
asctable<-table(symbolPnf$date,symbolPnf$status.xo)
asc<-asctable[,"X"]/(asctable[,"X"]+asctable[,"O"])
ascPnf <- pnfprocessor(high=asc,date=as.Date(names(asc)),boxsize=0.02,log=FALSE,style="bp")
# generate plots
for (s in symbolTable[,2]) {
  print(paste("plotting symbol: ",s))
  #oldpar <- par()
  # create plotting canvas
  png(filename=paste(s,".png"),width=3200,height=1800)
  par(mfrow=c(4,1))
  # plot symbol chart
  pnfplot(symbolPnf[symbolPnf$symbol==s,],main=paste("Commodity chart: ",s))
  # plot symbol rs
  pnfplot(symbolRS[symbolRS$symbol==s,],main=paste("Relative strength chart: ",s," vs. ",index))
  # plot index bp
  pnfplot(bpPnf,main=paste("Bullish percent chart: ",index))
  # plot index asc
  pnfplot(ascPnf,main=paste("Ascending percent chart: ",index))
  # restore plot settings
  dev.off()
  #par(oldpar)
}

