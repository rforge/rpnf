library(quantmod)
library(rpnfext)

args <- commandArgs(trailingOnly = TRUE)
if (length(args)<1) {
  print("No commandline args given! Going for default GDAXI")
  args <- c("GDAXI")
}

pnflist <- list()

for (arg in args) {
  print(paste0("Processing commandline arg = ",arg))  
  
  # Read a yahoo symbols list for given index string
  symbols <- getSymbolsForIndexSymbols(indexSymbols="GDAXI",stockMarketIdentifier=".DE")
                                #read.csv(file=paste0(arg,".csv"),header=F,stringsAsFactors = F)

  for (symbol in symbols[,1]) {
    print(paste0("--Processing ",symbol))
    # get quotes from yahoo
    quotes <- quantmod::getSymbols(Symbols=symbol,auto.assign=F)
    # determine PNF statistics
    pnfdata <- rpnf::pnfprocessor(high=quotes[,2],
                                  low=quotes[,3],
                                  date=index(quotes),
                                  reversal=3L,
                                  boxsize=rpnf::getLogBoxsize(percent=1),
                                  log=T)
    pnfdata$symbol <- symbol
    # save pnfdata to csv
    write.csv(pnfdata,file=paste0("pnfdata/",symbol,".csv"))
    pnflist[symbol] <- pnfdata
    # Plot to file
    filename <- paste0("plots/", symbol, ".txt")
    sink(file=filename,type=c("output"),split=F)
    rpnf::pnfplottxt(data=pnfdata[pnfdata$column>=max(pnfdata$column)-80,],
                     reversal=3,
                     boxsize=rpnf::getLogBoxsize(percent=1),
                     log=T,
                     main=symbol)
    sink()
  }
  
}
