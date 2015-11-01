library(quantmod)
library(data.table)
library(rpnfext)

args <- commandArgs(trailingOnly = TRUE)
if (length(args)<1) {
  print("No commandline args given! Going for default GDAXI")
  args <- c("GDAXI",".DE")
}

pnflist <- list()

print(paste0("Processing commandline indexSymbol = ",args[1:(length(args)-1)]))  

# Read a yahoo symbols list for given index string
symbols <- rpnfext::getSymbolsForIndexSymbols(indexSymbols=args[1:(length(args)-1)],stockMarketIdentifier=args[length(args)])

for (symbol in symbols) {
  print(paste0("--Processing ",symbol))
  # get quotes from yahoo
  quotes <- quantmod::getSymbols(Symbols=symbol,auto.assign=F)
  # determine PNF statistics
  pnfdata <- rpnf::pnfprocessor(high=quotes[,2],
                                low=quotes[,3],
                                date=index(quotes),
                                reversal=3L,
                                boxsize=rpnf::getLogBoxsize(percent=2),
                                log=T)
  pnfdata$symbol <- symbol
  # save pnfdata to csv
  write.csv(pnfdata,file=paste0("pnfdata/",symbol,".csv"))
  pnflist[[symbol]] <- pnfdata
  # Plot to file
  filename <- paste0("plots/", symbol, ".txt")
  sink(file=filename,type=c("output"),split=F)
  rpnf::pnfplottxt(data=pnfdata[pnfdata$column>=max(pnfdata$column)-80,],
                   reversal=3,
                   boxsize=rpnf::getLogBoxsize(percent=2),
                   log=T,
                   main=symbol)
  sink()
}

# save pnflist
save(pnflist,file = "pnflist.RData")

# convert pnflist into huge data.frame
pnfdata <- as.data.frame(data.table::rbindlist(pnflist))

##### delete from pnfdata all rows where date has not enough observations according to a 3 sigma rule
dplyr::count(pnfdata,date)
plot(dplyr::count(pnfdata,date))
counter <- dplyr::count(pnfdata,date)
datesToRemove <- counter[ counter[,2] < mean(unlist(dplyr::count(pnfdata,date)[,2])) - 3*sd(unlist(dplyr::count(pnfdata,date)[,2])), 1]
pnfdata <- pnfdata[ !pnfdata$date %in% datesToRemove$date, ]
plot(dplyr::count(pnfdata,date))

##### calculate meta-data for huge data.frame group by datum
baseCount <- dplyr::count(pnfdata,date)
colnames(baseCount) <- c("date","baseCount")
# X-percentage
xCount <- dplyr::count(pnfdata[ pnfdata$status.xo=='X', ],date)
colnames(xCount) <- c("date","xCount")
# Buy-percentage
buyCount <- dplyr::count(pnfdata[ pnfdata$status.bs=='Buy', ],date)
colnames(buyCount) <- c("date","buyCount")
# merge data
pnfCount <- dplyr::full_join(dplyr::full_join(baseCount,xCount),buyCount)
pnfCount[is.na(pnfCount)] <- 0
# calculate percentages
pnfCount$xPercentage <- pnfCount$xCount/pnfCount$baseCount
pnfCount$buyPercentage <- pnfCount$buyCount/pnfCount$baseCount
View(pnfCount)

##### determine and plot x-percentage
xPercentagePnf <- rpnf::pnfprocessor(high = pnfCount$xPercentage,
                                     date = pnfCount$date,
                                     reversal=3L,boxsize=0.02,log=F,style="bp")
filename <- paste0("plots/xPercentage.txt")
sink(file=filename,type=c("output"),split=F)
rpnf::pnfplottxt(xPercentagePnf[xPercentagePnf$column>=max(xPercentagePnf$column)-80,],reversal=3L,boxsize=0.02,log=F)
sink()

##### determine plot bullish-percentage
bullishPercent <- rpnf::pnfprocessor(high = pnfCount$buyPercentage,
                                     date = pnfCount$date,
                                     reversal=3L,boxsize=0.02,log=F,style="bp")
filename <- paste0("plots/bullishPercent.txt")
sink(file=filename,type=c("output"),split=F)
rpnf::pnfplottxt(bullishPercent[bullishPercent$column>=max(bullishPercent$column)-80,],reversal=3L,boxsize=0.02,log=F)
sink()


