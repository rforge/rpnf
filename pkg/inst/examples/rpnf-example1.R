#' This is the example from the rpnf-package description
#' 
### Initialize library
library(rpnf) # Load rpnf library
data(GDAXI) # Load some example data
#View(GDAXI) # here highs and lows of german DAX30 stock index for 2-years

### First example: linear analysis
# Determine point and figure informations 
# for a linear chart with boxsize 100 points
symbol.pnf <- pnfprocessor(
  high=GDAXI$High,
  low=GDAXI$Low,
  date=GDAXI$Date,
  boxsize=100,
  log=FALSE)  

# Result of the pnfprocessor is a data table, 
# which can be viewed and exported easily.
symbol.pnf
#View(symbol.pnf)

# Moreover it can be plotted in a modern style (still very alpha, traditional style planned)
pnfplot(symbol.pnf,main="P&F Plot GDAXI (linear)")
pnfplottxt(symbol.pnf,boxsize=100,log=FALSE,main="P&F Plot GDAXI (linear)")

### Second example: logarithmc example
# For most stocks and indices it is useful
# to do the analysis on a logarithmic scale.
# This can be done with pnfprocessor, too. 
# Ensure to make use of the getLogBoxsize() function 
# for an appropriate boxsize of a logarithmic chart.
# Determine point and figure informations for a logarithmic chart with boxsize 1% 
symbol.pnf <- pnfprocessor(
  high=GDAXI$High,
  low=GDAXI$Low,
  date=GDAXI$Date,
  boxsize=getLogBoxsize(2),
  log=TRUE)  

# View the result
symbol.pnf
#View(symbol.pnf)

# or plot it as a chart
pnfplot(symbol.pnf,main="P&F Plot GDAXI (log)")
pnfplottxt(symbol.pnf,boxsize=getLogBoxsize(2),log=TRUE,main="P&F Plot GDAXI (log)")

### Additional examples
# Examples for additional uses cases like
# - relative strength vs index
# - bullish percent of an index
# - and many others 
# will follow soon.