\docType{package}
\name{rpnf-package}
\alias{rpnf}
\alias{rpnf-package}
\title{R Point and Figure Library}
\description{
  The R Point and Figure Library is a tool set to create
  and analyze Point & Figure Charts for given time series
  or data frame objects.
}
\details{
  \tabular{ll}{ Package: \tab rpnf\cr Type: \tab Package\cr
  Version: \tab 0.9.1\cr Date: \tab 2013-04-19\cr License:
  \tab GPL-3\cr Built: \tab R 2.15.3; ; 2013-04-15 07:37:11
  UTC; windows\cr }
}
\examples{
### Initialize library
library(rpnf) # Load rpnf library
data(GDAXI) # Load some example data
#View(GDAXI) # here highs and lows of german DAX30 stock index for 3-years

### First example: linear analysis
# Determine point and figure informations
# for a linear chart with boxsize 100 points
symbol.pnf <- pnfprocessor(
  high=GDAXI$High,
  low=GDAXI$Low,
  date=GDAXI$Date,
  boxsize=100L,
  log=FALSE)

# Result of the pnfprocessor is a data table,
# which can be viewed and exported easily.
tail(symbol.pnf)
#View(symbol.pnf)

# Moreover it can be plotted in a modern style (still very alpha, traditional style planned)
pnfplot(symbol.pnf,main="P&F Plot GDAXI (linear)")
# Or in the old TXT style
pnfplottxt(symbol.pnf,boxsize=100L,log=FALSE,main="P&F Plot GDAXI (linear)")

### Second example: logarithmc example
# For most stocks and indices it is useful
# to do the analysis on a logarithmic scale.
# This can be done with pnfprocessor, too.
# Ensure to make use of the getLogBoxsize() function
# for an appropriate boxsize of a logarithmic chart.
# Determine point and figure informations for a logarithmic chart with boxsize 2\\\%
symbol.pnf <- pnfprocessor(
  high=GDAXI$High,
  low=GDAXI$Low,
  date=GDAXI$Date,
  boxsize=getLogBoxsize(2),
  log=TRUE)

# View the result
tail(symbol.pnf)
#View(symbol.pnf)

# or plot it as a modern chart
pnfplot(symbol.pnf,main="P&F Plot GDAXI (log)")
# Or in the old traditional TXT style
pnfplottxt(symbol.pnf,boxsize=getLogBoxsize(2),log=TRUE,main="P&F Plot GDAXI (log)")

### Additional examples
# Examples for additional uses cases like
# - relative strength vs index
# - bullish percent of an index
# - and many others
# can be found in your local package library directory.
# Search for rpnf-example1.R, rpnf-example2.R and rpnf-example3.R.
}
\author{
  Sascha M. Herrmann
  \email{sascha.martin.herrmann@gmail.com}
}
\references{
  Project Home Page \url{http://rpnf.r-forge.r-project.org}

  Dorsey, Thomas J. Point and Figure Charting: The
  Essential Application for Forecasting and Tracking Market
  Prices. 3rd ed. Wiley Trading. Hoboken, N.J: John Wiley &
  Sons, 2007.

  German version, which is the base for the package:
  Dorsey, Thomas. Sicher anlegen mit point & figure: klare
  Signale mit einfachen Methoden. Munich: FinanzBuch-Verl.,
  2000.
}
\seealso{
  \code{\link{pnfprocessor}}

  \code{\link{pnfplot}}

  \code{\link{pnfplottxt}}
}
\keyword{package}

