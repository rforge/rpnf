---
title: "rpnf"
author: "Sascha Herrmann <sascha.herrmann.consulting@gmail.com>"
date: "October 6, 2015"
output: html_document
---

# Purpose of **rpnf**

**rpnf** is a collection of R functions for creating Point & Figure (P&F) charts and statistics for a given time series.

# Features of **rpnf**

**rpnf** allows the creation of P&F charts and statistics with the following features

* X/O status processing of time series with
    + configurable box size
    + configurable reversal 
    + linear/logarithmic scales
* Determiniation of next X/O or reversal O/X points 
* Buy/Sell status
* Buy/Sell signal detection, currently
    + Double Top/Bottom
    + Triple Top/Bottom
    + Bullish/Bearish signal
    + Bullish/Bearish catapult
    + etc.

# How to install **rpnf**

1. We are asuming you have R installed and working. If not please consult one of well written howto's. 
2. Go to the R console, or your favorite R-IDE (we recommend http://www.rstudio.com)
3. Install latest stable version of **rpnf** with
```
install.packages("rpnf")
```
4. **rpnf** is now installed, and you can continue with the getting started section.

#### Getting the latest development version from Rforge

If you want to use (for a good reason) the latest development version from Rforge, then you can install it with:
```
install.packages("rpnf", repos="http://R-Forge.R-project.org")
```

# How to get started with **rpnf**

Once **rpnf** is installed you can start using it by loading the library:

```
library(rpnf) # Load rpnf library
```

A typical workflow for using **rpnf** consists of three steps:

* Preparing some time series data
* Determining the P&F statistics for this time series
* Plotting the P&F chart for this time series

### Getting some data

**rpnf** has some sample data built in:
```
data(GDAXI)   # Load some example data
```
You can have a look at these data with ```str(GDAXI)``` or ```GDAXI```.

### Determining the P&F statistics

To determine the P&F statistics of a time series you use:
```
pnfdata <- pnfprocessor(
  high=GDAXI$High,
  low=GDAXI$Low,
  date=GDAXI$Date,
  boxsize=100L,
  log=FALSE)
```

### Understanding the P&F statistics
### Generating P&F plots


# Support

If you need commerical support feel free to contact Sascha Herrmann <sascha.herrmann.consulting@gmail.com> for an offer. You can also request a new feature development there.

For non-commerical support please refer to the r-forge forums.

# Development roadmap

Our current focus is to develop a set of wrappers and extensions to **rpnf**, so that it gets easier to exploit the full functionality of **rpnf** for creating Bullish Percent, Relative Strenght Charts, and others.


<!-- Start of StatCounter Code for Netscape Composer -->
<script type="text/javascript">
var sc_project=8875301; 
var sc_invisible=1; 
var sc_security="e94b74b3"; 
var scJsHost = (("https:" == document.location.protocol) ?
"https://secure." : "http://www.");
document.write("<sc"+"ript type='text/javascript' src='" +
scJsHost+
"statcounter.com/counter/counter.js'></"+"script>");
</script>
<noscript><div class="statcounter"><a title="web statistics"
href="http://statcounter.com/free-web-stats/"
target="_blank"><img class="statcounter"
src="http://c.statcounter.com/8875301/0/e94b74b3/1/"
alt="web statistics"></a></div></noscript>
<!-- End of StatCounter Code for Netscape Composer -->
