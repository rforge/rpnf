#' Downloads symbols for given Yahoo index symbol by scraping the composite page. 
#'
#' @param indexSymbol An yahoo index symbol,e.g. ^DJI or ^GDAXI
#'
#' @return A character vector with all symbols contained in the given index.
#' @export
#' @import httr
#' @import stringr
#'
#' @examples
getComponentSymbolsForIndexSymbol <- function(indexSymbol = "^GDAXI") {
  # sanity checks
  if (!is.character(indexSymbol)) {
    stop("Parameter indexSymbol has to be character!")
  }
  if (length(indexSymbol)!=1) {
    stop("Parameter indexSymbol has to contain exactly one indexSymbol!")
  }

  # execute download, while result is non-empty
  # this way we query indexes with more than 50 commodities correctly
  result <- character()
  pageCounter <- 0L
  continueWhile <- TRUE
  while (continueWhile) {
    # get index composition page from Yahoo
    retrieved <- httr::GET(paste0("https://de.finance.yahoo.com/q/cp?s=",indexSymbol,"&c=",pageCounter)) 
    # check status code
    if (httr::status_code(retrieved)!=200) {
      stop(paste0("Error while retrieving page from Yahoo! Status code = ",httr::status_code(retrieved)))  
    }
    # extract content
    content <- httr::content(retrieved,"text")

    # extract symbols from links
    pattern <- "<a href=\"/q\\?s=([A-Z0-9-]+(\\.[A-Z]+)?)\">"
    subExtract <- stringr::str_extract_all(string = content,pattern = pattern)
    subExtract
    localResult <- stringr::str_replace_all(string = unlist(subExtract),pattern = pattern,replacement = "\\1")
    localResult
    length(localResult)
    if (length(localResult)>0) {
      if ( all(localResult %in% result) ) {
        # retrieved only symbols already existing
        continueWhile <- FALSE
      }
      result <- c(result,localResult)
      pageCounter = pageCounter+1
    } else {
      continueWhile <- FALSE
    }
  }
  # cleanup
  # remove index symbol, if it was added to result for whatever reason
  result <- result[!result %in% indexSymbol]
  result <- as.character(unique(result))
  return(result)
}

### Some manual test, should be automated with testthat

# indexSymbol <- "^GDAXI" # 30
# indexSymbol <- "^MDAXI" # 50
# indexSymbol <- "^SDAXI" # 50
# indexSymbol <- "^TECDAX" # 30
# indexSymbol <- "^FTSE" # 100
# indexSymbol <- "^BFX" # 20
# indexSymbol <- "^FCHI" # 40
# indexSymbol <- "FTSEMIB.MI" # 41?
# indexSymbol <- "^OMXSPI" # ???
# indexSymbol <- "^SSMI" # 20
# indexSymbol <- "^NDX" # 106?
# indexSymbol <- "^GSPC" # ??
# indexSymbol <- "^IXIC" # 2592
# indexSymbol <- "^DJI" # 30
# indexSymbol <- "^DJA" # 65
#
# symbols <- getComponentSymbolsForIndexSymbol(indexSymbol)
# symbols
# length(symbols)
