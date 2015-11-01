#' Downloads symbols for given Yahoo index symbol by scraping the composite page. 
#'
#' @param indexSymbols Character vector of yahoo index symbols (only 1 supported currently)
#' @param stockMarketIdentifier Appendix to identify the stock market, e.g. '.DE' for german XETERA
#'
#' @return Vector of yahoo symbols for usage in quantmod.
#' @export
#' @import httr
#' @import stringr
getSymbolsForIndexSymbols <- function(indexSymbols=c("GDAXI"),stockMarketIdentifier=".DE") {
  if (!is.character(indexSymbols)) {
    stop("Parameter indexSymbols has to be character!")
  }

  symbols <- character()
  for (indexSymbol in indexSymbols) {
    # get index composition page from Yahoo
    retrieved <- httr::GET(paste0("https://de.finance.yahoo.com/q/cp?s=^",indexSymbol)) 
    # check status code
    if (httr::status_code(retrieved)!=200) {
      stop(paste0("Error while retrieving page from Yahoo! Status code = ",httr::status_code(retrieved)))  
    }
    # extract content
    content <- httr::content(retrieved,"text")
    # extract symbols from content
    pattern1 <- paste0("<b><a href=\"/q\\?s=[A-Z0-9]+\\",stockMarketIdentifier,"\">([A-Z0-9]+\\.DE)</a></b>")
    extract1 <- stringr::str_extract_all(string=content,pattern=pattern1)
    pattern2 <- paste0(">([A-Z0-9]+\\",stockMarketIdentifier,")</a></b>")
    extract2 <- stringr::str_extract_all(string=extract1,pattern=pattern2)
    pattern3 <- paste0("([A-Z0-9]+\\",stockMarketIdentifier,")")
    newsymbols <- stringr::str_extract_all(string=extract2,pattern=pattern3)
    symbols <- c(symbols,unlist(newsymbols))    
  }
  return(symbols)
}