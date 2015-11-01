getSymbolsForIndexSymbols <- function(indexSymbols="GDAXI",stockMarketIdentifier=".DE") {
  if (!is.character(indexSymbols)) {
    stop("Parameter indexSymbols has to be character!")
  }
  if (length(indexSymbols)>1) {
    stop("Parameter indexSymbols with more than one entry not yet supported!")
  }
  indexSymbol <- indexSymbols[1]
  # get index composition page from Yahoo
  retrieved <- httr::GET(paste0("https://de.finance.yahoo.com/q/cp?s=^",indexSymbol)) 
  # check status code
  if (httr::status_code(retrieved)!=200) {
    stop(paste0("Error while retrieving page from Yahoo! Status code = ",httr::status_code(retrieved)))  
  }
  # extract content
  content <- httr::content(retrieved,"text")
  # extract symbols from content
  #pattern <- "<b><a href=\"/q\\?s=ADS.DE\">(ADS\.DE)</a></b>"
  pattern1 <- paste0("<b><a href=\"/q\\?s=[A-Z0-9]+\\",stockMarketIdentifier,"\">([A-Z0-9]+\\.DE)</a></b>")
  extract1 <- stringr::str_extract_all(string=content,pattern=pattern1)
  pattern2 <- paste0(">([A-Z0-9]+\\",stockMarketIdentifier,")</a></b>")
  extract2 <- stringr::str_extract_all(string=extract1,pattern=pattern2)
  pattern3 <- paste0("([A-Z0-9]+\\",stockMarketIdentifier,")")
  symbols <- stringr::str_extract_all(string=extract2,pattern=pattern3)
  unlist(symbols)
}