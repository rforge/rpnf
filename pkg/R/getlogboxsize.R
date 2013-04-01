# returns an appropriate boxsize for given percent value
getLogBoxsize <- function(percent) {
  if (class(percent)!="numeric")
    stop("percent value has to be numeric!")
  return (log(100+percent)-log(100))
}