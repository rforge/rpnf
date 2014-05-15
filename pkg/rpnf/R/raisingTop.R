#' Returns true if given column c exceeds previous column of same type (this is always column c-2)
raisingTop <- function(redData,column) {
  # if (column-2>=1) # old version
  if (column>=3) # new, optimized version
    return (maxBox(redData,column)>maxBox(redData,column-2))
  else
    return (FALSE)
}
