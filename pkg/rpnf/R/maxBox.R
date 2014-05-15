#' Returns the maximum box number in given column
maxBox <- function(redData,column) {
  # TODO we have to make a difference between X and O columns!
  # max will be inaccurate for O columns
  max(redData$boxnumber[redData$column==column])
}
