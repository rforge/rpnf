# determine next reversal boundary for given value and status
.nextReversal <- function(quote,status, reversal=3, boxsize=1, log=FALSE) {
  if (status == "X")
    return (.nextBox(quote, "O", boxsize=boxsize,log=log, offset=-reversal+1))
  else
    return (.nextBox(quote, "X", boxsize=boxsize,log=log, offset=reversal-1))
}
