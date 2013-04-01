# determine next box boundary for given value and status
# offset should only be used for reversal calculation
.nextBox <- function(quote,status, boxsize=1, log=FALSE, offset=0) {
  if (status == "X")
    .box2upper(.quote2box(quote=quote,boxsize=boxsize,log=log)+offset,boxsize=boxsize,log=log)
  else
    .box2lower(.quote2box(quote=quote,boxsize=boxsize,log=log)+offset,boxsize=boxsize,log=log)
}
