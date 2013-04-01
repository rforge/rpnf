test.nextReversal.lin <- function() {
  checkEqualsNumeric(rpnf:::.nextReversal(45.99999,"X",boxsize=1),43.0)
  checkEqualsNumeric(rpnf:::.nextReversal(46.0,"X",boxsize=1),44.0)
  checkEqualsNumeric(rpnf:::.nextReversal(46.00001,"X",boxsize=1),44.0)
  checkEqualsNumeric(rpnf:::.nextReversal(45.99999,"O",boxsize=1),48.0)
  checkEqualsNumeric(rpnf:::.nextReversal(46.0,"O",boxsize=1),49.0)
  checkEqualsNumeric(rpnf:::.nextReversal(46.00001,"O",boxsize=1),49.0)
  # check another boxsize
  checkEqualsNumeric(rpnf:::.nextReversal(45.99999,"X",boxsize=0.5),44.5)
  checkEqualsNumeric(rpnf:::.nextReversal(46.0,"X",boxsize=0.5),45.0)
  checkEqualsNumeric(rpnf:::.nextReversal(46.00001,"X",boxsize=0.5),45.0)
  checkEqualsNumeric(rpnf:::.nextReversal(45.99999,"O",boxsize=0.5),47.0)
  checkEqualsNumeric(rpnf:::.nextReversal(46.0,"O",boxsize=0.5),47.5)
  checkEqualsNumeric(rpnf:::.nextReversal(46.00001,"O",boxsize=0.5),47.5)
}
