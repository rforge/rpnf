test.nextBox.lin <- function() {
  checkEqualsNumeric(rpnf:::.nextBox(46.0,"X",boxsize=1),47)
  checkEqualsNumeric(rpnf:::.nextBox(46.00001,"X",boxsize=1),47)
  checkEqualsNumeric(rpnf:::.nextBox(46.99,"X",boxsize=1),47)
  checkEqualsNumeric(rpnf:::.nextBox(47.0,"X",boxsize=1),48)
  checkEqualsNumeric(rpnf:::.nextBox(46.0,"O",boxsize=1),46)
  checkEqualsNumeric(rpnf:::.nextBox(46.00001,"O",boxsize=1),46)
  checkEqualsNumeric(rpnf:::.nextBox(46.99,"O",boxsize=1),46)
  checkEqualsNumeric(rpnf:::.nextBox(47.00,"O",boxsize=1),47)  
  # check another boxsize
  checkEqualsNumeric(rpnf:::.nextBox(45.99999,"X",boxsize=0.5),46.0)
  checkEqualsNumeric(rpnf:::.nextBox(46.0,"X",boxsize=0.5),46.5)
  checkEqualsNumeric(rpnf:::.nextBox(46.00001,"X",boxsize=0.5),46.5)
  checkEqualsNumeric(rpnf:::.nextBox(46.49999,"X",boxsize=0.5),46.5)
  checkEqualsNumeric(rpnf:::.nextBox(46.0,"O",boxsize=0.5),46.0)
  checkEqualsNumeric(rpnf:::.nextBox(46.49999,"O",boxsize=0.5),46.0)
  checkEqualsNumeric(rpnf:::.nextBox(46.5,"O",boxsize=0.5),46.5)
}

test.nextBox.log <- function() {
  checkEqualsNumeric(rpnf:::.nextBox(101.1852,"X",boxsize=(log(101)-log(100)),log=TRUE),101.1853,tolerance=1e-06)
  checkEqualsNumeric(rpnf:::.nextBox(101.1853,"X",boxsize=(log(101)-log(100)),log=TRUE),101.1853,tolerance=1e-06)
  checkEqualsNumeric(rpnf:::.nextBox(101.1854,"X",boxsize=(log(101)-log(100)),log=TRUE),102.1972,tolerance=1e-06)
  checkEqualsNumeric(rpnf:::.nextBox(101.1852,"O",boxsize=(log(101)-log(100)),log=TRUE),100.1835,tolerance=1e-06)
  checkEqualsNumeric(rpnf:::.nextBox(101.1853,"O",boxsize=(log(101)-log(100)),log=TRUE),100.1835,tolerance=1e-06)
  checkEqualsNumeric(rpnf:::.nextBox(101.1854,"O",boxsize=(log(101)-log(100)),log=TRUE),101.1853,tolerance=1e-06)
}
