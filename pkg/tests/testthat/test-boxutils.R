context(desc="Test quote2box function")

test_that(desc="Test, if quote2box() throws errors/warnings on wrong arguments",
{
  expect_error(object=quote2box())
  expect_error(object=quote2box("aaa"))
  expect_error(quote2box())
  expect_error(quote2box(quote=1,boxsize=c(1,2)))
})


test_that(desc="Test, if quote2box() returns results of same length as input",
{
  # check length of one
  expect_equivalent(object=length(quote2box(rpois(1,100))),expected=1)
  # check random length > 1
  length <- 1+rpois(1,10)
  input <- rpois(length,30)
  expect_equivalent(object=length(quote2box(input)),expected=length)
})

test_that(desc="Test, if box2lower <= quote2box <= box2upper for all values",
{
  tst <- union(runif(n=50,min=0.1,max=1000),rpois(n=50,lambda=100))
  
  boxsize=1
  log=F
  boxnumber <- quote2box(quote=tst,boxsize=boxsize,log=log)
  lower <- .box2lower(boxnumber=boxnumber,boxsize=boxsize,log=log)
  upper <- .box2upper(boxnumber=boxnumber,boxsize=boxsize,log=log)
  expect_true(object=all(lower<=tst))
  expect_true(object=all(tst<=upper))
  
  boxsize=getLogBoxsize(1)
  log=T
  boxnumber <- quote2box(quote=tst,boxsize=boxsize,log=log)
  lower <- .box2lower(boxnumber=boxnumber,boxsize=boxsize,log=log)
  upper <- .box2upper(boxnumber=boxnumber,boxsize=boxsize,log=log)
  expect_true(object=all(lower<=tst))
  expect_true(object=all(tst<=upper))
})


context(desc="Test nextBox function")

test_that(desc="Test, if nextBox() throws errors/warnings on wrong arguments",
{
  expect_error(object=.nextBox())
})


test_that(desc="Test, if nextBox() returns results of same length as input",
{
  # check length of one
  expect_equivalent(object=length(.nextBox(quote=rpois(1,100),status="X")),expected=1)
  expect_equivalent(object=length(.nextBox(quote=rpois(1,100),status="O")),expected=1)
  # check random length > 1
  length <- 1+rpois(1,10)
  input <- rpois(length,30)
  expect_equivalent(object=length(.nextBox(quote=input,status="X")),expected=length)
  expect_equivalent(object=length(.nextBox(quote=input,status="O")),expected=length)
})

test_that(desc="Test, if nextBox produces appropriate values",
{
  tst <- union(runif(n=500,min=0.1,max=1000),rpois(n=500,lambda=100))

  boxsize=1
  log=F
  boxnumber <- quote2box(quote=tst,boxsize=boxsize,log=log)
  lower <- .box2lower(boxnumber=boxnumber,boxsize=boxsize,log=log)
  upper <- .box2upper(boxnumber=boxnumber,boxsize=boxsize,log=log)
  
  expect_equal(object=(upper-lower),expected=rep(x=1,times=length(boxnumber)))
  
  nextbox.tst <- .nextBox(quote=tst,status="X",boxsize=boxsize,log=log,offset=0)
  nextbox.lower <- .nextBox(quote=lower,status="X",boxsize=boxsize,log=log,offset=0)
  expect_equal(object=nextbox.lower,expected=nextbox.tst)
  
  nextbox.tst <- .nextBox(quote=tst,status="O",boxsize=boxsize,log=log,offset=0)
  nextbox.upper <- .nextBox(quote=upper,status="O",boxsize=boxsize,log=log,offset=0)
  expect_equal(object=(nextbox.upper-1),expected=nextbox.tst)

  
  boxsize=getLogBoxsize(1)
  log=T
  boxnumber <- quote2box(quote=tst,boxsize=boxsize,log=log)
  lower <- .box2lower(boxnumber=boxnumber,boxsize=boxsize,log=log)
  upper <- .box2upper(boxnumber=boxnumber,boxsize=boxsize,log=log)
  
  expect_equal(object=.box2lower(boxnumber=boxnumber+1,boxsize=boxsize,log=log),expected=upper)
  expect_equal(object=.box2upper(boxnumber=boxnumber-1,boxsize=boxsize,log=log),expected=lower)
  
  # FIXME finish this test
  #
  #
})

test_that(desc="Test, if nextBox produces appropriate values",
{
  expect_equal(object=.nextBox(quote=46.0,status="X",boxsize=1L,log=F),expected=47)
  expect_equal(object=.nextBox(quote=46.0000001,status="X",boxsize=1L,log=F),expected=47)
  expect_equal(object=.nextBox(quote=46.9999999,status="X",boxsize=1L,log=F),expected=47)
  expect_equal(object=.nextBox(quote=47.0,status="X",boxsize=1L,log=F),expected=48)

  expect_equal(object=.nextBox(quote=46.0,status="O",boxsize=1L,log=F),expected=46)
  expect_equal(object=.nextBox(quote=46.0000001,status="O",boxsize=1L,log=F),expected=46)
  expect_equal(object=.nextBox(quote=46.9999999,status="O",boxsize=1L,log=F),expected=46)
  expect_equal(object=.nextBox(quote=47.0,status="O",boxsize=1L,log=F),expected=47)
  
})
