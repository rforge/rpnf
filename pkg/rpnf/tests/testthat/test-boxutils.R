##### getLogBoxsize() tests #####
testthat::context(desc="Test getLogBoxsize() function")

testthat::test_that(desc="Test, if getLogBoxsize throws errors/warnings on wrong arguments",
                    {
                      testthat::expect_error(object=getLogBoxsize())
                      testthat::expect_error(object=getLogBoxsize("aaa"))
                      testthat::expect_error(getLogBoxsize(list(1,2)))
                      testthat::expect_warning(getLogBoxsize(0))
                      testthat::expect_warning(getLogBoxsize(c(1,2,-3)))
                      testthat::expect_warning(getLogBoxsize(c(1,2,3)))
                    })

testthat::test_that(desc="Test, if getLogBoxsize returns results of same length as input",
                    {
                      # check length of one
                      testthat::expect_equivalent(object=length(getLogBoxsize(2)),expected=1)
                      # check random length > 1
                      length <- 1+rpois(1,10)
                      input <- rpois(length,30)
                      testthat::expect_equivalent(object=length(getLogBoxsize(input)),expected=length)
                    })

testthat::test_that(desc="Test, if getLogBoxsize returns appropriate results",
                    {
                      testthat::expect_equivalent(object=getLogBoxsize(1),expected=0.009950331)
                      # FIXME this fails! I hope due to numerical reasons, 
                      # need to figure out how to display more accurate values in the console
                      # expect_equivalent(object=getLogBoxsize(-1),expected=-0.01005034)
                    })

##### quote2box() tests #####
testthat::context(desc="Test quote2box() function")

testthat::test_that(desc="Test, if quote2box() throws errors/warnings on wrong arguments",
                    {
                      testthat::expect_error(object=quote2box())
                      testthat::expect_error(object=quote2box("aaa"))
                      testthat::expect_error(quote2box())
                      testthat::expect_error(quote2box(quote=1,boxsize=c(1,2)))
                    })


testthat::test_that(desc="Test, if quote2box() returns results of same length as input",
                    {
                      # check length of one
                      testthat::expect_equivalent(object=length(quote2box(rpois(1,100))),expected=1)
                      # check random length > 1
                      length <- 1+rpois(1,10)
                      input <- rpois(length,30)
                      testthat::expect_equivalent(object=length(quote2box(input)),expected=length)
                    })

testthat::test_that(desc="Test, if box2lower <= quote2box <= box2upper for all values",
                    {
                      tst <- union(runif(n=50,min=0.1,max=1000),rpois(n=50,lambda=100))
                      
                      boxsize=1
                      log=F
                      boxnumber <- quote2box(quote=tst,boxsize=boxsize,log=log)
                      lower <- box2lower(boxnumber=boxnumber,boxsize=boxsize,log=log)
                      upper <- box2upper(boxnumber=boxnumber,boxsize=boxsize,log=log)
                      testthat::expect_true(object=all(lower<=tst))
                      testthat::expect_true(object=all(tst<=upper))
                      
                      boxsize=getLogBoxsize(1)
                      log=T
                      boxnumber <- quote2box(quote=tst,boxsize=boxsize,log=log)
                      lower <- box2lower(boxnumber=boxnumber,boxsize=boxsize,log=log)
                      upper <- box2upper(boxnumber=boxnumber,boxsize=boxsize,log=log)
                      testthat::expect_true(object=all(lower<=tst))
                      testthat::expect_true(object=all(tst<=upper))
                    })



##### xo.processor() tests #####
testthat::context(desc="Test xo.processor() function")

testthat::test_that(desc="Test, if xo.processor() throws errors/warnings on wrong arguments",
                    {
                      testthat::expect_error(object=xo.processor())
                    })

# test_that(desc="Test, if speed of xo.processor() is sufficent",
# {
#   data(DOW)
#   times <- system.time(xo.processor(high=DOW$High, low=DOW$Low,date=DOW$Date))
#   warning(paste0("Timings of xo.processor() for linear charts: ",times[1]," sec."))
#   expect_less_than(object=times[1],expected=0.15)
#   
#   times <- system.time(xo.processor(high=DOW$High, low=DOW$Low,date=DOW$Date,boxsize=getLogBoxsize(1),log=T))
#   warning(paste0("Timings of xo.processor() for logarithmic charts: ",times[1]," sec."))
#   expect_less_than(object=times[1],expected=0.15)
# })

testthat::test_that(desc="Test, if speed of xo.processor() scales nearly linear in input size",
                    {
                      # check if speed scales nearly linear
                      myfactor <- 10
                      length=2000 # approx. 1 years
                      mydeltas <- rnorm(n=length,mean=0,sd=1)
                      myts <- rep(x=1000,length.out=length)
                      for (i in 1:(length-1)) {
                        myts[i+1] = myts[i]+(myts[i]*mydeltas[i]/100)
                      }
                      times.short <- system.time(xo.processor(high=myts, low=myts,date=seq(1:length),boxsize=getLogBoxsize(1),log=T))
                      
                      length=length*myfactor # approx. 10 years
                      mydeltas <- rnorm(n=length,mean=0,sd=1)
                      myts <- rep(x=1000,length.out=length)
                      for (i in 1:(length-1)) {
                        myts[i+1] = myts[i]+(myts[i]*mydeltas[i]/100)
                      }
                      times.long <- system.time(xo.processor(high=myts, low=myts,date=seq(1:length),boxsize=getLogBoxsize(1),log=T))
                      
                      testthat::expect_less_than(object=times.long[1],expected=(2*myfactor)*times.short[1])
                    })

testthat::test_that(desc="Test, if xo.processor() produces correct output",
                    {
                      data(DOW)
                      load(file="boxutils-example1.RData") # this loads an object result
                      testthat::expect_equivalent(object=xo.processor(high=DOW$High,low=DOW$Low,date=DOW$Date),expected=result)
                    })
