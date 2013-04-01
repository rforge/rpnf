test.quote2box <- function() {
  tst <- union(runif(n=50,min=0.1,max=1000),rpois(n=50,lambda=100))
  boxsize <- log(100)-log(99)
  
  for (i in 1:length(tst)) {
    checkTrue(rpnf:::.box2lower(rpnf:::.quote2box(tst[i])) <= tst[i])  
    checkTrue(tst[i] < rpnf:::.box2upper(rpnf:::.quote2box(tst[i])))

    checkTrue(rpnf:::.box2lower(rpnf:::.quote2box(tst[i],boxsize=boxsize,log=T),boxsize=boxsize,log=T) <= tst[i])
    checkTrue(tst[i] < rpnf:::.box2upper(rpnf:::.quote2box(tst[i],boxsize=boxsize,log=T),boxsize=boxsize,log=T))
  }

  rm(tst,boxsize)  
}

