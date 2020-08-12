
# OPIM 5603 RStudio OPTIONAL Session 8+
# Wednesday, October 23, 2019


# Similar to analysis done for the confidence intervals, we can
# illustrate the meaning of p-value using random number generator in R:
#
# Recall (lecture slide 2.33): assume alpha = 0.05
#   If we have 1000 samples from the normal distribution with
#   population mean exactly equal mu0,
#   for approx. 950 of those samples null hypothesis will be accepted.
#   However for ~50 it will be rejected, despite the fact that
#   the null hypothesis is correct!
# The same should hold for one-sided t-tests

# Make a variation of classical t-tests functions:
#   function returns:  TRUE if H0 is accepted
#                     FALSE if H0 is rejected
#   for each of the three tests: lower-tailed, two-tailed, upper-tailed.

  ttests = function(v,mu0,alpha=0.05)
  {
    n = length(v)
    tstat = (mean(v)-mu0)/sqrt(var(v)/n)
    aux = pt(tstat,df=n-1)
    return(c(aux,2*pt(-abs(tstat),df=n-1),1-aux)>alpha)
  }

# try it:
  ttests(rnorm(100),mu0=0)
  ttests(rnorm(100),mu0=0.9)
  ttests(runif(100,3,15),mu0=9)
  ttests(rexp(100,rate=2),mu0=0.7)

# Idea: we want to replicate ttests function m times and count
#       how many times H0 is accepted by summing each row of
#       the matrix that replicate produces

# To allow use of different random number genearors we use do.call()

  do.call(rnorm,list(100,0,1))    # same as rnorm(100,0,1)
  do.call(rnorm,list(100))        # same as rnorm(100)
  
  ttests(do.call(rnorm,list(100)),mu0=0)

# try to replicate
  aux = replicate(12,ttests(do.call(rnorm,list(100)),mu0=0)); aux

# finally we need the row sums
  rowSums(aux)

  ttests_cnt = function(m,fn,arglist,mu0)
  {
    return(rowSums(replicate(m,ttests(do.call(fn,arglist),mu0))))
  }
  
# Try it
  ttests_cnt(12,rnorm,list(100),0)
  ttests_cnt(1000,rnorm,list(100),mu0=0)
  ttests_cnt(1000,runif,list(100),mu0=0.5)

# What do we get when we replicate this function?
  replicate(12,ttests_cnt(1000,rnorm,list(100),mu0=0))

# Repeat this 100 times and get the means of the rows of the matrix
# that replicate produces
  rowMeans(replicate(100,ttests_cnt(1000,rnorm,list(100),mu0=0)))

# Try repeating 1000 times (this will take a long time)
  rowMeans(replicate(1000,ttests_cnt(1000,rnorm,list(100),mu0=0)))

# Try it with 100 repetitions for mu0's different than population mean
  rowMeans(replicate(100,ttests_cnt(1000,rnorm,list(100),mu0=0.1)))
  rowMeans(replicate(100,ttests_cnt(1000,rnorm,list(100),mu0=0.2)))
  rowMeans(replicate(100,ttests_cnt(1000,rnorm,list(100),mu0=0.7)))
