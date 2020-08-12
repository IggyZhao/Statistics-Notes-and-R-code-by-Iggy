
# OPIM 5603 RStudio Session 7
# Wednesday, October 9, 2019


# Classical Theory: Confidence Intervals for SampleMean of Normal sample ##

# CIs for SampleMean of a Normal sample: Known Population Variance
  
# Create a function that for a random sample of length n from 
# normal distribution, assuming known variance case,
# returns the left end point and the right end point of the conf.interval
# Name it CIkep: C(onfidence) I(nterval) k(nown variance case) e(nd) p(oints)

  CIkep = function(v,popsd,conf=0.95) # Known pop.variance: popsd is its root #popsd = sigma -iggy
  {
    n = length(v)
    SM = mean(v)                      # SampleMean
    zC = qnorm(conf+(1-conf)/2)       # zC value for the given confidence
    hw = zC*popsd/sqrt(n)             # half-width of the conf.interval
    endpts = c(SM-hw,SM+hw)           # conf.interval end points
    return(endpts)
  }
  
# Try it on example on slide 2.27:
# Sample of 12 numbers from normal distribution with pop variance 4
  
  v = c(7.85, 6.7, 9.83, 4, 8.61, 6.05, 5.39, 7.82, 5.63, 7.07, 11.2, 6.8)
  CIkep(v,popsd=2,conf=0.9)


# Lecture slide 2.25: How to interpret "with probability 0.95?" 
#    Imagine you sample X1, X2, ., Xn from N(mu,sigma^2) independently 1000 times.
#    You get a thousand open intervals of same length centered at different points
#    Population mean mu would be inside approximately 950 of those intervals, and
#    outside of approximately 50 of them.

# We can verify this by generating random samples from normal distribution.

# Set some (arbitrary) population parameters
  popmean = 5; popsd = 7.87 # need to know here but not need to known in example before-iggy

# A random sample of size 30 from N(5,7.87^2):
  v = rnorm(30,popmean,popsd)

# check if population mean 5 is inside the confidence interval
  CIkep(v,popsd)
  CIkep(v,0.95) # 0.95 defualt-iggy

# Notice that for the purpose of this exercise we only care whether
# the population mean is inside the confidence interval.
# Modify the function CIkep to return TRUE if population mean is inside
# the conf.interval and FALSE otherwise.
# Call it CIk: C(onfidence) I(nterval) k(nown variance case)

  CIk = function(v,popmean,popsd,conf=0.95)
  {
    n = length(v)
    SM = mean(v)                           # SampleMean
    zC = qnorm(conf+(1-conf)/2)            # zC value for the given confidence
    hw = zC*popsd/sqrt(n)                  # half-width of the conf.interval
    return(SM-hw<popmean & popmean<SM+hw)  # TRUE if CI contains popmean, FALSE otherwise
  } # this return means the popmean (5 in this case) is inside the CI. -iggy
  
# try it on a random vector from N(popmean,popsd^2)
  CIk(rnorm(100,popmean,popsd),popmean,popsd)
# rnorm(100,popmean,popsd) is the vector v -iggy
  
# Now we can replicate this line and count how many TRUEs we get, i.e.,
# we count how many confidence intervals contain the population mean popmean.
  
# Step-by-step:
  replicate(7,CIk(rnorm(100,popmean,popsd),popmean,popsd)) # really pay attention is how many ture -iggy
  sum(replicate(7,CIk(rnorm(100,popmean,popsd),popmean,popsd))) # return 7 means all are true -iggy
  
# Function:
  CountCIk = function(m,n,popmean,popsd,conf=0.95) # count how many are true - iggy
  {
    return(sum(replicate(m,CIk(rnorm(n,popmean,popsd),popmean,popsd))))
  }
  
# try it couple of times
  CountCIk(1000,30,popmean,popsd)
  
# Thus "the number of conf.intervals that contain population mean" is a random
# variable! Clearly it is integer valued (not a continuous random variable).
# What is its distribution?
# To get an idea what it might be, replicate "CountCIk" M times and plot histogram
  
# Note: with M = 5000, m = 1000, and n = 30 we have 150 million calls to rnorm
#       This may take a long time: reduce m to 100 and notice that the number
#       of confidence intervals that contain population mean should be approx. 95

  popmean = 5; popsd = 7.87
  M = 5000; m = 100; n = 30; C = 0.95

  v = replicate(M,CountCIk(m,n,popmean,popsd,C))
  hist(v,breaks=seq(-0.5,100.5,1),freq=F,col="yellow2"); grid()#around 95-iggy
  myhist(v,seq(-0.5,100.5),main="Count of CIk's containing popmean")
  
# zoom into interval [80,100]:
  myhist(v,seq(79.5,100.5),main="Count of CIk's containing popmean")
# two outcomes, contian or not contain, so this distribution is similar to binomial-iggy
  # contain with p 0.95 and not contain 0.05 - iggy

# Look at the histogram: What could be the distribution of "the number of
#   conf.intervals that contain population mean?"
#   It is a discrete random variable, with possible values 0, 1, 2, ..., m
# Think:
#   For each sample the population mean should "fall inside" conf.interval
#   with probability 0.95 and "fall outside" with probability 0.05.
#   We count "fall inside's", so the outcomes are really 1 and 0.   
#   This is a Bernoulli variable with probability of success p = 0.95.
#   Summing this m = 100 times independently,  we get a Binomial B(100,0.95)
#   Therefore it makes sense to plot B(100,0.95) pdf over the histogram
#   to see how they compare.
  
  x = seq(80,100,1)
  lines(x,y=dbinom(x,m,prob=C),typ="p",pch=95,cex=3,col="blue")

#------------------------------------------------------------------------------------------------------
# CIs for SampleMean of a normal sample: Unknown Population Variance

# CIkep equivalent for Unknown Variance case:
# CIunkep: C(onfidence) I(nterval) unk(unknown variance case) e(nd) p(oints)
  
  CIunkep = function(v,conf=0.95)  # notice: no "popsd" parameter here
  {
    n = length(v)
    SM = mean(v)                   # SampleMean
    SV = var(v)                    # var function in R is sample varaince -iggy
    tC = qt(conf+(1-conf)/2,n-1)   # tC value for the given confidence
    hw = tC*sqrt(SV/n)         # half-width of the confidence interval
    return(c(SM-hw,SM+hw))         # endpoints of conf interval
  }

# Try it on example on slide 2.30:
# Sample of 12 numbers from normal distribution with unknown pop variance
  
  v = c(7.85, 6.7, 9.83, 4, 8.61, 6.05, 5.39, 7.82, 5.63, 7.07, 11.2, 6.8)
  CIunkep(v,conf=0.9)
  
# As in the known variance case, we'll verify the claims on lecture slide 2.29:
# How to interpret "with probability 0.95?" 

# Set
  popmean = 5; popsd = 7.87
  v = rnorm(30,popmean,popsd)
  
# check if population mean 5 is inside the confidence interval
  CIunkep(v)

# Create a function that for a random sample of length n from normal distribution,
# assuming unknown variance case, returns TRUE if the population mean is inside
# the  Conf. Interval, and FALSE otherwise
# CIunk: C(onfidence) I(nterval) unk(unknown variance case)

  CIunk = function(v,popmean,conf=0.95)
  {
    CI = CIunkep(v,conf)              # half-width of the confidence interval
    return(CI[1]<popmean & popmean<CI[2])  # TRUE if the CI contains popmean, FALSE otherwise
  }
  
# try it
  CIunk(v,popmean)

# execute several times
  CIunk(rnorm(30,popmean,popsd),popmean)

# Finally
  CountCIunk = function(m,n,popmean,popsd,conf=0.95)
  {
    return(sum(replicate(m,CIunk(rnorm(n,popmean,popsd),popmean,conf))))
  } # how many CIs in this case contain the popmean -iggy
  # rnorm(n,popmean,popsd) is the vector
  
# try it couple of times
  CountCIunk(100,30,popmean,popsd) # around 95 -iggy
  CountCIunk(100,30,popmean,popsd,conf=0.8) # around 80 -iggy
# As in the known variance case:
  popmean = 5; popsd = 7.87
  M = 5000; m = 100; n = 30; C = 0.95
  
  myhist(replicate(M,CountCIunk(m,n,popmean,popsd,C)),seq(79.5,100.5), # can set C as 0.95, 0.8...-iggy
         main="Count of CIunk's containing popmean")
  
# Plot B(100,0.95) probability distribution (pdf) function over the histogram
  x = seq(80,m,1)
  lines(x,y=dbinom(x,m,prob=C),typ="p",pch=95,cex=3,col="blue")

# try 0.8-iggy
  myhist(replicate(M,CountCIunk(m,n,popmean,popsd,0.8)),seq(59.5,100.5), # can set C as 0.95, 0.8...-iggy
         main="Count of CIunk's containing popmean")
  x = seq(60,m,1)
  lines(x,y=dbinom(x,m,prob=0.8),typ="p",pch=95,cex=3,col="blue")
  
# -------------------------------------------------------------------------- #
   
# Simulation in R: Confidence Intervals from empirical Sampling Distribution
  
# Example:
# Find the CI for the minimum of a normal N(0,1) sample of size 10.

# Idea: min(rnorm(10)) gives a representative of a minimum
  
  n=10
  popmean=0; popsd=1
  min(rnorm(n,popmean,popsd))
  
# Now replicate it many times and get a sampling distribution vector
  m = 1000000
  sdv = replicate(m,min(rnorm(n,popmean,popsd)))
  summary(sdv)  
  
  myhist(sdv,breaks=100,main="Minimum of sample of 10 N(0,1)'s")
  
  myhd(sdv,breaks=100,main="Minimum of sample of 10 N(0,1)'s")


# Let's focus on plotting empirical density:  
  d = density(sdv)
  plot(d,lwd=2,col="green4")
  polygon(d,col="palegreen")

# A bit nicer, and similar to our existing custom functions
  myed = function(v,ylim=NULL,xlab="",main="Empirical Density",
                  pcol="palegreen",dlwd=2,dcol="green4",axes=T)
  {
    main=paste(main,": ",format(length(v),big.mark=",",digits=0)," data points",sep="")
    d = density(v)
    plot(d,ylim=ylim,xlab=xlab,main=main,
         col.main=dcol,font.main=1,cex.main=1.25,axes=F)
    polygon(d,col=pcol)
    lines(d,lwd=dlwd,col=dcol)
    if (axes)
      myaxes(0,axpos(d$x))
    invisible(d)     # returns density as object (as return(d), but no console printout)
  }
  
  myed(sdv,main="Sampling Distr. of Minimum of 10 N(0,1) sample")

# What is 95% empirical Confidence Interval?
  
# Answer: sampling distribution vector 'sdv' represents the population distribution
#         Find its appropriate percentiles: 
  quantile(sdv,0.025)
  quantile(sdv,0.975)
  
  myeCI = function(v,conf=0.95)
  {
    lep = quantile(v,(1-conf)/2)       # eCI left endpoint
    rep = quantile(v,conf+(1-conf)/2)  # eCI right endpoint
    return(c(lep,rep))
  }
  
  myeCI(sdv)
  abline(v=myeCI(sdv),lwd=2,lty=2,col="orange")  

  