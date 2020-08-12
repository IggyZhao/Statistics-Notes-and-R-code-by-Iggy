
# OPIM 5603 RStudio Session 7
# Wednesday, October 9, 2019


# -- Classical Theory: Confidence Intervals for SampleMean of Normal sample -- #

# CIs for SampleMean of a Normal sample: Known Population Variance
  
# Create a function that for a random sample of length n from 
# normal distribution, assuming known variance case,
# returns the left end point and the right end point of the conf.interval
# Name it CIkep: C(onfidence) I(nterval) k(nown variance case) e(nd) p(oints)

  CIkep = function(v,popsd,conf=0.95) # Known pop.variance: popsd is its root
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

# Set some (arbitrary) population parameters and sample size
  popmean = 5; popsd = 7.87
  n = 30
  
# A random sample of size 30 from N(5,7.87^2):
  v = rnorm(n,popmean,popsd)

# check if population mean 5 is inside the confidence interval
  CIkep(v,popsd)

# Notice that for the purpose of this exercise we only care whether
# the population mean is inside the confidence interval.
# Use function CIkep and return TRUE if population mean is inside
# the conf.interval and FALSE otherwise.
# Call it CIk: C(onfidence) I(nterval) k(nown variance case)

  CIk = function(v,popmean,popsd,conf=0.95)
  {
    CI = CIkep(v,popsd,conf)
    return(CI[1]<popmean & popmean<CI[2])  # TRUE if CI contains popmean, FALSE otherwise
  }

# try it on a random vector from N(popmean,popsd^2)
  CIk(rnorm(n,popmean,popsd),popmean,popsd)

# Now we can replicate this line and count how many TRUEs we get, i.e.,
# we count how many confidence intervals contain the population mean popmean.
  
# Step-by-step:
  replicate(7,CIk(rnorm(n,popmean,popsd),popmean,popsd))
  sum(replicate(7,CIk(rnorm(n,popmean,popsd),popmean,popsd)))
  
# Function:
  CountCIk = function(m,n,popmean,popsd,conf=0.95)
  {
    return(sum(replicate(m,CIk(rnorm(n,popmean,popsd),popmean,popsd,conf))))
  }
  
# try it couple of times
  CountCIk(1000,n,popmean,popsd)
  
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
  hist(v,breaks=seq(-0.5,m+0.5,1),freq=F,col="yellow2"); grid()
  
  # execute myFunctions.R code to load in the custom functions we created
  myhist(v,seq(-0.5,m+0.5),main="Count of CIk's containing popmean")
  
# zoom into interval [80,100]:
  myhist(v,seq(79.5,m+0.5),main="Count of CIk's containing popmean")


# Look at the histogram: What could be the distribution of "the number of
#   conf.intervals that contain population mean?"
#   It is a discrete random variable, with possible values 0, 1, 2, ..., m
# Think:
#   For each sample the population mean  should "fall inside" conf.interval
#   with probability 0.95 and "fall outside" with probability 0.05.
#   We count "fall inside's", so the outcomes are really 1 and 0.   
#   This is a Bernoulli variable with probability of success p = 0.95.
#   Summing this m = 100 times independently,  we get a Binomial B(100,0.95)
# Therefore it makes sense to plot B(100,0.95) pdf over the histogram
#   to see how they compare.
  
  x = seq(80,m)
  points(x,y=dbinom(x,m,prob=C),pch=95,cex=3,col="blue")

# Try it again for confidece level C = 0.8

  C = 0.8
  v = replicate(M,CountCIk(m,n,popmean,popsd,C))
  myhist(v,seq(-0.5,m+0.5),main="Count of CIk's containing popmean")
  # zoom into interval [65,95]:
  myhist(v,seq(64.5,95.5),main="Count of CIk's containing popmean")
  # and compare to the binomial B(100,0.8):
  x = seq(65,95)
  points(x,y=dbinom(x,m,prob=C),pch=95,cex=3,col="blue")

  
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
  
# CIs for SampleMean of a normal sample: Unknown Population Variance

# CIkep equivalent for Unknown Variance case:
# CIunkep: C(onfidence) I(nterval) unk(unknown variance case) e(nd) p(oints)
  
  CIunkep = function(v,conf=0.95)  # notice: no "popsd" parameter here
  {
    n = length(v)
    SM = mean(v)                   # SampleMean
    SV = var(v)                    # Sample Variance
    tC = qt(conf+(1-conf)/2,n-1)   # tC value for the given confidence
    hw = tC*sqrt(SV/n)             # half-width of the confidence interval
    return(c(SM-hw,SM+hw))         # endpoints of conf interval
  }

# Try it on example on slide 2.30:
# Sample of 12 numbers from normal distribution with unknown pop variance
  
  v = c(7.85, 6.7, 9.83, 4, 8.61, 6.05, 5.39, 7.82, 5.63, 7.07, 11.2, 6.8)
  CIunkep(v,conf=0.9)
  
# As in the known variance case, we'll verify the claims on lecture slide 2.29:
# How to interpret "with probability 0.95?" 

# Set
  popmean = 5; popsd = 7.87; n = 30
  v = rnorm(n,popmean,popsd)
  
# check if population mean 5 is inside the confidence interval
  CIunkep(v)

# Create a function that for a random sample of length n from normal distribution,
# assuming unknown variance case, returns TRUE if the population mean is inside
# the  Conf. Interval, and FALSE otherwise
# CIunk: C(onfidence) I(nterval) unk(unknown variance case)

  CIunk = function(v,popmean,conf=0.95)
  {
    CI = CIunkep(v,conf)
    return(CI[1]<popmean & popmean<CI[2])  # TRUE if CI contains popmean, FALSE ow
  }
  
# try it
  CIunk(v,popmean)

# execute several times
  CIunk(rnorm(n,popmean,popsd),popmean)

# Finally
  CountCIunk = function(m,n,popmean,popsd,conf=0.95)
  {
    return(sum(replicate(m,CIunk(rnorm(n,popmean,popsd),popmean,conf))))
  }
  
# try it couple of times
  CountCIunk(1000,n,popmean,popsd)

# As in the known variance case:
  popmean = 5; popsd = 7.87
  M = 5000; m = 100; n = 30; C = 0.95
  
  myhist(replicate(M,CountCIunk(m,n,popmean,popsd,C)),seq(79.5,m+0.5),
         main="Count of CIunk's containing popmean")
  
# Plot B(100,0.95) probability distribution (pdf) function over the histogram
  x = seq(80,m,1)
  points(x,y=dbinom(x,m,prob=C),pch=95,cex=3,col="blue")

  
# ------------------------------------------------------------------------- #
   
# Simulation in R: Confidence Intervals from empirical Sampling Distribution
  
# Example: Find the CI for the minimum of a normal N(0,1) sample of size 10.

# Idea: min(rnorm(10)) gives a representative of a minimum
  
  n=10
  popmean=0; popsd=1
  min(rnorm(n,popmean,popsd))
  
# Now replicate it many times and get a sampling distribution vector
  m = 1000000
  sdv = replicate(m,min(rnorm(n,popmean,popsd)))
  
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
    lines(d,lwd=dlwd,col=dcol)   # plot density again over the polygon
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
  
  #--------------------------------------------------------------------------------------Session 8
  
  
  # Execution of the custom functions R script:
  
  source("myFunctions.R")
  
  # Recall: plotting densities with polygon function (load myFunctions.R)
  
  rv = abs(rnorm(1000000))
  plot(density(rv,from=0,cut=1))
  myed(rv,main="Abs. value of N(0,1)",pcol="lightyellow")
  myhd(rv,main="Abs. value of N(0,1)",col="lightyellow",breaks=100)
  myhist(rv,main="Abs. value of N(0,1)",col="lightyellow",breaks=200)
  
  u1 = runif(1000000,min=1,max=10)
  u2 = runif(1000000,min=1,max=10)
  myed(u1+u2,main="Sum of two independent U(1,10)")
  
  u1 = runif(1000000,min=1,max=10)
  u2 = runif(1000000,min=1,max=4)
  myed(u1+u2,main="Sum of independent U(1,10) and U(1,4)")
  
  
  # ----- Classical (one-sample) two-tailed t-test -----
  
  # (Motivation: Quiz 7 Problem 3)
  # A botanist is trying to determine whether an experimental growth substance
  # applied to plant seeds affected the average height of plants. The standard
  # average height is 306.9 mm. The botanist treated a random sample of 420 seeds
  # with the extract and subsequently obtained the height data provided in the
  # file PlantHeight.csv. She decides to make a conclusion in the following way:
  # if the height population mean is inside 95% confidence interval generated
  # by her sample she will conclude that the substance did not affect the growth
  # of plants.  Otherwise, she will conclude the substance affected the growth.
  
  plants = read.csv("Data/PlantHeight.csv")
  
  # (a) Plot the empirical density of plants' height data.
  
  ph = plants$Height
  myed(ph,main="Plant heights empirical density",xlab="Height [mm]")
  
  # Possibly a better depiction of the data is obtained by histogram
  myhd(ph,main="Plant heights empirical density",xlab="Height [mm]",breaks=20)
  # also see where the untreated population mean of 306.9 fits:
  abline(v=306.9,lwd=2,col="green3")
  
  # (b) Plot over the graph in (a) the pdf of a Normal distribution with
  #     parameters appropriately chosen from the sample.
  
  x = seq(295,320,0.01)
  lines(x,dnorm(x,mean(ph),sd(ph)),lwd=2,col="blue")
  
  # (c) Calculate the 95% confidence interval.
  
  # Session 7 function:
  CIunkep = function(v,conf=0.95)
  {
    n = length(v) 							       # Sample size
    SM = mean(v)							         # SampleMean
    SV = var(v)                        # Sample Variance
    tC = qt(conf+(1-conf)/2,n-1)		   # tC value for the given confidence
    hw = tC*sqrt(SV/n)	    			     # half-width of the confidence interval
    return(c(SM-hw,SM+hw))				     # endpoints of conf interval
  }
  
  CI = CIunkep(ph); CI
  
  # The 95% conf.interval is (306.9435, 307.5332)
  #  => the substance affects the growth.
  
  # -----------------------------------------------------------------------------
  
  # Lecture slide example: Two-tailed t-test calculation, unknown pop.variance
  
  v = c(7.85, 6.7, 9.83, 4, 8.61, 6.05, 5.39, 7.82, 5.63, 7.07, 11.2, 6.8)
  
  sm = mean(v); sm                                 # Sample Mean
  sv = var(v); sv                                  # Sample Variance
  n = length(v); n                                 # sample size
  alpha = 0.1                                      # significance level
  mu0 = 7                                          # Null-Hypothesis H0: mu = 7
  T = (sm-mu0)/(sqrt(sv)/sqrt(n)); T               # T statistic value
  pval = pt(-abs(T),n-1) + 1-pt(abs(T),n-1); pval  # p-value
  accept = pval > alpha; accept                    # if TRUE, accept H0
  #   Otherwise reject.
  
  # wrap this into a two-tailed t test (ttttest) function:
  ttttest = function(v,mu0,alpha)
  {
    sm = mean(v)                                # Sample Mean
    sv = var(v)                                 # Sample Variance
    n = length(v)                               # sample size
    T = (sm-mu0)/(sqrt(sv)/sqrt(n))             # T statistic value
    pval = pt(-abs(T),n-1) + 1-pt(abs(T),n-1)   # p-value from t(n-1)
    if (pval>alpha)
    {return(cat(" p-value equals",round(pval,5),">",alpha,"\n",
                "accept Null-Hypothesis: population mean equals",mu0))}
    else
    {return(cat(" p-value equals",round(pval,5),"<=",alpha,"\n",
                "reject Null-Hypothesis in favor of the Alternative:",
                "population mean is not equal",mu0))}
  }
  
  ttttest(v,7,0.1)
  
  # Recall the example on lecture slide 2.30 (also in Session 7):
  # 90% conf.interval (unknown variance case) for sample given by vector v is
  #
  #             (6.21587, 8.275797).
  #
  # Since mu0 = 7 is inside this interval
  # the two-tailed t-test accepts the null-hypothesis
  # See what happens to the following mu0 values:  
  
  ttttest(v,8,0.1)
  ttttest(v,6,0.1)
  ttttest(v,6.215,0.1)
  ttttest(v,6.216,0.1)
  
  # -----------------------------------------------------------------------------
  
  # Apply the t-test function to the botanist data:
  
  ttttest(ph,306.9,0.05)
  
  # the 95% conf.interval was (306.9435, 307.5332)
  
  ttttest(ph,306.943,0.05)
  ttttest(ph,306.944,0.05)
  
  # -----------------------------------------------------------------------------
  
  # Another example: student admissions data frame
  adm <- read.csv("Data/Admission.csv")
  
  # review the data frame
  adm            # "prints" the frame in a console
  View(adm)      # the entire frame in a separate window
  head(adm)      # initial rows only
  summary(adm)   # basic stats
  
  # plot the empirical density of the GMAT data
  myed(adm$GMAT,main="GMAT data",xlab="GMAT score",pcol="khaki",dcol="brown")
  myhd(adm$GMAT,main="GMAT data",xlab="GMAT score",col="khaki",dcol="brown",breaks=10)
  
  # # Without custom functions:
  # dGMAT = density(adm$GMAT)
  # plot(dGMAT,lwd=3,col="red",main="GMAT data",xlab="GMAT score")
  # polygon(dGMAT,col="lightgreen")
  
  # Apply ttttest() function on the GMAT data:
  # we test if population mean equals, say, 510
  # H0: The GMAT score population mean equals 510
  # Ha: The GMAT score population mean is NOT equal 510
  
  ttttest(adm$GMAT,510,0.5)
  # We reject H0 at significance level 0.05 in favor of Ha
  
  mean(adm$GMAT)  
  ttttest(adm$GMAT,500,0.5)
  
  # Compare to built-in t-test
  # Note: it uses confidence level instead of the significance
  t.test(adm$GMAT,mu=510,conf.level=0.95)   # 0.95 default conf.level
  t.test(adm$GMAT,mu=510)
  
  # Take a look at the confidence interval
  t.test(adm$GMAT,mu=471)     # 471 is inside the interval
  t.test(adm$GMAT,mu=470)     # 470 is outside the interval
  
  
  # -----------------------------------------------------------------------------
  
  # Classical one-tailed t-test: Salinity example
  
  # H0: The salinity of the lake water is less than or equal to 0.5
  #     (grams of salt in 1 kg of water)
  # Ha: The salinity of the lake water is strictly greater than 0.5
  
  u = read.csv("Data/Salinity.csv")
  View(u)
  mean(u$Salinity)
  
  # Plot the empirical density of the collected salinity data
  myed(u$Salinity,xlab = "Salinity [ g/l ]",main="Salinity measurements",
       pcol="lightblue",dcol="blue3")
  myhd(u$Salinity,xlab = "Salinity [ g/l ]",main="Salinity measurements",
       col="lightblue",border="blue",dcol="blue3",breaks=8)
  abline(v=0.5,col="red")
  
  t.test(u$Salinity,mu=0.5,alternative="greater")   # Upper-tailed t-test
  t.test(u$Salinity, mu=0.5)                        # Two-tailed t-test
  t.test(u$Salinity, mu=0.5, alternative="less")    # Lower-tailed t-test
  
  # Compare the value of the T-statistic with t(57) quantiles
  qt(c(0.025,0.05,0.95,0.975),57)
  
  # -----------------------------------------------------------------------------
  
  # Now apply the built-in t.test function to the botanist data:
  #
  # H0: The mean height of treated population is less than or equal to 306.9
  # Ha: The mean height of treated population is strictly greater than 306.9
  
  # Plot again the empirical density (dh above) of the collected height data
  
  myed(ph,main="Plant heights empirical density",xlab="Height [mm]")
  abline(v=306.9,col="blue")
  
  t.test(ph,mu=306.9,alternative="greater")   # Upper-tailed t-test
  t.test(ph,mu=306.9)                         # Two-tailed t-test
  t.test(ph,mu=306.9, alternative="less")     # Lower-tailed t-test
  
  # Compare the value of the T-statistic with t(419) quantiles
  qt(c(0.025,0.05,0.95,0.975),419)
  
# -------------------------------------------------------------------------Session 8+
  
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
  
  
  
#--------------------------------------------------------------------------Session 9 
  
  # a convenient way to store custom functions to memory
  
  source("myFunctions.R")
  
  # ----------------------------------------------------------------------
  
  # Two-tailed z-test: adjust the ttttest() function from Session 8
  
  ttztest = function(v,mu0,sigm,alpha)
  {
    sm = mean(v)                               # Sample Mean
    n = length(v)                              # sample size
    Z = (sm-mu0)/(sigm/sqrt(n))                # Z statistic value
    pval = pnorm(-abs(Z)) + 1-pnorm(abs(Z))    # p-value from N(0,1)
    if (pval>alpha)
    {return(cat(" p-value equals",round(pval,5),">",alpha,"\n",
                "accept Null-Hypothesis: population mean equals",mu0))}
    else
    {return(cat(" p-value equals",round(pval,5),"<=",alpha,"\n",
                "reject Null-Hypothesis in favor of the Alternative:",
                "population mean is not equal",mu0))}
  }
  
  # Problem 3 in Quiz 8:
  #   NormalData.csv contains random numbers generated from the
  #   normal distribution with variance 100.
  #   Test the hypothesis that the population mean equals 28.41
  #   assuming 5% significance level.
  #   Note: Population variance is known.
  
  nd = read.csv("Data/NormalData.csv")
  v = nd$Data
  
  ttztest(v,mu0=28.41,sigm=10,alpha=0.05)
  
  # compare  it to t-test(s):
  ttttest(v,mu0=28.41,alpha=0.05)     # you must load this from Session 8
  t.test(v,mu=28.41,conf.level = 0.95)  
  
  # ----------------------------------------------------------------------
  
  # Classical Two-Sample t-test
  
  # Example: Drinking water pollutants
  # Trace metals in drinking water affect the flavor and an unusually high
  # concentration can pose a health hazard. 40 pairs of data were taken
  # measuring pollutant concentration in bottom water and surface water.
  
  p = read.csv("Data/Pollutant.csv"); View(p)
  
  # Question: Is this data paired?
  
  # View the basic properties of data columns
  summary(p)
  
  # Based on summary values plot the scatter in [0,1]x[0,1] square 
  plot(p$bottom,p$surface,xlim=c(0,1),ylim=c(0,1),
       pch=19,col="blue2",axes=F); myaxes(0,0)
  abline(a=0,b=1,lwd=1.5,col="orange")
  
  # Check the correlation
  cor(p$bottom,p$surface)
  
  # Get the 'difference sample'
  delta = p$bottom-p$surface
  length(delta)
  summary(delta)
  
  # empirical density of delta:
  myhd(delta,breaks=7,ylim=c(0,6),xlab="ppm",main="Bottom - Surface (difference)")
  myed(delta,ylim=c(0,6),xlab="ppm",main="Bottom - Surface (difference)")
  
  # without custom functions:
  ddelta = density(delta)
  plot(ddelta, lwd=2, col="red",
       xlab="ppm", main="Bottom - Surface (difference)")
  grid()
  polygon(ddelta,col="lightgreen")
  abline(v=0,col="blue")
  
  # Paired sample t-test is really a classical one-sample t-test with
  #   H0: The population mean (of the difference) equals 0
  #   Ha: The population mean (of the difference) is NOT equal 0
  mu0 = 0
  
  # Compute the T statistic for the difference sample and the p-value
  n = length(delta)
  tstat = (mean(delta)-mu0)/(sd(delta)/sqrt(n)); tstat
  pval = pt(-abs(tstat),df=n-1) + 1 - pt(abs(tstat),df=n-1); pval
  
  # compare to built-in t-test:
  
  # One-sample t test for differences:
  t.test(delta, mu=0, conf.level=0.95)
  
  # Two-sample paired t test:
  t.test(p$bottom, p$surface, paired=TRUE, conf.level=0.95)
  
  
  # ----------------------------------------------------------------------
  
  # Example: Student GPA
  #
  # Random samples of GPA's of juniors and seniors at some university.
  
  gpa = read.csv("Data/GPAs.csv"); View(gpa)
  
  # Question: Is this data paired?
  
  # View the basic properties of data columns
  summary(gpa)
  
  # We must eliminate NA's from the second vector
  # (e.g., to calculate sample standard deviations)
  
  j = gpa$Juniors
  # Seniors GPA column contains NA's:
  gpa$Seniors
  is.na(gpa$Seniors)
  !is.na(gpa$Seniors)
  
  s = gpa$Seniors[!is.na(gpa$Seniors)]
  
  # check the lengths of these vectors
  length(j); length(s)
  
  # Lets take a look at densities first
  myed(j,xlab="GPA",main="Juniors",pcol="lightblue",dcol="blue3")
  myed(s,xlab="GPA",main="Seniors",pcol="khaki",dcol="orange3")
  
  # without custom functions:
  # dj = density(j)
  # plot(dj, xlab="GPA", main="Juniors"); grid()
  # polygon(dj,col="lightblue3")
  # 
  # ds = density(s)
  # plot(ds, xlab="GPA", main="Seniors"); grid()
  # polygon(ds,col="orange2")
  
  # Question: Pooled or separate variance?
  sd(j); sd(s); sd(j)/sd(s)
  
  # Ratio is 1.53: use the t.test() with var.equal = TRUE
  
  # Pooled variance:
  t.test(j, s, paired=F, var.equal=T, conf.level=0.95)
  t.test(j, s, var.equal=T)                            # same
  
  # Separate variance (Welch two sample t-test)
  t.test(j, s, var.equal=F)
  
  # Note: if we try a paired two sample t-test we get an error:
  t.test(j, s, paired=TRUE)
  
  # ----------------------------------------------------------------------
  
  # Quantile Plots: Help in consideration of
  #                "Does the data set come from a given distribution?"
  
  # Standard Normal Quantile Plot
  
  n = 5
  y = seq(0.5/n,1,1/n); y
  q = qnorm(y); q
  rn = rnorm(n)
  sort(rn)
  plot(q,sort(rn),xlim=c(-4,4),ylim=c(-4,4),
       pch=19,cex=0.75,col="blue",axes=F,
       main="Standard Normal Q-Q Plot")
  myaxes(0,0)
  abline(a=0,b=1,lwd=2,col="orange")
  
  # Built-in R function
  qqnorm(rn,pch=19,col="blue",xlim=c(-4,4),ylim=c(-4,4),axes=F)
  myaxes(0,0)
  abline(a=0,b=1,lwd=2,col="orange")
  
  # Try it for various samples of length 1000
  
  qqnorm(rnorm(1000),pch=19,cex=.5,col="blue",xlim=c(-4,4),ylim=c(-4,4),axes=F)
  myaxes(0,0)
  abline(a=0,b=1,lwd=2,col="orange")
  
  qqnorm(rexp(1000),pch=19,col="blue",xlim=c(-4,4),ylim=c(-4,4),axes=F)
  myaxes(0,0)
  abline(a=0,b=1,lwd=2,col="orange")
  
  qqnorm(runif(1000),pch=19,col="blue",xlim=c(-4,4),ylim=c(-4,4),axes=F)
  myaxes(0,0)
  abline(a=0,b=1,lwd=2,col="orange")
  
  # Problem: what if we qqplot some other normal random numbers 
  qqnorm(rnorm(1000,-4,12),pch=19,col="blue",axes=F)
  myaxes(0,0)
  abline(a=0,b=1,lwd=2,col="orange")
  
  # Idea: N(-4,12^2) rn's should be transformed into standard normal
  qqnorm(trv(rnorm(1000,-4,12),-4,12),pch=19,col="blue",axes=F)
  myaxes(0,0)
  abline(a=0,b=1,lwd=2,col="orange")
  
  # Custom Standard Normal Quantile Plot function
  
  snQQplot = function(v,ylim=c(-4,4),col="blue2",main=NULL)
  {
    n = length(v)     # sample size
    plot(qnorm(seq(0.5/n,1,1/n)),sort(v),
         xlim=c(-4,4),ylim=ylim,
         pch=19,cex=0.5,col=col,
         col.main=col,font.main=1,cex.main=1,axes=F,
         xlab="Theoretical quantiles",ylab="Sample quantiles",
         main=paste("Normal Q-Q Plot:",n,main,"points"))
    myaxes(0,0,lwd=1.5)
    abline(a=0,b=1,lwd=1/5,col="orange")
  }
  
  # How is snQQplot different than qqplot:
  rv = rexp(85)
  # qqplot:
  qqnorm(rv,pch=19,col="blue",xlim=c(-4,4),ylim=c(-4,4),axes=F)
  myaxes(0,0); abline(a=0,b=1,lwd=2,col="orange")
  
  # same plot using snQQplot: set trFlg = F
  snQQplot(rv,main="Exp(1)")
  
  # strnormQQplot with transformed rv:
  snQQplot(trv(rv),main="Transformed Exp(1)",col="purple")
  
  # known population parameters: generate normal random vector
  #    and check the sample mean and sample standard deviation
  n = 85
  rn = rnorm(n,-4,12)
  qqnorm(rnorm(1000,-4,12),xlim=c(-4,4),ylim=c(-40,40),
         pch=19,col="blue",axes=F)
  myaxes(0,0); abline(a=0,b=1,lwd=2,col="orange")
  # same plot
  snQQplot(rn,ylim=c(-40,40),main="N(-4,12)")
  
  # see the difference in quantile plots of transformed sample
  snQQplot(trv(rn),main="Transformed N(-4,12)")       # sample mean and sample sd
  snQQplot(trv(rn,-4,12),main="Transformed N(-4,12)") # known pop.mean and pop.sd
  
  # Generate rns from other distributions
  snQQplot(trv(rnorm(n)),main="Transf. N(0,1)")
  snQQplot(trv(runif(n)),main="Transf. Unif(0,1)",col="green3")
  snQQplot(trv(rexp(n)),main="Transf. Exp(1)",col="red2")
  
  # Now compare the plots
  par(mfrow=c(2,2))                         # set 2x2 panel for 4 plots
  snQQplot(trv(rnorm(n)),main="Transf. N(0,1)")
  snQQplot(trv(runif(n)),main="Transf. Unif(0,1)",col="green3")
  snQQplot(trv(rexp(n)),main="Transf. Exp(1)",col="red2")
  snQQplot(trv(rt(n,5)),main="Transf. t(5)",col="purple")
  par(mfrow=c(1,1))                         # reset panel back to single plot
  
  # re-run these lines for larger n's
  
  # ----------------------------------------------------------------------
  
  # Question: "Is data normally distributed?"
  
  # This is a brief preview into answering this question.
  # The topic is discussed in optional Session 9+ 
  
  # GMAT data: example from Session 8
  
  adm <- read.csv("Data/Admission.csv"); head(adm)
  
  length(adm$GMAT)
  # Densities of the GPA and GMAT vectors
  myed(adm$GPA,pcol="lightyellow",dcol="red2",xlab="average",
       main="GPA data density")
  myed(adm$GMAT,pcol="azure2",dcol="green4",xlab="average",
       main="GMAT data density")
  
  # Compute the sample mean and sample sd (sqrt of sample variance)
  sm = mean(adm$GMAT); sm
  ssd = sd(adm$GMAT); ssd
  x = seq(200,800)
  lines(x,dnorm(x,mean=sm,sd=ssd),lwd=2,col="blue")
  
  # increase the y-axis limit
  myed(adm$GMAT,ylim=c(0,0.005),pcol="azure2",dcol="green4",
       xlab="average",main="GMAT data density")
  lines(x,dnorm(x,mean=mean(adm$GMAT),sd=sd(adm$GMAT)),lwd=2,col="blue")
  abline(v=sm,lwd=2,lty=2,col="blue")
  
  # without custom functions:
  # d_GMAT = density(adm$GMAT)
  # plot(d_GMAT, lwd=3, col="green4",
  #      xlab="points", main="GMAT data density"); grid()
  # polygon(d_GMAT,col="azure2")
  # 
  # x = seq(200,800)
  # lines(x,dnorm(x,mean=sm,sd=ssd),lwd=2,col="blue")
  # abline(v=sm,lwd=2,lty=2,col="blue")
  
  # Is GMAT data normally distributed? Try with a snQQplot:  
  snQQplot(trv(adm$GMAT),main="Transf. GMAT")
  # How different is it from the snQQplot of the true Normal random sample 
  snQQplot(trv(rnorm(85,sm,ssd)),col="green4",main="Transf. Normal")
  
  # see Session 9+ for continuation on the topic  
  
  # ----------------------------------------------------------------------------Session 9+
  
  # download the updated version of myFunctions.R script
  # and store it in the same folder ass this script
  
  source("myFunctions.R")
  
  # Question: "Does the data set come from a normal distribution?"
  #
  #   Normal distribution is used here to illustrate the approach,
  #   yet it can easily be replaced by any standard distribution
  #   (i.e., any distribution that has random number generator and 
  #   quantile function in R)
  #
  # In Session 9 we compared the snQQplots of
  #     GMAT data and the true Normal random sample.
  # We want to quantify 'deviations' of the snQQplot points from
  # the diagonal (orange line in the plot)  
  
  # An often used function below returns the Standard Normal theoretical
  # quantiles for the mid-points of the n equally spaced sub-intervals
  
  snqts = function(n)
    return(qnorm(seq(0.5/n,1,1/n)))
  
  # For example, for n = 5 the subintervals of [0,1] are
  #  [0, 0.2], [0.2, 0.4], [0.4, 0.6], [0.6, 0.8], [0.8, 1],
  # and their midpoints are 0.1, 0.3, 0.5, 0.7, and 0.9.
  
  qnorm(c(0.1,0.3,0.5,0.7,0.9))
  snqts(5)
  
  # We want to quantify the deviations of plotted points from the diagonal
  # (i.e., the differences between the sample and the theoretical quantiles)
  #
  # We can measure these deviations in many ways.
  # The function below calculates two such deviations:
  #   Euclidean: Square root of 
  #              [sum of the squares of differences divided by number of points]
  #     MaxNorm: Maximum of the absolute values of the differences
  
  # About the input parameters:
  # If the population mean and standard deviation are known they can
  # be entered as optional parameters.
  # Otherwise the sample mean and sample standard deviation are used.
  # Theoretical quantiles can aso be provided as an optional argument:
  # this speeds up the execution when the function is replicated many
  # times since the theor. quantiles only depend on sample size and do
  # not need to be recalculated if the sample sizes stay the same.
  
  snQQdevs = function(v,qts=snqts(length(v)))
  {
    Deltas = sort(v)-qts
    return(c(sqrt(sum(((Deltas^2)/length(v)))),max(abs(Deltas))))
  }
  
  # Try it
  n = 85
  snQQdevs(rnorm(n))      # N(0,1) vector
  snQQdevs(runif(n))      # Unif(0,1) vector
  snQQdevs(rexp(n))       # Exp(1) vector
  snQQdevs(rt(n,5))       # t(5) vector
  
  # For transformed random vectors
  snQQdevs(trv(rnorm(n))) # Transf. N(0,1) vector
  snQQdevs(trv(runif(n))) # Transf. Unif(0,1) vector
  snQQdevs(trv(rexp(n)))  # Transf. Exp(1) vector
  snQQdevs(trv(rt(n,5)))  # Transf. t(5) vector
  
  # The corresponding values are visibly larger for three distributions
  # other than normal. Can the observed discrepancy be used to provide,
  # within some confidence, the decisive answer whether the sample came
  # from the normal distribution or not?  
  
  # Idea: We can use random number generator rnorm() to quantify
  #       how much the snQQ plots of the N(0,1) samples deviate
  #       from the diagonal?
  # How?
  #  Replicate 'snQQdevs(trv(rnorm(n)))' many times (say m = 500,000),
  #  where n is the size of the sample(s) for which the deviations in
  #  snQQplot() will be calculated (in the examples above n = 85)
  #  This will store Euclidean and MaxNorm deviations to a matrix
  #  with two rows and m columns:
  #     - the first row is a vector of Euclidean deviations, and
  #     - the second row is the vector of MaxNorm deviations.
  #
  #  These vectors are Sampling Distribution Vectors for Euclidean and
  #  MaxNorm deviations of N(0,1) samples, respectively.
  #  We can use density() function to plote these SDVs and the resulting
  #  empirical densities are empirical approximations of the
  #  true pdf's for the two deviation in consideration.  
  
  # Try it for m = 8 and n = 85, just to see the matrix:
  n = 85
  replicate(8,snQQdevs(trv(rnorm(n)),snqts(n)))
  
  # In our examples above n=85. Let's generate sampling distributions
  # of length m = 500000; call it devs85.
  # Note that this will take some serious time to execute.
  
  devs85 = replicate(500000,snQQdevs(trv(rnorm(n)),snqts(n)))
  
  # Now plot the empirical densities of the two deviations vectors  
  # Input parameters:
  # - n (as above) is used only in the plot titles
  # - markers is an optional vector of length 2. If provided,
  #   it plots vertical lines on the respective Sampling Distr, plots
  #   It will be used in the next block.
  
  devsplot = function(devs,n,markers=c(NULL,NULL),
                      main=NULL,sub1=NULL,sub2=NULL)
  {
    par(mfrow=c(2,1))          # set 1x2 panel for 2 plots
    main=p("deviations Sampling Distr. of",main,"samples of size",n)
    myed(devs[1,],pcol="palegreen",dcol="green4",
         main=p("Euclidean",main),sub=sub1) 
    abline(v=markers[1],lwd=3,col="orange3")
    myed(devs[2,],pcol="lightblue",dcol="royalblue",
         main=p("MaxNorm",main),sub=sub2) 
    abline(v=markers[2],lwd=3,col="orange3")
    par(mfrow=c(1,1))          # reset panel for single plot
  }
  
  # Now we can visualize sampling distribution for Euclidean and MaxNorm
  # Q-Q plot deviations for Transf.Standard Normal sample of size n = 85
  n = 85
  devsplot(devs85,n,main="Transf.N(0,1)")
  
  # Finally we need to see where the Euclidean deviation of a sample
  # of size n (like rnorm(n) or runif(n) above) fit inside the
  # Euclidean sampling distribution (the green density), and its MaxNorm
  # deviation inside MaxNome sampling distribution (the blue density)  
  
  # Start with a normal sample of size n = 85: run it multiple times
  devsplot(devs85,n,snQQdevs(trv(rnorm(n))),main="TrSN")
  
  # Now try Unif sample of size n = 85: run it multiple times
  devsplot(devs85,n,snQQdevs(trv(runif(n))),main="TrSN")
  
  # Also try Exp sample of size n = 85: run it multiple times
  devsplot(devs85,n,snQQdevs(trv(rexp(n))),main="TrSN")
  
  # In the latter case each vertical line (that depicts the sample
  # Q-Q plot deviation) is quite often in the (right) tail of its
  # respective sampling distribution.
  
  # Notice that the sampling distributions have only the right tail,
  # since the measured deviations are always positive and being
  # close to zero indicates that the sample Q-Q plot lies very
  # close to the diagonal. In other words, if the sample deviation
  # is close to zero we are more confident the sample came from the
  # Normal distribution.
  
  # The opposite holds for the right tail: further away in the tail 
  # the sample deviation is, it is more likely that sample is not
  # from the Normal distribution.
  
  # Hence we got the empirical p-value: it is the "mass" of the
  # sampling distribution to the right of the deviation line.
  # However, since the sampling distribution is a length m vector
  # (and the plot we is its empirical density), we really count
  # how many entries in the vector are larger than the sample
  # deviation (i.e., to the right of the vertical deviation line)
  # and divide that count by m.
  
  # The next function returns the 2x2 matrix:
  # First row are sample Euclidean and MaxNorm deviations
  # Second row are their corresponding empirical p-values.
  # Note that the sample size must be the same as the sizes
  # of synthetic samples used to generate devsn (devs85 above)
  
  devspvals = function(v,devsn)
  {
    n = length(v)                          # sample size n
    sdevs = snQQdevs(v,qts=snqts(n))       # sample deviations
    euDev = sdevs[1]                       # sample Euclidean deviation
    mnDev = sdevs[2]                       # sample MaxNorm deviation
    euSDV = devsn[1,]                      # Euclidean Sampling Distr vector
    mnSDV = devsn[2,]                      # MaxNorm Sampling Distr vector
    eupVal = length(euSDV[euSDV>=euDev])/length(euSDV)  # Euclidean dev. p-value
    mnpVal = length(mnSDV[mnSDV>=mnDev])/length(mnSDV)  # MaxNorm dev. p-value
    return(matrix(c(euDev,eupVal,mnDev,mnpVal),nrow=2,ncol=2))
  }
  
  # Run each line several times and observe how the p-values
  # in second row are much smaller for non-normal samples
  n = 85
  devspvals(trv(rnorm(n)),devs85)
  devspvals(trv(runif(n)),devs85)
  devspvals(trv(rexp(n)),devs85)
  devspvals(rt(n,5),devs85)
  
  # The next function uses the pre-computed devsn matrix, plots
  # the sample both deviations within their respective sampling 
  # distributions and calculates the empirical p-values. 
  
  sampledevsplot = function(v,devsn,main=NULL,name=NULL)
  {
    s = devspvals(v,devsn)
    n = length(v)
    subEu = paste("Your",name,"sample:  Size:",n,
                  "  Deviation:",round(s[1,1],4),
                  "  p-value:",round(s[2,1],4))
    subMN = paste("Your",name,"sample:  Size:",n,
                  "  Deviation:",round(s[1,2],4),
                  "  p-value:",round(s[2,2],4))
    devsplot(devsn,n,s[1,],main,subEu,subMN)
  }
  
  # Execute each of the lines several times
  n = 85
  sampledevsplot(trv(rnorm(n)),devs85,"TrSN","TrSN")
  sampledevsplot(trv(runif(n)),devs85,"TrSN","Transf.Unif(0,1)")
  sampledevsplot(trv(rexp(n)),devs85,"TrSN","Transf.Exp(1)")
  sampledevsplot(trv(rt(n,5)),devs85,"TrSN","Transf.t(5)")
  
  # More important than visualization:
  #   If we collect many p-values from devspvals() 2nd row output
  #   for Transformed Standard Normal vectors, they should be
  #   uniformly distributed on [0,1]:
  
  # Note: the next line takes very long time to execute
  pVals = replicate(10000,devspvals(trv(rnorm(n)),devs85)[2,])
  
  par(mfrow=c(2,1))          # set two subpanels
  myhist(pVals[1,],breaks=seq(0,1,0.01),
         col="palegreen",border="green4",
         main="Distribution of Euclidean Deviations p-values",
         sub="from Transformed N(0,1) samples")
  myhist(pVals[2,],breaks=seq(0,1,0.01),
         col="lightblue",border="royalblue",
         main="Distribution of MaxNorm Deviations p-values",
         sub="from Transformed N(0,1) samples")
  par(mfrow=c(1,1))          # reset to single panel
  
  # Do the same for Transformed Unif(0,1) vectors
  
  # Note: the next line takes very long time to execute
  UnifpVals = replicate(1000,devspvals(trv(runif(n)),devs85)[2,])
  
  par(mfrow=c(2,1))          # set two subpanels
  myhist(UnifpVals[1,],breaks=seq(0,0.1,0.01),
         col="palegreen",border="green4",
         main="Distribution of Euclidean Deviations p-values",
         sub="from Transformed Unif(0,1) samples")
  myhist(UnifpVals[2,],breaks=seq(0,1,0.01),
         col="lightblue",border="royalblue",
         main="Distribution of MaxNorm Deviations p-values",
         sub="from Transformed Unif(0,1) samples")
  par(mfrow=c(1,1))          # reset to single panel
  
  # Notice that the p-values for Uniform data are very small
  # Zoom in to x-axis:
  
  par(mfrow=c(2,1))          # set two subpanels
  myhist(UnifpVals[1,],breaks=seq(0,0.3,l=101),
         col="palegreen",border="green4",
         main="Distribution of Euclidean Deviations p-values",
         sub="from Transformed Unif(0,1) samples")
  myhist(UnifpVals[2,],breaks=seq(0,0.3,l=101),
         col="lightblue",border="royalblue",
         main="Distribution of MaxNorm Deviations p-values",
         sub="from Transformed Unif(0,1) samples")
  par(mfrow=c(1,1))          # reset to single panel
  
  # Create the histograms for the transformed Exp(1) vectors
  # You will see that the p-values gather even more around zero
  # than in the  case of uniform data
  
  # Important Warning:
  #   the pre-computed devs85 matrix CAN ONLY be used for samples
  #   of size n = 85. If you want to verify the "normality" for a
  #   sample of some other size, say 500, then you need to
  #   pre-compute the matrix with a call
  #         snQQdevsSDVs(m,500)
  #   where m is preferablly very large.
  
  # ----------------------------------------------------------------------
  
  # We apply this analysis for the admission data sets.
  # In other words: can we justify assumption that admission
  #                 data is normally distributed?
  
  adm <- read.csv("Data08/Admission.csv"); head(adm)
  
  # GPA data empirical density
  myed(adm$GPA,pcol="lightyellow",dcol="red2",xlab="average",
       main="GPA data density")
  
  # GMAT data empirical density and comparison to normal pdf
  myed(adm$GMAT,ylim=c(0,0.005),pcol="azure2",dcol="green4",
       xlab="average",main="GMAT data density")
  sm = mean(adm$GMAT); sm
  ssd = sd(adm$GMAT); ssd
  x = seq(200,800)
  lines(x,dnorm(x,mean=sm,sd=ssd),lwd=2,col="blue")
  abline(v=sm,lwd=2,lty=2,col="blue")
  
  # Session 9 question: Is GMAT data normally distributed? 
  snQQplot(trv(adm$GMAT),main="GMAT")
  # How different is it from the snQQplot of the true Normal random sample 
  snQQplot(trv(rnorm(85,sm,ssd)),col="green4",main="Transf.Normal")
  
  # Now we can do much more:
  sampledevsplot(trv(adm$GMAT),devs85,"GMAT")
  
  # Same question for the GPA vector:
  # we re-use devsSDs85 since GPA sample is also of size 85
  sampledevsplot(trv(adm$GPA),devs85,"GPA data")
  
  # Conclusion:
  # The GPA data seems "less normal" than the GMAT data since both
  # empirical p-values are smaller than their GMAT counterparts.
  # Still the p-values of 0.14 for the Euclidean deviation and 
  # and 0.23 for MaxNorm deviation would result in the acceptance
  # of the null-hypothesis that it is a normal data, under any
  # reasonable significance level.
  
  # ----------------------------------------------------------------------
  
  # Standard R function: Shapiro-Wilkes test for normality
  
  shapiro.test(adm$GMAT)
  shapiro.test(adm$GPA)
  
  # Question: How are the p-values of this test distributed for N(0,1) samples?
  
  s = replicate(100000,shapiro.test(rnorm(85))$p.value)
  myhist(s,breaks=seq(0,1,0.01),ylim=c(0,1.2),
         col="lightgreen",border="green4",
         main="Distribution of Shapiro-Wilkes Test p-values")
  
  
  
  #-----------------------------------------------------------------------------Session 10
  
  
  #    Hypothesis Testing and Confidence Intervals 
  #     via Sampling Distributions generated in R
  
  
  source("myFunctions.R")    # load custom functions
  
  # ----------------------------------------------------------------------
  
  # 1st Approach: Make (a justified) assumption about population distr.
  
  # ----------------------------------------------------------------------
  
  # Example 1:  (example from Session 8)
  
  # Test the null hypothesis that the population mean of GMAT data is 510.
  
  adm <- read.csv("Data/Admission.csv"); head(adm)
  GMAT = adm$GMAT
  summary(GMAT)
  
  # From Session 9: GMAT data density, sample mean, etc.
  myhd(GMAT,ylim=c(0,0.005),col="azure2",border="green4",
       xlab="GMAT score",main="GMAT data density")
  myed(GMAT,ylim=c(0,0.005),pcol="azure2",dcol="green4",
       xlab="GMAT score",main="GMAT data density")
  sm = mean(GMAT); sm
  ssd = sd(GMAT); ssd
  x = seq(220,800)
  lines(x,dnorm(x,mean=sm,sd=ssd),lwd=2,col="blue")
  abline(v=sm,lwd=2,lty=2,col="blue")
  
  # Is GMAT data normally distributed? Try with a snQQplot:  
  snQQplot(trv(GMAT),main="Transf. GMAT")
  # How different is it from the snQQplot of the true Normal random sample?
  snQQplot(trv(rnorm(85,sm,ssd)),col="green4",main="Transf. Normal")
  
  # KEY: Based on observed GMAT density and the quantile plot WE ASSUME
  #      the population distribution is Normal.
  #      (discussed in great detail, and justified, in Session 9+)  
  # The best (unbiased) estimators for population mean and variance are
  # Sample Mean and Sample Variance, respectively.  
  # Thus we assume population distribution is 
  #              N(sm,ssd^2) = N(488.4,81.5(square))
  
  # The sample size is n = 85 (there are 85 rows in the dataframe).
  n = length(GMAT)
  
  # To create one sample
  rnorm(n,sm,ssd)
  
  # To compute the metric (mean) for one sample
  mean(rnorm(n,sm,ssd))
  
  # Finally, replicate this 50 thousand times to get a 
  # Sampling Distribution Vector for the Population Mean
  m = 50000
  sdv1a = replicate(m,mean(rnorm(n,sm,ssd)))
  
  # Plot the empirical density of the sampling distribution vector
  
  # Note: 'Sampling Distribution' and 'Sampling Distribution Vector'
  #   are synonyms. The generated vector is a 'representative'
  #   of the distribution we (usually) don't know exactly.
  #   The way we visialize vector 'representing' the distribution
  #   is via the empirical density.  Hence it is reasonable to refer to
  #   the Empirical Density of Sampling Distribution Vector as
  #   'Sampling Distribution' as well.
  
  myed(sdv1a,cntFlg=F,
       main=paste("Pop.Mean Sampling Distribution from Normal sample of size",n),
       sub="assuming GMAT data mean and variance")
  
  myed(sdv1a,ylim=c(0,0.05),cntFlg=F,
       main=paste("Pop.Mean Sampling Distr. from Normal sample of size",n),
       sub="assuming GMAT data mean and variance")
  
  # without custom functions:
  d_sdv1a = density(sdv1a)
  plot(d_sdv1a,lwd=3,col="green2",
        main=paste("Pop.Mean Sampling Distribution from Normal sample of size",n),
        "assuming GMAT data mean and variance"); grid()
   polygon(d_sdv1a,col="lightgreen")
  
  # Note: in this case we know the true distribution of the sampling
  #       distribution: N(488.4, 81.5/sqrt(85))
  # Question: what is the "half-mass" point (median)?
  abline(v=median(sdv1a),lwd=3,col="magenta",lty=3)
  # Where does the sm fit in?
  abline(v=sm,lwd=2,lty=2,col="blue")
  
  # Null-hypothesis H0: GMAT population mean is 510
  mu0 = 510
  
  # How far from sm is the mu0?
  abline(v=mu0,lwd=1.5,col="red",lty=3)
  abline(v=sm+sm-mu0,lwd=1.5,col="red",lty=3)
  
  # Empirical p-value (see the plot)
  left_p = length(sdv1a[sdv1a<sm+sm-mu0])
  right_p = length(sdv1a[sdv1a>mu0])
  pval = (left_p+right_p)/length(sdv1a); pval
  
  # In order to avoid keeping track of which of the two values,
  # mu0 and sm+sm-mu0 is larger/smaller,
  lowbd = min(mu0,sm+sm-mu0)    # lower bound
  uppbd = max(mu0,sm+sm-mu0)    # upper bound
  
  # Compute the probabilities 'beyond' these lines to obtain
  # the p-values for the hypothesis
  left_p = length(sdv1a[sdv1a<lowbd])
  right_p = length(sdv1a[sdv1a>uppbd])
  pval = (left_p+right_p)/length(sdv1a); pval
  
  # We reject the hypothesis at significance level 0.05.
  
  # Since this is a test for the population mean, we can compare to built-in t-test:
  t.test(GMAT,mu=mu0,conf.level = 0.95)
  
  # Related question: What is the empirical conf.interval for 95% confidence?
  myeCI(sdv1a,0.95)
  abline(v=myeCI(sdv1a,0.95),lwd=3,lty=2,col="orange2")  
  
  # without custom functions:
  # left_ep = quantile(sdv,0.025)
  # right_ep = quantile(sdv,0.975)
  # CI = c(left_ep,right_ep); CI
  # abline(v=CI,lwd=1.5,col="purple")
  
  # As in the classical t-test we shall often AVOID the computation of p-values
  # and use empirical confidence interval instead.
  
  # Conclusion: Since the hypothesized value mu0 = 510 is outside the two-sided
  #             empirical 95% confidence interval, we reject the null hypothesis
  #             in favor of the alternative: Population mean is not equal 510.  
  
  
  # Useful wrap (to avoid cutting and pasting same lines over and over again)
  # Function plots the sampling distribution density and marks confidence interval
  #   on the plot. It also returns the confidence interval endpoints.  
  
  mysdci = function(sdv,xlim=NULL,ylim=NULL,xlab="",main=NULL,sub=NULL,cntFlg=F,
                    pcol="palegreen",dlwd=2,dcol="green4",axes=T,conf=0.95,rnd=2)
  {
    CI = myeCI(sdv,conf)
    ssub = paste(100*conf,"% Empirical Confidence Interval: (",
                 round(CI[1],rnd),", ",round(CI[2],rnd),")",sep="")
    if (!is.null(sub)) sub = c(sub,ssub) else sub = ssub
    myed(sdv,xlim=xlim,ylim=ylim,xlab=xlab,main=main,sub=sub,cntFlg=cntFlg,
         pcol=pcol,dlwd=dlwd,dcol=dcol,axes=axes)
    abline(v=median(sdv),lwd=2,col="magenta",lty=2)
    abline(v=CI,lwd=3,lty=2,col="orange2")  
    return(CI)
  }
  
  # Apply it to 'sdv1a' sampling distribution vector:
  mysdci(sdv1a,ylim=c(0,0.05),
         main=paste("Pop.Mean Sampling Distr. from Normal samples of size",n),
         sub="assuming GMAT data mean and variance")
  abline(v=sm,lwd=2,col="blue",lty=5)       # GMAT data sample mean (average)
  abline(v=mu0,lwd=2,col="red2")            # hypothesized value: mu0 = 510
  
  
  # Note: In classical theory CI for population mean depends only on
  #       the sample and the confidence level.
  # In this case the empirical CI depends on the generated sdv.
  # How much of the variation should we see?  
  # We can certainly repeat CI calculations say M = 100 times.
  # But we must reduce m to avoid long execution times
  
  m = 1000; M = 100
  CIsdv1a = rpl(M,myeCI(rpl(m,mean(rnorm(n,sm,ssd)))))
  
  par(mfrow=c(2,1))
  myhd(CIsdv1a[1,],breaks=10,cntFlg=F,main="95% CI's left end-points")
  myhd(CIsdv1a[2,],breaks=10,cntFlg=F,main="95% CI's right end-points",
       col="lightblue",border="blue2",dcol="blue")
  par(mfrow=c(1,1))
  
  # wrap it as an auxiliary function:
  
  myCIsdv = function(CIsdv,marker=NULL,ylim=NULL,conf=0.95,rnd=2)
  {
    par(mfrow=c(2,1))
    b = ceiling(sqrt(length(CIsdv)/2))
    main = paste(100*conf,"% CI's ",sep=""); sub=NULL
    if (!is.null(marker))
      sub = paste(" (",round(marker[1],rnd)," indicated)",sep="")
    myhd(CIsdv[1,],breaks=b,ylim=ylim,cntFlg=F,main=paste(main,"left end-points",sub))
    abline(v=marker[1],lwd=3,lty=2,col="orange2") 
    if (!is.null(marker))
      sub = paste(" (",round(marker[2],rnd)," indicated)",sep="")
    myhd(CIsdv[2,],breaks=b,ylim=ylim,cntFlg=F,main=paste(main,"right end-points",sub),
         col="lightblue",border="blue2",dcol="blue")
    abline(v=marker[2],lwd=3,lty=2,col="slateblue") 
    par(mfrow=c(1,1))
  }
  
  myCIsdv(CIsdv1a,myeCI(sdv1a))
  myCIsdv(CIsdv1a,myeCI(sdv1a),ylim=c(0,0.6))
  
  # ----------------------------------------------------------------------------------------
  
  # Example 2:
  
  # Empirical Confidence Interval for population standard deviation
  # "Null-hypothesis: population standard deviation is 78"
  
  # Lineup as before: Sample size, mean, and standard deviation of GMAT data
  # n = length(GMAT); n
  # sm = mean(GMAT); sm
  # ssd = sd(GMAT); ssd
  
  # Hypothesized Population Standard Deviation value
  sd0 = 78
  
  # "Metric" is the  sample standard deviation: we need to replicate it many times
  sd(rnorm(n,sm,ssd))
  
  # Get the Sample Standard Deviation of 50,000 replicates
  #   (stored as sampling distribution vector) and plot its density 
  m = 50000
  sdv2a = replicate(m,sd(rnorm(n,sm,ssd)))
  mysdci(sdv2a,ylim=c(0,0.07),pcol="khaki",dcol="brown",
         main=paste("Pop.St.Dev. Sampling Distr. from Normal samples of size",n),
         sub="assuming GMAT data mean and variance")
  abline(v=sd0,lwd=2,col="red2")            # hypothesized value: sd0 = 78
  abline(v=ssd,lwd=2,col="blue",lty=5)      # sample sd of GMAT data
  
  # Test conclusion: We accept the null-hypothesis at significance level 0.05.
  
  # CI variation:
  m = 1000; M = 100
  CIsdv2a = rpl(M,myeCI(rpl(m,sd(rnorm(n,sm,ssd)))))
  myCIsdv(CIsdv2a,myeCI(sdv2a))
  myCIsdv(CIsdv2a,myeCI(sdv2a),ylim=c(0,1.2))
  
  # ----------------------------------------------------------------------------------------
  
  # Example 3:
  
  # Empirical Confidence Interval for the population 75th percentile
  # "Null-hypothesis: Population 75th percentile is 600"
  
  qnt = 0.75        # 75th percentile = 0.75 quantile
  # qnt = 0.99        # 99th percentile
  # qnt = 1           # maximum
  
  # Sample size, mean, standard deviation, and the desired percentile of GMAT data
  # n = length(GMAT); n
  # sm = mean(GMAT); sm
  # ssd = sd(GMAT); ssd
  sqnt = quantile(GMAT,qnt); sqnt
  
  # Hypothesized Population 75th Percentile (Third Quartile) value
  qnt0 = 600
  
  # "Metric" is qnt quantile
  quantile(rnorm(n,sm,ssd),qnt)
  
  # Get the desired quantile of 50,000 replicates and plot density
  m = 50000
  sdv3a = replicate(m,quantile(rnorm(n,sm,ssd),qnt))
  mysdci(sdv3a,pcol="lightblue",dcol="royalblue",
         main=p("Pop.",100*qnt,"th Percentile Sampling Distr. from Normal samples of size ",n,sep=""),
         sub="assuming GMAT data mean and variance")
  abline(v=qnt0,lwd=2,col="red2")           # hypothesized value: qnt0 = 600
  abline(v=sqnt,lwd=2,col="blue",lty=5)     # GMAT data 75th Empirical Percentile
  
  # Test conclusion: Since 600 is outside the conf interval we reject the null-hypothesis
  
  # CI variation:
  m = 1000; M = 100
  CIsdv3a = rpl(M,myeCI(rpl(m,quantile(rnorm(n,sm,ssd),qnt))))
  myCIsdv(CIsdv3a,myeCI(sdv3a))
  myCIsdv(CIsdv3a,myeCI(sdv3a),ylim=c(0,0.6))
  
  # TRY repeating for other percentiles
  
  # ----------------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------------
  
  # Second approach: Make NO assumptions about population distribution,
  #                  instead use only the data points and create
  #                  simulated samples from these points (re-sampling)
  
  # ----------------------------------------------------------------------------------------
  
  # The sampling distr. will be obtained by "resampling": use the built-in function "sample"
  x = seq(1,5)
  
  sample(x,5,replace=T)    # we use re-sampling with replacement
  sample(x,5,replace=F)
  
  sample(x,12,replace=T)
  # sample(x,12,replace=F)
  
  # ----------------------------------------------------------------------------------------
  
  # Example 1 via resampling
  
  # Empirical Confidence Interval for the population mean
  # "Null-hypothesis: the population mean of GMAT data is 510"
  
  # Sample size (note: we do not need sm and ssd anymore)
  n = length(GMAT); n
  
  # In our case, we re-sample from the GMAT vector
  s = sample(GMAT,n,replace=T)
  # check the mean of the sample obtained by re-sampling
  mean(s)  
  
  # Replicate it
  m = 50000
  sdv1b = replicate(m,mean(sample(GMAT,n,T)))
  # and plot sampling distribution and CI
  mysdci(sdv1b,ylim=c(0,0.05),
         main="Pop.Mean Sampling Distr. via resampling from GMAT data")
  abline(v=mu0,lwd=2,col="red2")            # hypothesized value: mu0 = 510
  abline(v=sm,lwd=2,col="blue",lty=5)       # GMAT data sample mean (average)
  
  # CI variation:
  m = 1000; M = 100
  CIsdv1b = rpl(M,myeCI(rpl(m,mean(sample(GMAT,n,T)))))
  myCIsdv(CIsdv1b,myeCI(sdv1b))
  myCIsdv(CIsdv1b,myeCI(sdv1b),ylim=c(0,0.6))
  
  
  # Try it for another N(sm,ssd^2) vector
  w = rnorm(n,sm,ssd)
  sdvn = replicate(m,mean(sample(w,n,T)))
  mysdci(sdvn,pcol="olivedrab1",dcol="olivedrab",xlim=c(420,550),ylim=c(0,0.05),
         main=p("Pop.Mean Sampling Distr. via resampling from random Normal sample of size",n),
         sub="assuming GMAT data mean and variance")
  abline(v=mean(w),lwd=2,col="blue",lty=5)       # w sample mean (average)
  abline(v=sm,lwd=2,col="dodgerblue",lty=1)      # GMAT average
  
  # Try it for another population distribution
  w = rexp(n)
  sdve = replicate(m,mean(sample(w,n,T)))
  mysdci(sdve,pcol="pink",dcol="purple",xlim=c(0.5,1.8),ylim=c(0,5),
         main=p("Pop.Mean Sampling Distr. via resampling from",n,"Exp(1) data pts"))
  abline(v=mean(w),lwd=2,col="blue",lty=5)       # w sample mean (average)
  abline(v=1,lwd=2,col="dodgerblue",lty=1)       # pop mean of Exp(1)
  
  # ----------------------------------------------------------------------------------------
  
  # Example 2 via resampling
  
  # Empirical Confidence Interval for the population standard deviation
  # "Null-hypothesis: population standard deviation is 78"
  
  m = 50000
  sdv2b = replicate(m,sd(sample(GMAT,n,T)))
  mysdci(sdv2b,ylim=c(0,0.08),pcol="khaki",dcol="brown",
         main="Pop.St.Dev. Sampling Distr. via resampling from GMAT data")
  abline(v=sd0,lwd=2,col="red2")            # hypothesized value: sd0 = 78
  abline(v=ssd,lwd=2,col="blue",lty=5)      # GMAT data Sample Standard Deviation
  
  # CI variation:
  m = 1000; M = 100
  CIsdv2b = rpl(M,myeCI(rpl(m,sd(sample(GMAT,n,T)))))
  myCIsdv(CIsdv2b,myeCI(sdv2b))
  myCIsdv(CIsdv2b,myeCI(sdv2b),ylim=c(0,1))
  
  # ----------------------------------------------------------------------------------------
  
  # Example 3 via resampling
  
  # Empirical Confidence Interval for the population 75th percentile
  # "Null-hypothesis: 75th percentile of the population is 600"
  
  qnt = 0.75
  # qnt = 0.5
  m = 50000
  
  sdv3b = replicate(m,quantile(sample(GMAT,n,T),qnt))
  # auxiliary string for the super-long chart title
  main = p("Pop.",100*qnt,
           "th Percentile Sampling Distr. via resampling from GMAT data",sep="")
  mysdci(sdv3b,pcol="lightblue",dcol="royalblue",main=main)
  abline(v=qnt0,lwd=2,col="red2")           # hypothesized value: qnt0 = 600
  abline(v=sqnt,lwd=2,col="blue",lty=5)     # GMAT data 75th Empirical Percentile
  
  # Why such bumpiness?  
  summary(sdv3b)
  # Key: How many distinct values does vector 'sdv3b' have?
  # Related: How many distinct values are in 'sdv1b'?
  length(sdv1b);  length(unique(sdv1b))
  length(sdv3b);  length(unique(sdv3b))
  # Get the frequency table for 'sdv3b':
  table(sdv3b)
  
  # Empirical density does a poor job in representing sparse data; try histogram
  myhist(sdv3b,freq=T,main=main,col="lightblue2",border="royalblue")
  myhist(sdv3b,freq=T,breaks=seq(min(sdv3b)-0.5,max(sdv3b)+0.5),
         main=main,col="lightblue2",border="royalblue")
  myhd(sdv3b,breaks=seq(min(sdv3b)-0.5,max(sdv3b)+0.5),cntFlg=F,main=main,
       col="lightblue2",border="royalblue")
  myhd2(sdv3b,cntFlg=F,main=main,col="lightblue2",border="royalblue")
  
  # sampling distribution for percentiles is very bumpy; What about CIs?
  
  # CI variation:
  m = 1000; M = 100
  CIsdv3b = rpl(M,myeCI(rpl(m,quantile(sample(GMAT,n,T),qnt))))
  myCIsdv(CIsdv3b,myeCI(sdv3b))
  myCIsdv(CIsdv3b,myeCI(sdv3b),ylim=c(0,6))
  
  # Unusual distribution? Check the actual frequencies:
  table(CIsdv3b[1,])   # Frequency table of left end-points
  table(CIsdv3b[2,])   # Frequency table of rightt end-points
  
  # Recall: sampling distribution for percentiles is very bumpy: this will be the
  #         case for all percentiles: try median (uncomment "qnt = 0.5" line)
  
  # Try it for a Normal N(0,1) sample of size 85 = length(GMAT)
  n = length(GMAT)
  # n = 5000
  w = rnorm(n)
  qnt = 0.75
  m = 50000
  sdvnr = replicate(m,quantile(sample(w,n,T),qnt))
  
  summary(sdvnr)
  length(sdvnr); length(unique(sdvnr))
  
  main = p("Pop.",100*qnt,
           "th Percentile Sampling Distr. via resampling from N(0,1) sample of size ",n,sep="")
  
  mysdci(sdvnr,pcol="lightblue2",dcol="navy",main=main)
  abline(v=qnorm(qnt),lwd=2,col="blue",lty=5)     # Pop. 75th Percentile of N(0,1)
  
  myhd(sdvnr,breaks=seq(min(sdvnr),max(sdvnr),l=101),cntFlg=F,
       main=main,col="lightblue2",border="navy")
  myhd2(sdvnr,cntFlg=F,main=main,col="lightblue2",border="navy")
  
  # CI variation:
  m = 1000; M = 100
  CIsdvnr = rpl(M,myeCI(rpl(m,quantile(sample(w,n,T),qnt))))
  myCIsdv(CIsdvnr,myeCI(sdvnr),ylim=c(0,300))
  
  table(CIsdvnr[1,])   # Frequency table of left end-points
  table(CIsdvnr[2,])   # Frequency table of right end-points
  
  
  # Same problems for larger samples (uncomment "n = 5000" line)
  
  # Conclusion: Bumpier sampling distribution plots, yet confidence intervals are 'stable'
  
  # ----------------------------------------------------------------------------------------
  # ----------------------------------------------------------------------------------------
  
  # Example 4:
  
  # Can we check if the GPA and GMAT scores from the admission data set are independent?
  #
  # The question really is: are the underlying random variables, GPA and GMAT scores of
  # the student population, independent?
  #
  # Independence of random variables cannot be checked by calculation.
  # Furthermore, we only have a sample of pairs (GPA, GMAT) of size n = 85.
  #
  # But we can check un-correlated-ness!
  # Correlation: "a quantity measuring the extent of interdependence"
  #
  # Independent random variables are uncorrelated (correlation = 0).
  # The converse is not true: There exist dependent random variables whose correlation is zero.
  
  # We test the following:
  # Null-hypothesis: GPA and GMAT scores are uncorrelated!
  
  # If the null-hypothesis is accepted, i.e., the GPA and GMAT scores are uncorrelated,
  # they may or may not be independent.
  # However, if the null hypothesis is rejected in favor of the alternative:
  # GPA and GMAT scores are not uncorrelated, hence not independent!
  
  GPA = adm$GPA
  
  # Scatter plot the GPA scores against the GMAT scores
  plot(GMAT,GPA,pch=19,col="blue",frame.plot=F); grid()
  
  # What is the sample correlation?  
  Scor = cor(GPA,GMAT); Scor
  
  sm1 = mean(GPA);  ssd1 = sd(GPA)
  sm2 = mean(GMAT); ssd2 = sd(GMAT)
  
  # the empirical densities
  par(mfrow=c(2,1))                       # set 2x1 plots
  myed(GMAT,ylim=c(0,0.005),pcol="azure2",dcol="green4",
       xlab="average",main="GMAT data density")
  myed(GPA,pcol="bisque",dcol="coral3",
       xlab="average",main="GMAT data density")
  par(mfrow=c(1,1))                       # set 1x1 plot
  
  # ----------------------------------------------------------------------------------------
  # 1st approach: Assume normality of GPA and GMAT data
  #
  # Find the confidence interval for the correlation of two 'independent' vectors
  # from normal distributions N(sm1,ssd1) and N(sm2,ssd2), respectively, where
  # sm1, ssd1, sm2, and ssd2 are sample variance and sample standard deviation
  # of GPA and GMAT vectors from the admission data set.
  
  # Question: if GPA and GMAT vectors were independent (more precisely, uncorrelated),
  #           how likely it is that their correlation is approx. 0.46?
  # We can answer this if we know the distribution of the 'sample' correlation of
  # two independent normal random vectors of same length as GPA and GMAT vectors
  
  n = length(GPA)
  cor(rnorm(n,sm1,ssd1),rnorm(n,sm2,ssd2))
  
  # The sampling distribution is created by replicating this 100 thousand times
  m = 100000
  sdv4a = replicate(m,cor(rnorm(n,sm1,ssd1),rnorm(n,sm2,ssd2)))
  
  mysdci(sdv4a,ylim=c(0,4),pcol="thistle1",dcol="orchid4",
         main=paste("Pop.Correlation Sampling Distr. from 2 indep Normal samples of size",n),
         sub="assuming GPA / GMAT data mean and variance")
  
  # Where does the correlation of our GPA and GMAT vectors (0.46) fall in?
  abline(v=Scor,lwd=2,col="red")
  
  # Extremly far in the tail.  Check the maximum of the sampling distribution vector
  max(sdv4a)
  
  # Do you accept or reject H0: GPA and GMAT scores are independent?
  
  # Related question: how close is the sampling  distr. to the normal distribution:
  x = seq(-0.5,0.5,0.01)
  lines(x,dnorm(x,0,sd(sdv4a)),lwd=2,col="blue")
  
  # ----------------------------------------------------------------------------------------
  
  # 2nd approach:  Resampling
  
  # Recall: resampling (a full length of vector) without replacement is same as permuting it
  v = seq(6)
  sample(v,6,replace=FALSE)
  
  # Resampling without replacement (permutations of data vectors)
  m = 100000
  sdv4b = rpl(m,cor(sample(GPA,n,F),sample(GMAT,n,F)))
  
  # It is not hard to see that it is enough to permute only one of the vectors 
  sdv4b = rpl(m,cor(GPA,sample(GMAT,n,F)))
  mysdci(sdv4b,ylim=c(0,4),pcol="thistle3",dcol="slateblue",
         main=paste("Pop.Correlation Sampling Distr. via resampling w/o replacement"),
         sub="from permuted GPA and GMAT samples")
  x = seq(-0.5,0.5,0.01)
  lines(x,dnorm(x,0,sd(sdv4b)),lwd=2,col="blue")
  
  # Do you accept or reject H0: GPA and GMAT scores are independent?
  
  # Plot both sample distributions from 4a and 4b
  par(mfrow=c(2,1))                       # set 2x1 plots
  mysdci(sdv4a,ylim=c(0,4),pcol="thistle1",dcol="orchid4",
         main=paste("Pop.Correlation Sampling Distr. from 2 indep Normal samples of size",n),
         sub="assuming GPA / GMAT data mean and variance")
  mysdci(sdv4b,ylim=c(0,4),pcol="thistle3",dcol="slateblue",
         main=paste("Pop.Correlation Sampling Distr. via resampling w/o replacement"),
         sub="from permuted GPA and GMAT samples")
  par(mfrow=c(1,1))                       # set 1x1 plot
  
  
  # Interesting observations:  
  
  # Try resampling WITH replacement - the sampling distr. look pretty much the same
  
  sdv4c = rpl(m,cor(sample(GPA,n,T),sample(GMAT,n,T)))
  mysdci(sdv4c,ylim=c(0,4),pcol="pink2",dcol="pink4",
         main=paste("Pop.Correlation Sampling Distr. via resampling with replacement"),
         sub="from permuted GPA and GMAT samples")
  x = seq(-0.5,0.5,0.01)
  lines(x,dnorm(x,0,sd(sdv4c)),lwd=2,col="blue")
  
  
  # In fact, replace the GPA and GMAT vectors by any two 'independent' generated vectors
  # of length 85. The sampling distribution will look identical
  
  sdv4d = rpl(m,cor(runif(n),runif(n)))
  mysdci(sdv4d,ylim=c(0,4),pcol="wheat",dcol="wheat4",
         main=paste("Pop.Correlation Sampling Distr. via resampling with replacement"),
         sub=paste("from permuted Unif(0,1) samples of size",n))
  x = seq(-0.5,0.5,0.01)
  lines(x,dnorm(x,0,sd(sdv4d)),lwd=2,col="blue")
  
  # All-star lineup  
  par(mfrow=c(2,2))                       # set 2x2 plots
  mysdci(sdv4a,ylim=c(0,4),pcol="thistle1",dcol="orchid4",rnd=4,
         main="Corr with GPA / GMAT mean and var")
  mysdci(sdv4b,ylim=c(0,4),pcol="thistle3",dcol="slateblue",rnd=4,
         main="Corr via resampling w/o replacement from GPA and GMAT")
  mysdci(sdv4c,ylim=c(0,4),pcol="pink2",dcol="pink4",rnd=4,
         main="Corr via resampling with replacement from GPA and GMAT")
  mysdci(sdv4d,ylim=c(0,4),pcol="wheat",dcol="wheat4",rnd=4,
         main="Corr via resampling with replacement from Unif(0,1)")
  par(mfrow=c(1,1))                       # set 1x1 plot
  
  # Hence the distributions of the vectors play no role here - only the fact that
  # the vectors were independently generated (via R random number generator: the 
  # vectors are just uncorrelated) matters in shaping the sampling distribution.
  
  #------------------------------------------------------------------------------Seesion 11
  
  
  # --------------- Simple Linear Regression ---------------
  
  # Use a dataframe in base R package named 'women':
  # contains a small sample of women's heights and weights
  View(women)
  
  # Scatter plot the data
  plot(women,pch=19,cex=1,frame.plot=F,
       col="dodgerblue",xlab="Height",ylab="Weight"); grid()
  
  # More common
  h = women$height
  w = women$weight
  plot(h,w,pch=19,cex=1,frame.plot=F,
       col="blue",xlab="Height",ylab="Weight"); grid()
  
  # Suppose that we want to predict weight using height as a predictor
  # A basic R function for fitting a linear model is
  #    lm(formula = dependentvar ~ predictor(s), data = dataframe).
  
  r1 = lm(formula = weight~height,data = women)
  
  # the same is obtained by:
  r1 = lm(w~h)
  
  r1
  summary(r1)           # detailed results for the fitted model
  names(r1)             # all names (attributes?) of the object
  
  coef(r1)              # model parameters: intercept and slope(s)
  r1$coefficients       # Intercept, Slope
  r1$coefficients[2]    # Slope
  
  residuals(r1)         # vector of residuals
  r1$residuals          # vector of residuals
  
  fitted(r1)            # vector of predicted values ("Y hat")
  r1$fitted.values      # Y hat
  
  # add the lm fit line to the plot
  abline(r1,lwd=1.5,col="red")
  
  # To see the intercept on the y-axis:
  plot(h,w,xlim=c(-10,80),ylim=c(-100,200),axes=F,
       pch=19,cex=1,col="blue",xlab="Height",ylab="Weight"); myaxes(0,0)
  abline(r1,lwd=1.5,col="red")
  
  # The w ~ h scatter-plot indicates a slight quadratic dependence of weight on height
  # Try Multiple Linear Regression with height^2 as term (use I(h^2))
  mr1 = lm(w ~ h + I(h^2))
  summary(mr1)
  # Compare the residuals of mr1 and r1: mr1 residuals are much smaller.
  # Also the adjusted R-squared value is larger
  
  # see the fitted values and relate them to the residuals
  points(women$height,r1$fitted.values,col="darkred",cex=1,pch=19)
  sum(r1$residuals)
  
  # create fuction that returns lm object together with plots
  mylm = function(x,y,ptcol="blue",lncol="red",xlim=NULL,ylim=NULL,fitted=FALSE)
  {
    plot(x,y,frame.plot=F,pch=19,cex=0.75,col=ptcol,xlim=xlim,ylim=ylim); grid()
    r = lm(y~x)
    abline(r,lwd=1.5,col=lncol)
    if (fitted) {points(x,r$fitted.values,pch=19,cex=0.75,col="darkred")}
    print(summary(r))
    cat("Cor:",cor(x,y))
    return(r)
  }
  
  r2 = mylm(women$height,women$weight)
  r2 = mylm(women$height,women$weight,fitted=TRUE)
  
  # Plotting other regression related information
  
  par(mfrow=c(2,2))           # set 2x2 plots ...
  plot(r2,pch=19,col="blue")  # ... and plot r2
  par(mfrow=c(1,1))           # back to single plot
  
  # make it a 'procedure'
  myreg = function(regress)
  {
    par(mfrow=c(2,2))
    plot(regress,pch=19,cex=0.75,col="blue")
    par(mfrow=c(1,1))
  }
  
  myreg(r2)
  
  # Note: The scatter-plot residuals against predictor (height) looks
  #   the same as the scatter=plot residuals againts fitted values.
  #   The only difference is the scale on the x-axis
  
  plot(h,r1$residuals,frame.plot=F,pch=19,col="blue");grid()  
  plot(r1$fitted.values,r1$residuals,frame.plot=F,pch=19,col="green4");grid()  
  
  
  # Digression: To suppress the intercept (set it to zero) use option -1 in 'formula'
  plot(h,w,axes=F,pch=19,cex=1,xlim=c(0,80),ylim=c(0,200),
       col="blue",xlab="Height",ylab="Weight")
  myaxes(0,0)
  
  r0 = lm(formula = weight~height -1,data = women)  # or r0 = lm(w~h-1)
  summary(r0)
  abline(r0,lwd=1.5,col="orange")
  abline(r,lwd=1.5,col="red")
  
  #  -----------------------------------------------------------------------------Session 12
  
  source("myFunctions.R")     # custom functions
  
  
  # --------------- Simple Linear Regression (cont) ---------------
  
  # HeightWeight.csv contains a sample of heights and weights
  hw = read.csv("Data/HeightWeight.csv")
  
  # Scatter plot the data
  plot(hw,pch=19,cex=1,frame.plot=F,
       col="dodgerblue",xlab="Height",ylab="Weight"); grid()
  
  h = hw$Height; w = hw$Weight
  r = mylm(h,w)
  myreg(r)
  snQQplot(trv(r$residuals)) 
  
  # What is the estimate of the regression variance?
  # Does weight depend linearly on height at 5% significance?
  
  
  # Simulations that generate samples (Xi,Yi) from population (X,Y)
  # Using R we can construct various scenarios of Y dependence on X
  
  # Scenario 1: Textbook case simple linear regression
  x = runif(500,0,10)
  y = 250 + 1*x + rnorm(500,0,10)    # Beta0 = 250, Beta1 = 1, error ~ N(0,sd=10)
  r = mylm(x,y)
  # residual plots:
  myreg(r)
  
  # Scenario 2: Heteroscedasticity
  x = runif(500,0,10)
  y = 100 + 2*x + x*rnorm(500,0,1) # Beta0 = 250, Beta1 = 1, error ~ x*N(0,1)
  r = mylm(x,y)
  # residual plots:
  myreg(r)
  
  # Scenario 3: Nonlinearity 1st case: quadratic dependence
  x = runif(500,0,10)
  y = 100 + 2*x + 3*x^2 + rnorm(500,0,5)
  r = mylm(x,y)
  # residual plots:
  myreg(r)
  
  # Scenario 4: Nonlinearity 2nd case
  x = runif(500,0,10)
  y = ifelse(x<7,100+2*x+rnorm(500),100+5*x+rnorm(500))
  r = mylm(x,y)
  # residual plots:
  myreg(r)
  
  # Scenario 5: Ugly outliers in predictor x
  x = runif(500,0,10)
  y = 250 + x + rnorm(500,0,10)
  #  r = mylm(x,y)
  x[500] = 55
  r = mylm(x,y)
  
  # exclude the outlier (x[500],y[500]):
  x1 = x[-500]; y1 = y[-500]
  r1 = lm(y1 ~ x1); summary(r1)
  abline(r1,lwd=2,col="green2")
  
  # Handling robust linear regression with "rlm" function from MASS package
  library(MASS)
  r2 = rlm(y~x); summary(r2)
  abline(r2,lwd=3,col="orange",lty=3)
  # Check the "Summaries" on the console output: 
  # rlm output is not the same as for exclusion of the outlier
  
  # residual plots for Scenario 5
  myreg(r)
  
  
  # Scenario 6: Ugly outliers in dependent var y
  x = runif(500,0,10)
  y = 250 + x + rnorm(500,0,10)
  y[500] = 375
  r = mylm(x,y)
  
  r2 = rlm(y~x); summary(r2)
  abline(r2,lwd=2,col="orange")
  # residual plots:
  myreg(r)
  
  # ----------------------------------------------------------------------------------------
  
  # Simple Linear Regression: Slope Variance
  #   Slide 3.29 of the class lectures states that b1 is
  #   a normal random variable with mean beta1 and variance sigm^2/SSx
  #   Can we verify this with simulation in R?
  
  # Create a function with inputs:
  #    vector x,
  #    regression slope (beta1),
  #    regression intercept (beta0),
  #    square root of regression variance (sigm)
  # and that returns the coefficients b0 and b1, assuming linear model
  #    Y = beta0 + beta1*x + N(0,sigm^2)
  
  f = function(x,beta0,beta1,sigm)
  {
    n = length(x)
    y = beta0 + beta1*x + rnorm(n,0,sigm)
    r = lm(y~x)
    return(r$coefficients)
  }
  
  # Try it:
  beta0 = 3; beta1 = 7; sigm = 2
  
  x = runif(100,0,4)
  f(x,beta0,beta1,sigm)
  # we need averages of the rows (R function rowMeans())
  
  # What do we get if we replicate this function 
  sdv = replicate(7,f(x,beta0,beta1,sigm)); sdv
  
  # To verify that the variance of b1 is sigm^2/SSx, we need to use the fixed x vector
  # and generate varius random y values (where randomness is generate from N(0,sigm^2))   
  n = 100
  x = runif(n,0,4)
  sm = mean(x)
  ssx = (n-1)*var(x)
  
  # As above, create two sampling distributions and check the rowMeans
  sdv = replicate(20000,f(x,beta0,beta1,sigm))
  rowMeans(sdv)
  
  # Extract the slope sampling distribution and its empirical density
  slopes = sdv[2,]
  myed(slopes,main="Regression slope sampling distribution")
  mysdci(slopes,main="Regression slope sampling distribution")
  
  # Finally, compare it to the theoretical distribution N(beta1,sigm^2/SSx)
  t = seq(6,8,0.001)
  lines(t,dnorm(t,beta1,sigm/sqrt(ssx)),lwd=2,col="blue")
  
  
  # While we're at it: the intercept sampling distribution
  intcpts = sdv[1,]
  mysdci(intcpts,pcol="yellowgreen",
         main="Regression intercept sampling distribution")
  
  # Compare it to the theoretical distribution N(beta0,(mean(x)*sigm)^2/SSx)
  t = seq(1.5,4.5,0.001)
  lines(t,dnorm(t,beta0,sqrt(1/n+sm^2/ssx)*sigm),lwd=2,col="blue")
  
  
  # ----------------------------------------------------------------------------------------
  
  # --------------- Multiple Linear Regression ---------------
  
  # Intro Example: Boston dataset
  
  library(MASS)
  View(Boston)
  plot(Boston,cex=0.25,pch=19,col="blue")
  
  # pairwise correlations:
  options(digits=2)      # suppressing too many decimals
  cor(Boston)
  
  library(lattice)
  levelplot(cor(Boston))
  
  mr = lm(medv~.,data=Boston); summary(mr)
  
  
  # Example 1
  
  n = 1000
  x1 = runif(n,0,2)
  x2 = runif(n,0,2)
  y = 1 + x1 + 4*x2 + rnorm(n)
  
  # Multiple Linear Regression: use the same 'lm' function as in Simple Regression
  mr = lm(y ~ x1 + x2)
  summary(mr)
  
  # Residuals and fitted values can be compared as in case of simple regression
  plot(mr$fitted.values,mr$residuals,pch=19,col="blue",frame.plot=F,axes=F)
  myaxes(0,0)
  
  myreg(mr)
  
  # Is there an extension to mylm() function?    
  # It is a challenge to present "three-dimensional" data.
  # One possibility is to plot two-dimensional scatter-plots:
  #    y against x1,  
  #    y against x2, 
  #    x2 against x1.
  
  plot(x1,y,pch=19,col="blue",frame.plot=F,axes=F); myaxes(0,0)
  plot(x2,y,pch=19,col="blue",frame.plot=F,axes=F); myaxes(0,0)
  plot(x1,x2,pch=19,col="blue",frame.plot=F,axes=F); myaxes(0,0)
  
  # 'plot' function can be used if we 'pack' (x1,x2,y) in a data-frame.  
  
  d = cbind(x1,x2,y)
  class(d)                            # d is a matrix
  View(d)
  plot(d,pch=19,cex=0.5,col="blue")   # and plot is incomplete
  
  d = as.data.frame(cbind(x1,x2,y))
  class(d)
  View(d)
  plot(d,pch=19,cex=0.5,col="blue")
  
  
  # Digression: 3D plotting in R (OPTIONAL)
  
  # It would be nice to see a 3D plot of this data
  library(plotly)   # plotly package (large non-standard package, must be installed)
  
  # Custom subroutine for plot_ly:
  #        x1: x-axis, x2: y-axis, y: z-axis,
  #        cols: sequence of colors
  #        colscale: for coloring 3D points in different colors (scales)
  
  scat3d = function(x1,x2,y,cols=c("blue2"),colscale=cols,cex=2)
  {
    x1n = deparse(substitute(x1))   # extracts the x1 variable name
    x2n = deparse(substitute(x2))
    yn = deparse(substitute(y))
    plot_ly()%>%
      add_trace(x=x1,y=x2,z=y,type="scatter3d",mode="markers",colors=cols,
                color=colscale,
                marker=list(size=cex))%>%
      layout(scene=list(xaxis=list(title=x1n),
                        yaxis=list(title=x2n),
                        zaxis=list(title=yn)))%>%
      hide_colorbar()%>% hide_legend()
  }
  
  # 3D plot of a (x1,x2,y)
  scat3d(x1,x2,y)
  
  # try the colorscale applied on residuals:
  scat3d(x1,x2,y,c("orange2","blue2"),mr$residuals)
  
  # A nicer view:
  #   color orange the points whose residuals are negative (below the regression plane)
  #   and in blue whose residuals are positive (above the regression plane)
  
  scat3d(x1,x2,y,c("orange2","blue2"),1*(mr$residuals>=0))
  
  # A nicer^2 view: add the regression plane to the 3D plot
  
  # Auxiliary function producing 3D surface on given grid
  surface = function(xs,ys,coeff)
  {
    g = expand.grid(xs,ys)
    aux = coeff[1] + coeff[2]*g[1] + coeff[3]*g[2]
    if (length(coeff)>3) aux = aux + coeff[4]*g[1]*g[2]
    return(matrix(aux[,1],nrow=length(xs),ncol=length(ys),byrow=T))
  }
  
  # Appended scat3d(): adds the regression plane
  regr3d = function(x1,x2,y,mr,
                    ptcols=c("orange2","blue2"),cex=2,ptshow=T,
                    sfccol="gray75",op=.8,sfcshow=T)
  {
    x1n = deparse(substitute(x1))   # extracts the x1 variable name
    x2n = deparse(substitute(x2))
    yn = deparse(substitute(y))
    x1r = range(x1); x2r = range(x2); sfc = surface(x1r,x2r,mr$coefficients)
    plot_ly(colors=ptcols)%>%
      add_surface(x=x1r,y=x2r,z=sfc,type="surface",
                  colorscale=list(list(0,sfccol),list(1,sfccol)),
                  surfacecolor=c(0,1),
                  opacity=op,visible=sfcshow)%>%
      add_trace(x=x1,y=x2,z=y,type="scatter3d",mode="markers",
                color=1*(mr$residuals>=0),
                marker=list(size=cex),visible=ptshow)%>%
      layout(scene=list(xaxis=list(title=x1n),
                        yaxis=list(title=x2n),
                        zaxis=list(title=yn)))%>%
      hide_colorbar()%>% hide_legend()
  }
  
  regr3d(x1,x2,y,mr)
  
  
  # Example 2: Square term in x2 variable
  
  n = 1000
  x1 = runif(n,-2,2)
  x2 = runif(n,-2,2)
  y = 1 + x1 + 4*x2^2 + rnorm(n)   # Note: Square of x2
  
  d = as.data.frame(cbind(x1,x2,y))
  plot(d,pch=19,cex=0.5,col="blue")
  
  mr = lm(y~x1+x2); summary(mr)
  
  regr3d(x1,x2,y,mr)     # optional
  
  # Replace x2 by its square:
  x2sq = x2^2
  d = as.data.frame(cbind(x1,x2sq,y))
  plot(d,pch=19,cex=0.5,col="blue")
  
  mr = lm(y~x1+x2sq); summary(mr)
  
  regr3d(x1,x2sq,y,mr)     # optional
  
  # Alternatively the square term can be handled with I() symbol
  mr = lm(y~x1+I(x2^2)); summary(mr)
  
  # Experimenting with lm operators
  mr = lm(y~x1+x2^2); summary(mr)     # same as lm(y~x1+x2)
  
  mr = lm(y~(x1+x2)^2); summary(mr)   # includes interaction between x1 and x2
  mr = lm(y~x1*x2); summary(mr)       # same (see example 3 below)
  
  
  # Example 3: Interaction terms
  
  n = 10000
  x1 = runif(n,0,30)
  x2 = runif(n,0,30)
  
  y = 1 + x1 + 4*x2 + 2*x1*x2    # no error term
  scat3d(x1,x2,y,c("blue2"))
  
  y = 1 + x1 + 4*x2 + 2*x1*x2 + rnorm(n,0,50)
  
  d = as.data.frame(cbind(x1,x2,y))
  plot(d,pch=19,cex=0.5,col="blue")
  
  mr = lm(y~x1+x2); summary(mr)
  regr3d(x1,x2,y,mr)     # optional
  
  
  # Multilinear Regression WITH INTERACTIONS included
  #   (use "x1 * x2" instead of "x1 + x2")
  mri = lm(y~x1*x2); summary(mri)
  
  # regression "plane" in 3D plot is a surface:
  regr3d(x1,x2,y,mri)           # optional
  regr3d(x1,x2,y,mri,ptshow=F)  # regression "plane" only
  
  
  # Example 4: Three variables from Boston dataset
  
  # Extract three vectors from 'Boston'
  #   rm     average number of rooms per dwelling
  #   lstat  lower status of the population (percent)
  #   medv   median value of owner-occupied homes in $1000s
  
  rm = Boston$rm
  lstat = Boston$lstat
  medv = Boston$medv
  
  d = as.data.frame(cbind(rm,lstat,medv))
  plot(d,pch=19,cex=0.5,col="blue")
  scat3d(rm,lstat,medv,"blue2",cex=3) # optional
  
  # Try multiple regression with these three variables
  mr2 = lm(medv~rm+lstat); summary(mr2)
  regr3d(rm,lstat,medv,mr2,cex=3)     # optional
  
