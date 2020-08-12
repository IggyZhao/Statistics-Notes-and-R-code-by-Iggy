
# OPIM 5603 RStudio Session 9
# Wednesday, October 30, 2019

ttztest = function(v,mu0,sigm,alpha)
{
  sm = mean(v)                                   # Sample Mean
  n = length(v)                                  # sample size
  Z = (sm-mu0)/(sigm/sqrt(n))            # T statistic value
  pval = pnrom(-abs(Z),n-1) + 1-pt(abs(Z),n-1)     # p-value
  if (pval>alpha)
  { return(paste("p-value equals",round(pval,5),">",alpha,
                 "; accept Null-Hypothesis: population mean equals",mu0)) }
  else
  { return(paste("p-value equals",round(pval,5),"<=",alpha,
                 "; reject Null-Hypothesis in favor of the Alternative:",
                 "population mean is not equal",mu0)) }
}


# a convenient way to store custom functions to memory
  setwd("C:/Users/Iggy Zhao/Desktop")
  source("myFunctions.R")

# ----------------------------------------------------------------------

# Classical Two-Sample t-test  

# Example: Drinking water pollutants
# Trace metals in drinking water affect the flavor and an unusually high
# concentration can pose a health hazard. 40 pairs of data were taken
# measuring pollutant concentration in bottom water and surface water.

  p = read.csv("Pollutant.csv"); View(p)

# Question: Is this data paired?

# View the basic properties of data columns
  summary(p)

# Check the scatter of the pairs
  plot(p$bottom,p$surface,pch=19,col="blue2"); grid()

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

# Compute the observed test statistic for the difference sample
  n = length(delta)
  tstat = (mean(delta)-mu0)/(sd(delta)/sqrt(n)); tstat

# Compute the p value:
  pval = pt(-abs(tstat),df=n-1) + 1 - pt(abs(tstat),df=n-1); pval
  # reject the null -iggy

# compare to built-in t-test:
  
  # One-sample t test for differences:
  t.test(delta, mu=0, conf.level=0.95)

  # Two-sample paired t test:
  t.test(p$bottom, p$surface, paired=TRUE, conf.level=0.95)


# ----------------------------------------------------------------------

# Example: Student GPA
#
# Random samples of GPA's of juniors and seniors at some university.

  gpa = read.csv("GPAs.csv"); View(gpa)

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
  dj = density(j)
  plot(dj, xlab="GPA", main="Juniors"); grid()
  polygon(dj,col="lightblue3")

  ds = density(s)
  plot(ds, xlab="GPA", main="Seniors"); grid()
  polygon(ds,col="orange2")

# Question: Pooled or separate variance?
  sd(j)
  sd(s)
  sd(j)/sd(s)

# built-in t-test:
  
  # Pooled variance:
  t.test(j, s, paired=FALSE, var.equal=TRUE, conf.level=0.95)
  t.test(s, j, paired=FALSE, var.equal=TRUE, conf.level=0.95)
  # Separate variance (Welch two sample t-test)
  t.test(j, s, paired=FALSE, var.equal=FALSE, conf.level=0.95)


# ----------------------------------------------------------------------

# Quantile Plots: Help in consideration of
#                "Does the data set come from a given distribution?"
   
  n = 10
  y = seq(0.5/n,1,1/n)
  q = qnorm(y)
#  options(digits=3)
  rn = rnorm(n)
  plot(q,sort(rn),pch=19,cex=0.75,col="blue",
       xlim=c(-4,4),ylim=c(-4,4),main="Standard Normal Quantile Plot",axes=F)
  myaxes(0,0)
  abline(a=0,b=1,lwd=2,col="orange")
  
# Built-in R function
  qqnorm(rn,pch=19,col="blue",xlim=c(-4,4),ylim=c(-4,4),axes=F)
  myaxes(0,0)
  abline(a=0,b=1,lwd=2,col="orange")

# Try it for various samples of length 1000
  
  qqnorm(rnorm(100),pch=19,col="blue",xlim=c(-4,4),ylim=c(-4,4),axes=F)
  myaxes(0,0)
  abline(a=0,b=1,lwd=2,col="orange")
# Standard Normal Quantile Plot
 
  
  qqnorm(rnorm(1000),pch=19,col="blue",xlim=c(-4,4),ylim=c(-4,4),axes=F)
  myaxes(0,0)
  abline(a=0,b=1,lwd=2,col="orange")
  
  qqnorm(rexp(1000),pch=19,col="blue",xlim=c(-4,4),ylim=c(-4,4),axes=F)
  myaxes(0,0)
  abline(a=0,b=1,lwd=2,col="orange")

  qqnorm(runif(1000),pch=19,col="blue",xlim=c(-4,4),ylim=c(-4,4),axes=F)
  myaxes(0,0)
  abline(a=0,b=1,lwd=2,col="orange")
  
  qqnorm(rnorm(1000,-4,12),pch=19,col="blue",axes=F)
  myaxes(0,0)
  abline(a=0,b=1,lwd=2,col="orange")
  
# Custom Standard Normal Quantile Plot function

  # About input parameters:
  # If the population mean and standard deviation are [known],
  # they can be entered as optional parameters.
  # Otherwise the sample mean and sample standard deviation are used.
  
  stnormQQplot = function(v,mu=mean(v),sigm=sd(v),col="blue2",main=NULL)
  {
    n = length(v)  # sample size
      # Now: Transform v to vector of sample mean (close to) 0
      #      and sample variance (close to) 1
    Transf_v = (v-mu)/sigm
    plot(qnorm(seq(0.5/n,1,1/n)),sort(Transf_v),
         xlim=c(-4,4),ylim=c(-4,4),
         pch=19,cex=0.5,col=col,
         col.main=col,font.main=1,cex.main=1.25,axes=F,
         xlab="Theoretical quantiles",ylab="Empirical quantiles",
         main=paste("Quantile Plot:",n,main,"points"))
    myaxes(0,0,lwd=1.5)
    abline(a=0,b=1,lwd=1/5,col="orange")
  }

  rn = rnorm(1000,-4,12)
  stnormQQplot(rn,main="N(-4,12)"); grid()
  stnormQQplot(rn,mu=-4,sigm=12,main="N(-4,12)"); grid()
  
  stnormQQplot(rnorm(85),main="N(0,1)")
  stnormQQplot(runif(85),main="Unif(0,1)",col="green3")
  stnormQQplot(rexp(85),main="Exp(1)",col="red2")
  
# known population parameters: generate normal random vector
#    and check the sample mean and sample standard deviation

  v = rnorm(100,-5,13); mean(v); sd(v)

# see the difference in quantile plots
  stnormQQplot(v)                 # uses sample mean and sample sd
  stnormQQplot(v,mu=-5,sigm=13)   # uses known pop.mean and pop.sd
  
# Note: generate rn from another distribution,
#    say exp, unif, and t(5), and compare the plots
  
  par(mfrow=c(2,2))          # set 2x2 panel for 4 plots
  
  n = 10000
  stnormQQplot(rnorm(n),main="N(0,1)")
  stnormQQplot(runif(n),main="Unif(0,1)",col="green3")
  stnormQQplot(rexp(n),main="Exp(1)",col="red2")
  stnormQQplot(rt(n,5),main="t(5)",col="purple")
  # re-run these lines for larger n's
  
  par(mfrow=c(1,1))          # reset panel back to single plot


# ----------------------------------------------------------------------

# Hypothesis Testing / Confidence Intervals via R simulations

# ----------------------------------------------------------------------

# First approach:
# Make (a justified) assumption about population distribution

# ----------------------------------------------------------------------

# Example 1:  (example done in Session 8)
# Test the null hypothesis that the population mean of GMAT data is 510.

  adm <- read.csv("Admission.csv"); head(adm)

# Densities of the GPA and GMAT vectors
  myed(adm$GPA,pcol="lightyellow",dcol="red2",xlab="average",main="GPA data density")
  myed(adm$GMAT,pcol="khaki",dcol="green4",xlab="average",main="GMAT data density")

  summary(adm$GMAT)
  x = seq(310,700,0.1)
  lines(x,dnorm(x,mean=mean(adm$GMAT),sd=sd(adm$GMAT)),lwd=2,col="blue")

  # increase the y-axis limit
  myed(adm$GMAT,ylim=c(0,0.005),pcol="khaki",dcol="green4",
       xlab="average",main="GMAT data density")
  lines(x,dnorm(x,mean=mean(adm$GMAT),sd=sd(adm$GMAT)),lwd=2,col="blue")
  
# without custom functions:
  # d_GPA = density(adm$GPA)
  # plot(d_GPA, lwd=3, col="red2",
  #      xlab="average", main="GPA data density"); grid()
  # polygon(d_GPA,col="lightyellow")
  # 
  # d_GMAT = density(adm$GMAT)
  # plot(d_GMAT, lwd=3, col="green4",
  #      xlab="points", main="GMAT data density"); grid()
  # polygon(d_GMAT,col="khaki")
  # 
  # x = seq(min(d_GMAT$x),max(d_GMAT$x),(max(d_GMAT$x)-min(d_GMAT$x))/1000)
  # lines(x,dnorm(x,mean=mean(adm$GMAT),sd=sd(adm$GMAT)),lwd=2,col="blue")

# Null-hypothesis H0: GMAT population mean is 510
  mu0 = 510

# Compute the sample mean and sample sd (sqrt of sample variance)
  sm = mean(adm$GMAT); sm
  ssd = sd(adm$GMAT); ssd
  abline(v=sm)
# Is this normally distributed data  
  stnormQQplot(adm$GMAT)
  stnormQQplot(rnorm(85,sm,ssd))
  
  
  
  
  
  
  
  
  
  
  
  
  
  

# KEY: Based on observed GMAT density and the quantile plot WE ASSUME
#      the population distribution is normal,
# The best (unbiased) estimators for population mean and variance are
# sample mean and sample variance, respectively.  
# Thus we assume population distribution is N(sm,ssd) = N(488.4,81.5)

# The sample size is n = 85 (there are 85 rows in the dataframe).
  n = length(adm$GMAT)

# To create one sample
  rnorm(n,sm,ssd)

# To compute the metric (mean) for one sample
  mean(rnorm(n,sm,ssd))

# Finally, replicate this 10K times to get a sampling distribution vector
# for the Sample Mean
  m = 100000
  sdv = replicate(m,mean(rnorm(n,sm,ssd)))

# Plot its density
  myed(sdv,main="Sample Mean sampling distr")
  myed(sdv,ylim=c(0,0.05),main="Sample Mean sampling distr")
  
# without custom functions:
  # d_sdv = density(sdv)
  # plot(d_sdv,lwd=3,col="green2",
  #      main = "Sample Mean sampling distr"); grid()
  # polygon(d_sdv,col="lightgreen")

# Note: in this case we know the true distribution of the sampling
#       distribution: N(488.4, 81.5/sqrt(85))
# Question: what is the "half-mass" point (median)?
  abline(v=median(sdv),lwd=3,col="magenta",lty=3)
# Where does the sm fit in?
  abline(v=sm,lwd=2,col="blue")

# How far from sm is the mu0?
  abline(v=mu0,lwd=1.5,col="red",lty=3)
  abline(v=sm+sm-mu0,lwd=1.5,col="red",lty=3)

# Empirical p-value (see the plot)
  left_p = length(sdv[sdv<sm+sm-mu0])
  right_p = length(sdv[sdv>mu0])
  p = (left_p+right_p)/length(sdv); p
  
# In order to avoid keeping track of which of the two values,
# mu0 and sm+sm-mu0 is larger/smaller,
  lowbd = min(mu0,sm+sm-mu0)    # lower bound
  uppbd = max(mu0,sm+sm-mu0)    # upper bound

# Compute the probabilities 'beyond' these lines to obtain
# the p-values for the hypothesis
  left_p = length(sdv[sdv<lowbd])
  right_p = length(sdv[sdv>uppbd])
  p = (left_p+right_p)/length(sdv); p

# We reject the hypothesis at significance level 0.05.

# again: compare to built-in t-test:
  t.test(adm$GMAT,mu=mu0,conf.level = 0.95)

# Related question: What is the empirical conf.interval for 95% confidence?
  myeCI(sdv,0.95)
  abline(v=myeCI(sdv,0.95),lwd=3,lty=2,col="orange2")  
  
# without custom functions:
  left_ep = quantile(sdv,0.025)
  right_ep = quantile(sdv,0.975)
  CI = c(left_ep,right_ep); CI
  abline(v=CI,lwd=1.5,col="purple")

# As in the classical t-test, we shall AVOID the computation of p-values
# and use empirical confidence interval instead
