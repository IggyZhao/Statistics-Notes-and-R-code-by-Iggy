#
#  OPIM 5603 RStudio Session 2
#  Wednesday, September 4, 2019
#

# Reload the credit card data from Session 1:
  ccdata = read.csv("Data01/CCData.csv")

# Variables used last time
  x = ccdata$Cards; x
  freq = ccdata$Freq; freq
  prob = freq/sum(freq); prob

# -------------- Variance of a discrete r.v. --------------

# Definition of variance contains expected value mu:
  mu = weighted.mean(x,prob); mu

# Understanding R syntax: what are the results of:
  x
  x-mu
  (x-mu)^2
  ((x-mu)^2)*prob
  sum(((x-mu)^2)*prob)

# What about:
  weighted.mean(((x-mu)^2),prob)
# Notice: This follows from the fact that Var(X) = E((X-mu)^2),

# As discussed before, the same is obtained if prob is replaced by freq
  weighted.mean(((x-mu)^2),freq)

# Function to calculate variance using values and corresponding probabilities as arguments
  pdvar = function(x,pr)
  {
    mu = weighted.mean(x,pr)
    return(weighted.mean((x-mu)^2,pr))
  }

# Using the function to compute the variance of C:
  pdvar(x,prob)

# As noted above, the same result is obtained if prob vector is replaced by the freq vector
  pdvar(x,freq)

# Digression: simple function example:
  mysum = function(a,b)
  {
    return(a+b)
  }

  mysum(2,3)
  mysum(c(2,6,8),c(1,2,2))
  mysum(c(2,6,8),c(1,2,2,4))
  mysum(c(2,6,8),24)

# Applying the function 'pdvar' to other datasets

  sc = read.csv("Data01/ShinyChevy.csv")
  View(sc)
  sum(sc$Freq)                        # sum of frequencies is 230 (# days)
  sum(sc$x*sc$Freq)                   # 'sumproduct' is 1840 (#cars sold)
  weighted.mean(sc$x,sc$Freq)         # expected value of the associated random variable
  pdvar(sc$x,sc$Freq/sum(sc$Freq))    # variance of the associated random variable
  pdvar(sc$x,sc$Freq)                 # variance of the associated random variable (as above)
  sqrt(pdvar(sc$x,sc$Freq))           # standard deviation of the associated random variable
  
  cr = read.csv("Data01/ChevyRules.csv"); View(cr)
  sum(cr$Freq)
  sum(cr$x*cr$Freq)
  weighted.mean(cr$x,cr$Freq)
  pdvar(cr$x,cr$Freq)
  sqrt(pdvar(cr$x,cr$Freq))


# -------------- Binomial distribution B(n,p) in R --------------
  
# Probability of exactly 6 successes in n=8 Binomial trials with success prob p=0.82:
# dbinom is the probability density of a binomial random variable  
  dbinom(6,8,0.82)
  
# Probability of AT LEAST 6 successes in n=8 Binomial trials with success prob p=0.82:
  dbinom(6,8,0.82) + dbinom(7,8,0.82) + dbinom(8,8,0.82)
  
# Let's get all the probabilities for the binomial random variable B(8,0.82):
  k = seq(0,8)                # values B = B(8,0.82) assumes are 0,1,...,7,8
  prob = dbinom(k,8,0.82)     # vector of probabilities P(B=0), P(B=1),..., P(B=7), P(B=8)
  sum(prob)
  
# Can we see it as a probability distribution table?
  pdtable = data.frame(k,prob)
  View(pdtable)
  
# Column headers should be 'k' and 'P(B=k)'
  colnames(pdtable) = c("k","P(B=k)")
  View(pdtable)
  
# Nice way to visualize: histogram with heights equal to the probabilities
  plot(k,prob)
  plot(k,prob,type="h")
  plot(k,prob,col="orange",type="h",lwd=20)
  barplot(prob,names.arg=k, col="orange"); grid()
  
# Probability of exactly 650 successes in n=800 Binomial trials with success prob p=0.82:
  dbinom(650,800,0.82)
  
# Probability of AT LEAST 650 successes in n=800 Binomial trials with success prob p=0.82:
  dbinom(seq(650,800),800,0.82)
  sum(dbinom(seq(650,800),800,0.82))
  
  # All probabilities of B(800,0.82):
  k = seq(0,800)             # values B = B(800,0.82) assumes are 0,1,...,799,800
  prob = dbinom(k,800,0.82)  # vector of probabilities P(B=0), P(B=1),..., P(B=799), P(B=800)
  sum(prob)
  
# Probability distribution table in this case is rather long (801 row)
  pdtable = data.frame(k,prob)
  colnames(pdtable) = c("k","P(B=k)")
  options(digits=16)
  View(pdtable)
  
# Try
  barplot(prob,names.arg=k, col="orange"); grid()
  
# Why? Try with plot  
  plot(k,prob,col="orange",type="h"); grid()
  plot(k,prob,col="orange",type="h",xlim=c(610,700)); grid()
  plot(k,prob,col="orange",type="h",xlim=c(610,700),lwd=4); grid()
  
# We should depict P(B=k) only for k = 610,..,700
  k1 = seq(610,702)
  prob1 = dbinom(k1,800,0.82)
  barplot(prob1,names.arg=k1, col="orange", ylim=c(0,0.04)); grid()
  barplot(prob1,names.arg=k1, col="orange", ylim=c(0,0.04), border=NA); grid()
  
# Question: what if we vary the value of p? 
  k = seq(0,800)
  plot(k,dbinom(k,800,0.82),col="orange",type="h",ylim=c(0,0.04)); grid()
  plot(k,dbinom(k,800,0.50),col="green3",type="h",ylim=c(0,0.04)); grid()
  plot(k,dbinom(k,800,0.25),col="blue",type="h",ylim=c(0,0.04)); grid()

# Question: what if we increase n? 
  k1 = seq(610,702)
  barplot(dbinom(k1,800,0.82),names.arg=k1, col="orange", ylim=c(0,0.04), border=NA); grid()

  k2 = seq(6400,6720)
  barplot(dbinom(k2,8000,0.82),names.arg=k2, col="orange", ylim=c(0,0.04), border=NA); grid()
  barplot(dbinom(k2,8000,0.82),names.arg=k2, col="orange", ylim=c(0,0.013), border=NA); grid()

  # See Session02+ for further observations

  
# -------------- Normal distribution in R: dnorm, pnorm, qnorm functions --------------
  
# Lecture: selected Standard Normal pdf and CDF values:
  dnorm(0,mean=0,sd=1)   # bell-curve 'peak'
  dnorm(0)               # same: default values for mean and sd are 0 and 1, respectively
  pnorm(0,mean=0,sd=1)   # CDF at zero: must be 1/2 because of the symmetry
  pnorm(0)               # same
  pnorm(-1)
  pnorm(1)
  pnorm(1)-pnorm(-1)     # bell-curve mass within one standard deviation
  
# Plot the standard normal pdf:
  x = seq(-4,4,0.01)
  v = dnorm(x)
  plot(x,v)
  plot(x,v,cex=0.5,col="blue")
  plot(x,dnorm(x),pch=19,cex=0.5,col="blue2"); grid()
  plot(x,dnorm(x),type="l",lwd=2,col="blue3"); grid()
  
# Standard Normal CDF:
  plot(x,pnorm(x),type="l",lwd=2,col="red"); grid()
  
# Combine the graphs
  plot(x,dnorm(x),type="l",lwd=2,col="blue",ylim=c(0,1))
  lines(x,pnorm(x),lwd=2,col="red"); grid()
  
# Slightly more aesthetic
  plot(x,dnorm(x),type="l",lwd=2,col="blue",ylim=c(0,1),axes=FALSE,xlab="",ylab="")
  lines(x,pnorm(x),lwd=2,col="red")
  axis(1,pos=0,lwd=1,at=seq(-4,4,0.5),col="gray75")
  axis(2,pos=0,lwd=1,at=seq(0,1,0.1),col="gray75")
  abline(v=(seq(-4,4,0.5)), col="gray75", lty="dotted")
  abline(h=(seq(0,1,0.1)), col="gray75", lty="dotted")
  
# Quantile function
  z = seq(0,1,0.001)
  plot(z,qnorm(z),pch=19,cex=0.5,col="green3",axes=FALSE); grid()
  axis(1,pos=0,col="gray75")
  axis(2,pos=0,col="gray75")
  
# 20 random numbers from the Standard Normal distribution
  rnorm(20,mean=0,sd=1)
  rnorm(20)              # as above, the default values for mean and sd can be omitted
  
# Lecture examples
# (a)
  pnorm(0.85)
# (b)
  1-pnorm(1.32)
  pnorm(-1.32)
  pnorm(1.32,lower.tail=FALSE)
# (c)
  pnorm(1.78)-pnorm(-2.1)

# School rash example: compute  P(X<3) where X ~ N(6,1.5^2)
  pnorm(3,mean=6,sd=1.5)
# transformation to standard normal Z = (X-mu)/sigma
  pnorm(-2)
