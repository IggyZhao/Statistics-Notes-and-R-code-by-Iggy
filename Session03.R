#
#  OPIM 5603 RStudio Session 3
#  Wednesday, September 11, 2019
#

# -------------- Normal distribution in R: dnorm, pnorm, qnorm functions --------------
  
# Lecture: selected Standard Normal pdf and CDF values:
  dnorm(0,mean=0,sd=1)   # bell-curve 'peak'
  dnorm(0,0,1)           # same: arguments mean and sd are written in proper order
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
  plot(x,dnorm(x),type="l",lwd=3,col="blue"); grid()
  
# Standard Normal CDF:
  plot(x,pnorm(x),type="l",lwd=3,col="red"); grid()
  
# Combine the graphs
  plot(x,dnorm(x),type="l",lwd=3,col="blue",ylim=c(0,1))
  lines(x,pnorm(x),lwd=3,col="red"); grid()

# Slightly more aesthetic
  plot(x,dnorm(x),type="l",lwd=3,col="blue",ylim=c(0,1),axes=FALSE,xlab="",ylab="")
  lines(x,pnorm(x),lwd=3,col="red")
  axis(1,pos=0,lwd=1,at=seq(-4,4,0.5),col="gray75")
  axis(2,pos=0,lwd=1,at=seq(0,1,0.1),col="gray75",las=1)
  abline(v=seq(-4,4,0.5), col="gray75", lty="dotted")
  abline(h=seq(0,1,0.1), col="gray75", lty="dotted")
  
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


# 'Six-sigma' digression: what is the probability that Standard Normal variable 
#  attains a value outside [mu-sigma, mu+sigma] = [-1,1] interval?
  plot(x,dnorm(x),type="l",lwd=3,col="blue",ylim=c(0,0.4),axes=FALSE,xlab="",ylab="")
  axis(1,pos=0,lwd=1,at=seq(-4,4,0.5),col="gray75")
  axis(2,pos=0,lwd=1,at=seq(0,1,0.1),col="gray75",las=1)
  abline(v=seq(-4,4,0.5), col="gray75", lty="dotted")
  abline(h=seq(0,1,0.1), col="gray75", lty="dotted")
  
  # coloring the region above [-1,1] and below the graph 
  polygon(seq(-1,1,0.001),c(0,dnorm(seq(-0.999,0.999,0.001)),0),col=rgb(0,1,0,0.2))
  # and its area is:
  pnorm(1)-pnorm(-1)
  
# the probability that Standard Normal variable attains a value outside [-1,1]:  
  1-(pnorm(1)-pnorm(-1))    # One sigma: roughly 32%

# Alternative calculation, using the symmetry of N(0,1)
  2*pnorm(-1)  

  1-(pnorm(2)-pnorm(-2))    #   Two sigma: roughly 4.5%
  1-(pnorm(3)-pnorm(-3))    # Three sigma: roughly 3 in 1000
  1-(pnorm(4)-pnorm(-4))    #  Four sigma: roughly 6 in 100K
  1-(pnorm(5)-pnorm(-5))    #  Five sigma: roughly 6 in 10M
  1-(pnorm(6)-pnorm(-6))    #   Six sigma: roughly 2 in a billion
  
  
# -------------- Back to Binomial distribution B(n,p) in R --------------

# Recall example: find the probabilty of "at least 650 students passing the exam" (see class lecture)
# vector of probabilities of exactly 650, 651, 652, ..., 799, 800 successes in B(800,0.82):
  x = seq(650,800)
  dbinom(x,size=800,prob=0.82)

# probability of AT LEAST 650 sucesses in B(800,0.82):
  sum(dbinom(x,800,0.82))

# probability of AT MOST 600 sucesses in B(800,0.82):
  sum(dbinom(seq(0,600),800,0.82))
# this can easily be computed via binomial function CDF:
  pbinom(600,size=800,prob=0.82)
  
# How do we use CDF to compute probability of AT LEAST 650 sucesses?
# The complementary event of "AT LEAST 650 successes in 800 trials"
#                          is "AT MOST 649 successes in 800 trials"
  
  pbinom(649,size=800,prob=0.82)    # prob of AT MOST 649 successes in 800 trials
  1-pbinom(649,size=800,prob=0.82)  # prob of AT LEAST 650 successes in 800 trials

# alternatively, we can use lower.tail optional argument:
  pbinom(649,800,0.82,lower.tail=FALSE)

# Generating binomial random numbers: vector of 7 r.n. from B(800,0.82)
  rbinom(7,800,0.82)
