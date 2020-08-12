#
#  OPIM 5603 RStudio OPTIONAL Session 2+
#  Wednesday, September 4, 2019
#

# Binomial random variable: B ~ Bin(n,p). Then (see lectures)
#   mu = E(B) = np
#   sigm = sqrt(Var(B)) = sqrt(npq), with q = 1-p
#
# Define Transformed Binomial random variable by T = (B-mu)/sigm.
#   Then E(T) = 0 and Var(T) = 1 (will be shown in class soon)

# Function f plots the primitive histogram of
#   the Transformed Binomial B(n,p)
#   plotted over interval [-4,4] on the x-axis

  f = function(n,p,col="orange",head="",wd=100)
  {
    mu = n*p; sigm = sqrt(mu*(1-p))
    k = seq(-mu/sigm,(n-mu)/sigm,1/sigm)
    plot(k,sigm*dbinom(seq(0,n),n,p),xlim=c(-4,4),ylim=c(0,0.4),
         lwd=wd/sqrt(n),col=col,type="h",lend="butt",
         axes=F,xlab="",ylab="",main=head,col.main=col,
         panel.first=grid(col="lightgray",lty="dotted"))
    axis(1,pos=0,lwd=2)
    axis(2,pos=0,lwd=2,lwd.ticks=0,labels=F)
    axis(2,pos=-4,col="gray90")
    return(NA)
  }
   
# Try it:
  f(800,0.82)
  
# Fix some p between 0 and 1 and (not a very large) integer N

  p = 0.82
  N = 15

# The loop below runs a function f(n,p) for
#   n = 2, 4, 8, 16, 32, 64, ..., 2^N  (powers of 2)
# and with randomly chosen colors of the histograms depicting
# the probability density of the Transformed Binomial(n,p).
# As n increases the histograms appear to shape toward a
# 'Standard Normal' bell-curve, regradless of the choice of p.
  
  for (i in seq(1,N))
  {
    f(2^i,p,do.call(rgb,as.list(runif(3))),
      paste("Transformed Binomial ( n =",2^i,", p =",p,")"))
    Sys.sleep(1)
  }
