#
#  OPIM 5603 RStudio Session 6
#  Wednesday, October 2, 2019
#


# Recall from Session 5: plotting Sample Mean histogram and empirical density
#              myhist()  "my histogram"
#                myhd()  "my histogram & (empirical) density"
  
  myhist = function(v,breaks,freq=F,ylim=NULL,xlab=NULL,main=NULL,
                    col="yellow",border="orange2")
  {
    hist(v,breaks=breaks,freq=freq,ylim=ylim,xlab=xlab,main=main,col=col,border=border,
         col.main=border,font.main=1,cex.main=1.25); grid()
  }

  myhd = function(v,breaks,ylim=NULL,xlab=NULL,main=NULL,
                  col="lightyellow",border="orange2",lwd=2,dcol="red")
  {
    myhist(v,breaks=breaks,freq=F,ylim=ylim,col=col,border=border,xlab=xlab,main=main)
    lines(density(v),lwd=lwd,col=dcol)
  }
  
# Recall: SampleMean of two, three, five Unif(0,1)'s
  
  N = 100000
  strN = format(N,big.mark=",",digits=0,scientific=F) #statistic
  
  myhd(replicate(N,mean(runif(2))),seq(0,1,0.01),ylim=c(0,3),
       main=paste("Two Unif(0,1) Sample Mean:",strN,"data points"))
  # n=2 in this case, the main is deplicated by 100000 times - iggy
  # the distribution is like a triangle - iggy slide 2.15
  
  myhd(replicate(N,mean(runif(3))),seq(0,1,0.01),ylim=c(0,3),
       main=paste("Three Unif(0,1) Sample Mean:",strN,"data points"))
  # more like a bell curve - iggy
  
  myhd(replicate(N,mean(runif(5))),seq(0,1,0.01),ylim=c(0,3),
       main=paste("Five Unif(0,1) Sample Mean:",strN,"data points"))
  

# Note: Representative of the Sample Mean of a sample from Unif(0,1) of size 2
#       was obtained by following the design on slide 2.15.

  myhd(replicate(N,mean(runif(2))),seq(0,1,0.01),ylim=c(0,2.5),
       main=paste("Two Unif(0,1) Sample Mean:",strN,"data points"))
  # replicate function is much faster than the method below - iggy
  
#       Sometimes (particularly for small sample sizes) the design on slide 2.14
#       may be simpler.  For example, in this case we have
  
  myhd((runif(N)+runif(N))/2,seq(0,1,0.01),ylim=c(0,2.5), 
       main=paste("Two Unif(0,1) Sample Mean:",strN,"data points\n(alternative design)"))
  # will get the same results, but this is not good if n is large e.g. 40 - iggy
  # both of them are design to generate random vectors - iggy
  
  N = 1000000   # try both for N = million, see the execution times
  
# In other words, both
#     replicate(N,mean(runif(2)))
# and
#     (runif(N)+runif(N))/2
# are good representatives of the Sample Mean of a sample from Unif(0,1) of size 2.
# Second approach is much faster yet it's harded to construct for larger sample sizes.
  

    
# --------- Functional and aesthetic improvements to myhist() and myhd() --------- 

# Start with auxiliary function thatdetermines the position of the y-axis??? on the plot:
#    if zero is within x-axis interval [a,b] the y-axis position is zero, otherwise a
  
  axpos = function(intv)
  {
    if (intv[1]<=0 & tail(intv,1)>=0)
      return(0)
    else
      return(intv[1])
  }
  
  axpos(seq(-10,20))    # must return  0 # if one smaller and one larger than 0, return 0
  axpos(seq(3,20))      # must return  3
  axpos(seq(-9,-1))     # must return -9

  axpos(c(-15,4))       # must return  0
  axpos(c(5,18))        # must return  5
  axpos(c(-6,-2))       # must return -6
  
  # even simpler construct
  axpos = function(intv)
    return(intv[1] * !(intv[1]<=0 & tail(intv,1)>=0))

# Auxilliary grid and axis plotting function
  myaxes = function(xaxpos,yaxpos,lwd=2,col="gray50",las=1)
  {
    grid()
    axis(1,pos=xaxpos,lwd=lwd,col=col,las=las)
    axis(2,pos=yaxpos,lwd=lwd,col=col,las=las)
  }
    
# How to use?
  x = seq(-4,4,0.01)
  plot(x,dnorm(x),type="l",col="blue",axes=F)
  myaxes(0,axpos(x))

  x = seq(6,14,0.01)
  plot(x,dnorm(x,10,1),type="l",col="green4",axes=F) # the curve is shifted by 10
  myaxes(0,axpos(x))

  breaks = seq(1,9,0.1)  
  hist(rnorm(10000,5,1),freq=F,col="lightyellow",border="orange2",axes=F)
  myaxes(0,axpos(breaks))

  breaks = seq(-4,4,0.1)  
  hist(rnorm(10000),freq=F,col="palegreen",border="green4",axes=F)
  myaxes(0,axpos(breaks))
  
# Rewritten myhist() and myhd() functions
  
  # Note: breaks="Sturges" is the default value ofr breaks in hist() function
  #       (type in 'hist' in the search field of Help tab to the left)
  
  myhist = function(v,breaks="Sturges",freq=F,ylim=NULL,xlab=NULL,main=NULL, # breaks="Sturges" is the default value of breaks 
                    col="yellow2",border="orange2",axes=T)
  {
    main=paste(main,": ",format(length(v),big.mark=",",digits=0,scientific=F),
               " data points",sep="")
    h = hist(v,breaks=breaks,freq=freq,ylim=ylim,xlab=xlab,main=main,col=col,border=border,
             col.main=border,font.main=1,cex.main=1.25,axes=F)
    if (axes)
      myaxes(0,axpos(h$breaks))
    invisible(h)     # returns histogram object (as return(h), but no console printout)
  }

  myhd = function(v,breaks="Sturges",ylim=NULL,xlab=NULL,main=NULL,
                  col="lightyellow",border="orange2",dlwd=2,dcol="red",axes=T)
  {
    h = myhist(v,breaks=breaks,freq=F,ylim=ylim,xlab=xlab,main=main,
               col=col,border=border,axes=F)
    lines(density(v),lwd=dlwd,col=dcol)
    if (axes)
      myaxes(0,axpos(h$breaks))
    invisible(h)     # returns histogram object (as return(h), but no console printout)
  }

# Try myhist()  
  myhist(replicate(100000,mean(runif(2))),seq(0,1,0.01),ylim=c(0,3),
         main="2 Unif(0,1) Sample Mean")

# --------- Plotting Sample Mean histogram and empirical density (cont.) ---------
  
# Note: we will use extensively some standard functions and to make lines a bit shorter
#       we give them shorter 'nicknames':
#       - replicate() is shorten to rpl()
#       - string concatenation function paste() is shorten to p()
  
  rpl = replicate; p = paste
  
# Try myhd() on SampleMean of two, three, five,... Unif(0,1)'s
# the following breaks sequence and part of the chart title are common:
  breaks = seq(0,1,0.01) # 100 intervals -iggy
  tt = "Unif(0,1) Sample Mean" # part of the title - iggy

  myhd(rpl(100000,mean(runif(2))),breaks,ylim=c(0,3),main=p(2,tt))# the red line is th empirical function
  myhd(rpl(100000,mean(runif(2))),ylim=c(0,3),main=p(2,tt))       # skip breaks #just let R to decide breaks -iggy
  myhd(rpl(100000,mean(runif(3))),breaks,ylim=c(0,3),main=p(3,tt))# bell shaping and higher - iggy
  myhd(rpl(100000,mean(runif(5))),breaks,ylim=c(0,3),main=p(5,tt))
  myhd(rpl(100000,mean(runif(17))),breaks,ylim=c(0,3),main=p(17,tt))
  myhd(rpl(100000,mean(runif(17))),breaks,main=p(17,tt))          # skip ylim! - let r decide 
       
  # Note: Optional argument length.out of seq() command specifies the length
  #       of the vector and its name can be abbreviated as l (small letter L).
  #       To get exactly n bins the length of the seq() vector must be n+1.
  # Examples:
  seq(0,1,0.1)
  seq(0,1,l=11)# 10 bins = 11 points -iggy 
  seq(0,1,l=10)# 10 points -iggy
  
  
# What if we try SampleMean of independent standard normal's?
  
  myhd(rpl(100000,mean(rnorm(2))),seq(-4,4,l=101),main="2 N(0,1) Sample Mean") # sample mean of normal is still normal, but not the variance
  myhd(rpl(100000,mean(rnorm(2))),seq(-4,4,l=101),ylim=c(0,0.6),main="2 N(0,1) Sample Mean")

# The histogram and density seem Bell-Curve-shaped
# So we want to compare them to a pdf of a normal N(SMm,SMsd^2)!
#    But for which mean SMm and variance SMsd^2 ?
#    Notation: SMm stands for SampleMean mean and
#             SMsd for SampleMean standard deviation.
# Try with SMm=0 and couple of trials for standard deviation:
  x=seq(-4,4,l=1001)                                        
  lines(x,y=dnorm(x,mean=0,sd=1),lwd=3,col="magenta")         # variance = 1
  lines(x,y=dnorm(x,mean=0,sd=sqrt(2)),lwd=3,col="green2")    # variance = 2
  lines(x,y=dnorm(x,mean=0,sd=sqrt(1/2)),lwd=3,col="blue")    # variance = 1/2 # theoretical distribution -iggy
  # Therefore variance = 1/2 seems to produce the right fit
  # just confirm for the narmal sample! -iggy
  
# Recall lectures:
#
#  If X1,...Xn is a sample from any distribution (CDF) F
#  with population mean mu and population variance sigm^2
#  then the SampleMean = (X1+...+Xn)/n is a random variable with 
#  expected value mu and variance sigm^2/n (slide 2.7)
#
#  Moreover, if X1,...Xn is a sample from NORMAL distribution N(mu,sigm^2) then
#  the SampleMean is a NORMAL random variable N(mu,sigm^2/n) (slide 2.11)
#
#  This answers "for which parameters" question:
#    If we want to compare the SampleMean histogram (density) to a pdf of
#    some Normal distribution with mean SMm ('SampleMean mean')
#    and standard deviation SMsd ('SampleMean standard deviation')
#    then slide 2.11 says that SMm = mu and SMsd = sigm/sqrt(n)

# Rewritten: What if we start with normal N(0,1) random numbers?
  
  N = 100000            # the number of replicates
  mu = 0; sigm = 1      # population mean and population standard deviation
  n = 2                 # sample size
  
  myhd(rpl(N,mean(rnorm(n,mean=mu,sd=sigm))),seq(-5,5,l=101),ylim=c(0,0.6),
       main=p(n,"N(0,1) Sample Mean"))
  
  # calculation how to compare with theoretical distribution 
  x=seq(-5,5,l=1001)    # increase 10x resolution on the x-axis for pdf plot
  SMm = mu              # expected value of SampleMean (= population mean)
  SMsd = sigm/sqrt(n)   # standard deviation of SampleMean
  lines(x,y=dnorm(x,mean=SMm,sd=SMsd),lwd=3,col="blue")

# Recap: This section compares the histogram and empirical density of the
#        simulation obtained SampleMean of normal N(mu,sigm^2) sample to 
#        to the ('theoretical') pdf of Normal N(mu,sigm^2/n) distribution.
#        This is done to 'test' the statement on the lecture slide 2.11


# We would like to re-use the code above for different random number generators:
# This procedure adds comparison to normal density (pdf) to the myhd();
#
# Notice: in order to apply proper normal pdf we need as inputs:
#         - mean = SampleMean expected value (= mu), 
#         -   sd = SampleMean standard deviation (= sigm/sqrt(n)).
#
# Furthermore, hd input is used to specify which histogram plotting function
# should be used. Its default value is myhd() function.  In Session 6+ a more
# general function myhd2() is designed that separately deals with discrete data.

# Name: myhdnd()  "my histogram & (empirical) density & normal (distr) density"
  
  myhdnd = function(v,mean=0,sd=1,breaks="Sturges",ylim=NULL,xlab=NULL,main="rv's",
                    col="lightyellow",border="orange2",
                    dlwd=2,dcol="red",bclwd=2,bccol="blue",axes=T,
                    hd=myhd) # don't have densities? listen -40:00:00 iggy
  {
    h = hd(v,breaks=breaks,ylim=ylim,xlab=xlab,main=main,col=col,border=border,
           dlwd=dlwd,dcol=dcol,axes=F)
    x=seq(h$breaks[1],tail(h$breaks,1),l=10*length(h$breaks))
    lines(x,y=dnorm(x,mean,sd),lwd=bclwd,col=bccol)
    if (axes)
      myaxes(0,axpos(h$breaks))
  }

  # Re-run the code above:
  N = 100000
  mu = 0; sigm = 1
  n = 2
  myhdnd(rpl(N,mean(rnorm(n,mu,sigm))),mu,sigm/sqrt(n),seq(-4,4,l=100),ylim=c(0,0.6),
         main="2 N(0,1) Sample Mean")

# Apply the procedure to independent Uniform's:
#   Beware that the population mean mu and population standard deviation sigm
#   MUST be calculated from the Uniform distribution parameters min and max,
#   unlike the case of Normal distribution where they were the distribution parameters
  
  N = 100000
  min = 0; max = 1
  mu = (min+max)/2               # e.g., Unif(0,1) population mean is 0.5
  sigm = sqrt((max-min)^2/12)    # e.g., Unif(0,1) population variance 1/12
  
  # the following breaks sequence and part of the chart title are common:
  tt = p("Unif(",min,",",max,") Sample Mean",sep="")
  breaks = seq(min,max,l=101)
  
  # Two independent Uniforms; n = 2
  n=2;   myhdnd(rpl(N,mean(runif(n,min,max))),mu,sigm/sqrt(n),breaks,main=p(n,tt))
# same triangle as before 
  
  # Try n=3, n=5, n=17, n=250.  What do you observe?
  n=3;   myhdnd(rpl(N,mean(runif(n,min,max))),mu,sigm/sqrt(n),breaks,main=p(n,tt))
  n=5;   myhdnd(rpl(N,mean(runif(n,min,max))),mu,sigm/sqrt(n),breaks,main=p(n,tt))
  n=17;  myhdnd(rpl(N,mean(runif(n,min,max))),mu,sigm/sqrt(n),breaks,main=p(n,tt))
  n=250; myhdnd(rpl(N,mean(runif(n,min,max))),mu,sigm/sqrt(n),breaks,main=p(n,tt))
  # zoom-in on the horizontal axis:
  n=250; myhdnd(rpl(N,mean(runif(n,min,max))),mu,sigm/sqrt(n),seq(0.4,0.6,l=101),main=p(n,tt))
  
# Apply the procedure to independent Exp(1)'s
  N = 100000
  lam = 1
  mu = 1/lam     # Exp(lam) population mean
  sigm = 1/lam   # Exp(lam) population standard deviation
  tt = p("Exp(",lam,") Sample Mean",sep="")

  # Two independent Exponentials; set breaks = 100 to see where the values fall-in
  n=2; myhdnd(rpl(N,mean(rexp(n,lam))),mu,sigm/sqrt(n),breaks=100,main=p(n,tt))

  n=2; myhdnd(rpl(N,mean(rexp(n,lam))),mu,sigm/sqrt(n),seq(0,9,l=101),main=p(n,tt))

  # Try n=3, n=5, n=17, ...  What do you observe?
  n=3;   myhdnd(rpl(N,mean(rexp(n,lam))),mu,sigm/sqrt(n),seq(0,9,l=101),main=p(n,tt))
  n=5;   myhdnd(rpl(N,mean(rexp(n,lam))),mu,sigm/sqrt(n),seq(0,9,l=101),main=p(n,tt))
  n=17;  myhdnd(rpl(N,mean(rexp(n,lam))),mu,sigm/sqrt(n),seq(0,9,l=101),main=p(n,tt))
  # zoom-in on the horizontal axis:
  n=17;  myhdnd(rpl(N,mean(rexp(n,lam))),mu,sigm/sqrt(n),seq(0,2.5,l=101),main=p(n,tt))# still skew
  n=30;  myhdnd(rpl(N,mean(rexp(n,lam))),mu,sigm/sqrt(n),seq(0,2.5,l=101),main=p(n,tt))
  n=250; myhdnd(rpl(N,mean(rexp(n,lam))),mu,sigm/sqrt(n),seq(0,2.5,l=101),main=p(n,tt))
  n=250; myhdnd(rpl(N,mean(rexp(n,lam))),mu,sigm/sqrt(n),seq(0.5,1.5,l=101),main=p(n,tt))
  
# Apply the procedure to independent Student t(4)'s
  N = 100000
  df = 4
  mu = 0                 # t(df) population mean
  sigm = sqrt(df/(df-2)) # t(df) population standard deviation (df >2)
  tt = p("t(",df,") Sample Mean",sep="")

  # Two independent t(4)'s; n = 2
  n=2; myhdnd(rpl(N,mean(rt(n,df))),mu,sigm/sqrt(n),seq(-25,25,l=101),main=p(n,tt))

  # Try n=3, n=5, n=17, n=250.  What do you observe?
  n=3;   myhdnd(rpl(N,mean(rt(n,df))),mu,sigm/sqrt(n),seq(-15,15,l=101),main=p(n,tt))
  n=5;   myhdnd(rpl(N,mean(rt(n,df))),mu,sigm/sqrt(n),seq(-10,15,l=101),main=p(n,tt))
  n=17;  myhdnd(rpl(N,mean(rt(n,df))),mu,sigm/sqrt(n),seq(-5,5,l=101),main=p(n,tt))
  n=250; myhdnd(rpl(N,mean(rt(n,df))),mu,sigm/sqrt(n),seq(-0.7,0.7,l=101),main=p(n,tt))


# --------- Plotting Transformed Sample Mean histogram and empirical density ---------

# A clever way to avoid varying SMm and SMsd is to transform the SampleMean:
#     This random variable has expected value zero and variance 1 (see slide 2.10).
#     Hence if its empirical density is compared to the density of some Normal variable
#     that variable is the Standard Normal N(0,1).
#
# Define a transformation function.  Input parameters are:
#        v: vector representing r.v's in the sample
#       mu: population mean
#     sigm: population standard deviation

  TSM = function(v,mu,sigm) #transform mean
  {
    n = length(v)
    SMm = mu; SMsd = sigm/sqrt(n)
    return((mean(v)-SMm)/SMsd)
  }
  
  # try couple of times
  v = runif(2,0,1); v;  mean(v);  TSM(v,1/2,sqrt(1/12)) #transformed mean 
  TSM(c(1,1),1/2,sqrt(1/12))
  TSM(c(1/2,1/2),1/2,sqrt(1/12))
  
# v is 2 random numbers from unif dis; 

# Apply the procedure to independent Uniforms
  N = 100000
  min = 0; max = 1
  mu = (min+max)/2               # e.g., Unif(0,1) population mean is 0.5
  sigm = sqrt((max-min)^2/12)    # e.g., Unif(0,1) population variance 1/12
  tt = p("Unif(",min,",",max,") Transformed Sample Mean",sep="")
  br = seq(-5,5,l=101)           # TSM values are compared to N(0,1) pdf

  # Two independent Uniforms; n = 2
  n=2; myhdnd(rpl(N,TSM(runif(n,min,max),mu,sigm)),breaks=br,main=p(n,tt))
  # Note that mean=0 and sd=1 are omitted, since those are the default values
  
  # Try n=3, n=5, n=17, n=250.  What do you observe?
  n=3;   myhdnd(rpl(N,TSM(runif(n,min,max),mu,sigm)),breaks=br,main=p(n,tt))
  n=5;   myhdnd(rpl(N,TSM(runif(n,min,max),mu,sigm)),breaks=br,main=p(n,tt))
  n=17;  myhdnd(rpl(N,TSM(runif(n,min,max),mu,sigm)),breaks=br,main=p(n,tt))
  n=30;  myhdnd(rpl(N,TSM(runif(n,min,max),mu,sigm)),breaks=br,main=p(n,tt))
  n=250; myhdnd(rpl(N,TSM(runif(n,min,max),mu,sigm)),breaks=br,main=p(n,tt))


# Central Limit Theorem: The Transformed Sample Mean
#  converges 'in distribution' to N(0,1) as sample size n increases

# Transformed SampleMean of two independent exponential Exp(1) random numbers
  N = 100000
  lam = 1
  mu = 1/lam     # Exp(lam) population mean
  sigm = 1/lam   # Exp(lam) population standard deviation
  tt = p("Exp(",lam,") Transformed Sample Mean",sep="")

  # Two independent Exponentials; set breaks = 100 to see where the values fall-in
  n=2; myhdnd(rpl(N,TSM(rexp(n,lam),mu,sigm)),breaks=100,main=p(n,tt))
  
  # try with seq from -4 to 10
  n=2; myhdnd(rpl(N,TSM(rexp(n,lam),mu,sigm)),breaks=seq(-4,10,l=101),main=p(n,tt))

  # Repeat this for n=3, n=5,...
  n=3;  myhdnd(rpl(N,TSM(rexp(n,lam),mu,sigm)),breaks=seq(-4,10,l=101),main=p(n,tt))
  n=5;  myhdnd(rpl(N,TSM(rexp(n,lam),mu,sigm)),breaks=seq(-4,10,l=101),main=p(n,tt))
  n=17; myhdnd(rpl(N,TSM(rexp(n,lam),mu,sigm)),breaks=seq(-4,10,l=101),main=p(n,tt))
  n=30; myhdnd(rpl(N,TSM(rexp(n,lam),mu,sigm)),breaks=seq(-4,10,l=101),main=p(n,tt))
  # Notice that the convergence to N(0,1) is much slower than for the uniforms.
  
  # Observe that with larger sample sizes the x-axis limits now can be changed,
  # since by the CLT the distribution should be closer to Standard Normal
  n=30;  myhdnd(rpl(N,TSM(rexp(n,lam),mu,sigm)),breaks=seq(-5,6,l=101),main=p(n,tt))
  n=250; myhdnd(rpl(N,TSM(rexp(n,lam),mu,sigm)),breaks=seq(-5,6,l=101),main=p(n,tt))


# Note: function myhdnd() is fairly general and can be re-used whenever we want to 
#       quickly check whether a data vector 'follows' normal distribution.
#       Recall the birth weight and cosmic rays data from Session 4:
  
  Weight = read.csv("/Users/Iggy Zhao/Documents/opim5603/Data04/BirthWeight.csv")
  w = Weight$kg
  length(w)
  min(w); max(w)
  mean(w); sd(w)
  myhdnd(w,mean(w),sd(w),breaks=100,main="Birth Weight data")
  
  Rays = read.csv("/Users/Iggy Zhao/Documents/opim5603/Data04/Rays.csv")
  dt = Rays$deltaT
  myhdnd(dt,mean(dt),sd(dt),col="palegreen",border="green4",
         breaks=100,main="Interarrival Times of cosmic rays data")



# --------- Illustration of CLT via empirical CDF function in R ---------

# Central Limit Theorem: The Transformed SampleMean
#             (SampleMean-mu) / (sigm/sqrt(n))
#        converges 'in distribution' to N(0,1) as sample size n increases
  
# Empirical CDF in R: ecdf()
  
  w = c(0.15,0.21,0.33,0.75,0.82); w
  plot(ecdf(w),lwd=3); grid()
  abline(v = w, col="orange",lty=2)
  
  w = runif(4); w
  ecdf(w)
  plot(ecdf(w),xlim=c(-0.1,1.1),col="red",lwd=3); grid()
  abline(v = w, col="orange",lty=3)
  
  v = runif(100)
  plot(ecdf(v),xlim=c(-0.1,1.1),col="red",lwd=3); grid()
  
  v = runif(10000)
  plot(ecdf(v),xlim=c(-0.1,1.1),col="red",lwd=3); grid()
# compare it to CDF of Unif(0,1)
  lines(x=c(0,1),y=c(0,1),col="blue",lwd=3)


# We illustrate the Central Limit Theorem, namely the convergence of 
#   Transformed Sample Means to a N(0,1), by plotting the empirical CDFs of
#   Transformed Sample Means and comparing them to the Standard Normal CDF (pnorm)
  
# Use myhdnd() as a guide: instead of plotting histogram empirical density and
#   comparing density() against dnorm(),
#   we compare ecdf() to pnorm().
  
  myepnp = function(v,mean=0,sd=1,xlim=c(-3,3),ylim=c(0,1),xlab="",ylab="",main=NULL, # empirical CDF 
                   plwd=3,pcol="blue2",nplwd=2,npcol="red2",axes=T)
  {
    main=paste(main,": ",format(length(v),big.mark=",",digits=0)," data points",sep="")
    plot(ecdf(v),xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,main=main,lwd=plwd,col=pcol,
         col.main=pcol,font.main=1,cex.main=1.25,axes=F)
    x=seq(xlim[1],xlim[2],l=100*(xlim[2]-xlim[1]))
    lines(x,y=pnorm(x,mean,sd),lwd=nplwd,col=npcol)
    if (axes)
      myaxes(0,axpos(xlim))
  }
  
# Now we want to look at samples from different distributions.
# Start with small sample sizes and while increasing them follow the changes on the plots
#
# We can start with a 'sample' of size 1:
#   choose a distribution (and normal is a not a good choice here: why?)
#   and create N replicates of a random number from r.v. (X - mu)/sigm
#   (which is a transformed 'sample mean' for a 'sample' of size 1)
#   myepmp() will plot the empirical CDF of (X - mu)/sigm
#   and compare it to a Standard Normal CDF.  

# Try the Uniform distribution U(1,6):
#   Pop mean mu = 3.5
#   Pop.variance sigma^2 = 25/12, so pop.sd = sqrt(25/12)
  
  myepnp(rpl(5,TSM(runif(1,1,6),3.5,sqrt(25/12))), #subtract 3.5 
         main=p(1,"Unif(1,6) Transformed Sample Mean"))

# Similarly as above: set the variables that are parameters of the distribution
#   as well as population mean and population st.deviation
  
  N = 10000
  min = 1; max = 6
  mu = (min+max)/2                       # Unif(min,max) population mean
  sigm = sqrt((max-min)^2/12)            # Unif(min,max) population standard deviation
  
  tt = paste("Unif(",min,",",max,") Transformed Sample Mean",sep="")

  # Changing sample sizes n: 1, 2, 3, 5. 50
  n=1;  myepnp(rpl(N,TSM(runif(n,min,max),mu,sigm)),main=p(n,tt))
  n=2;  myepnp(rpl(N,TSM(runif(n,min,max),mu,sigm)),main=p(n,tt))
  n=3;  myepnp(rpl(N,TSM(runif(n,min,max),mu,sigm)),main=p(n,tt))
  n=5;  myepnp(rpl(N,TSM(runif(n,min,max),mu,sigm)),main=p(n,tt))
  n=50; myepnp(rpl(N,TSM(runif(n,min,max),mu,sigm)),main=p(n,tt))

# Nicer visualization: make a primitive animation with sample sizes increasing
  N = 10000
  for (n in seq(1,10))
  {
    myepnp(rpl(N,TSM(runif(n,min,max),mu,sigm)),main=p(n,tt))
    Sys.sleep(1.0)
  }
  
# Exponential distribution
  lambda = 1
  mu = 1/lambda        # Exp(lambda) population mean
  sigm = 1/lambda      # Exp(lambda) population standard deviation
  tt = paste("Exp(",lambda,") Transformed Sample Mean",sep="")

  for (n in c(1,2,3,4,5,10,20,30,100,200,300,1000))
  {
    myepnp(rpl(N,TSM(rexp(n,lambda),mu,sigm)),main=p(n,tt))
    Sys.sleep(1.0)
  }


# --------- Biased vs. Unbiased Sample Variance ---------

# Create a function "SampleVariances" that returns a pair
#   (biased Sample Variance, unbiased Sample Variance)

  SVs = function(v)
  {
    SM = mean(v)
    ssq = sum((v-SM)^2)
    ssize = length(v)
    return(c(ssq/ssize,ssq/(ssize-1)))
  }
  
# Try it
  SVs(runif(4,0,1))
  SVs(runif(30,0,1))

# Replicate 7 times
  replicate(7,SVs(runif(4)))
  replicate(7,SVs(runif(30)))
  

# Note: 'replicate' produces a matrix with 2 rows and 7 columns:
#       the first row are the biased SV's and the second unbiased SV's
# R function 'rowMeans' produces the means of each row separately
  v = replicate(7,SVs(runif(4))); v
  rowMeans(v)
  # Since the population variance for Unif(0,1) is 1/12, it is reasonable
  # to multiply the results by 12 and compare them to 1
  12*rowMeans(v)
  
  
  N = 100000     # number of replicates
  
  12*rowMeans(replicate(N,SVs(runif(4))))
  12*rowMeans(replicate(N,SVs(runif(20))))
  

# Try for uniform Unif(1,6) sample of length n = 30.
# Population Variance = (6-5)^2/12 = 25/12 = 2.08333
  
  rowMeans(replicate(N,SVs(runif(30,1,6))))
  
# Try for standard normal sample of length n = 30.
# Compare the results to population variance of 1
  rowMeans(replicate(N,SVs(rnorm(30))))
  rowMeans(replicate(N,SVs(rnorm(30,5,3)))) # mu = 5 and sigm = 3, pop.variance = 3^2 = 9


# Note related to lecture slide 2.18: "Were the population mean known ..."
#   If we knew the population mean mu we could subtract it from X_k's
#   instead of a Sample Mean: we write a 'quasi' SVs function

  qSVs = function(v,popmean)
  {
    qssq = sum((v-popmean)^2)
    ssize = length(v)
    return(c(qssq/ssize,qssq/(ssize-1))) #qssq/ssize  -ppl variance-iggy
  }

# Repeat the initial steps above: the population mean of a Unif(0,1) is 1/2 = 0.5
# Run the following lines several times. What do you observe?
  12*rowMeans(replicate(100000,qSVs(runif(4),0.5)))
  12*rowMeans(replicate(100000,qSVs(runif(20),0.5)))

