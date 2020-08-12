#
#  OPIM 5603 RStudio Session 4
#  Wednesday, September 18, 2019
#

# Example 1
#   BirthWeight.csv contains the birth weights (in kg) of approx.
#   65 thousand newborn babies from the northeastern U.S. in 2018. 
#   How is the data distributed?
  
  Weight = read.csv("Data04/BirthWeight.csv")
  View(Weight)

# a quick way to see the quartiles and the average
  summary(Weight)
  
# extract the 'vector' of weights (true data)
  w = Weight$kg
  min(w); max(w); mean(w)
  length(w)
  
# cheap-shot at data distribution
  hist(w)

  hist(w,breaks=seq(1,5,0.05))
  hist(w,breaks=seq(1,5,0.05),freq=F)
  
  # nicer plot
  hist(w,breaks = seq(1,5,0.05),freq=F,
       col="orange",border="brown",xlab="kg",
       main="Weights of newborn babies",
       col.main="brown",font.main=1,cex.main=1.25); grid()

# Empirical Density function in R:
  densw = density(w)
  lines(densw,lwd=3,col="blue")

  
# Example 2
#   The rate at which the low energy (around 1 GeV) cosmic ray particles
#   arrive at the top of the atmosphere is about one per square centimeter
#   per second.  Rays.csv contains the inter-arrival times of low-energy
#   cosmic ray particles hitting a space station plate of area 1 sqcm
#   during the period of roughly 36 hours.  How is the data distributed?

  Rays = read.csv("Data04/Rays.csv")

  summary(Rays)
  
  dt = Rays$deltaT
  length(dt)
  
  hist(dt,breaks = seq(0,11,0.05),freq=F,
       col="green2",border="green3",xlab="seconds",
       main="Inter-arrivals of low-energy particles",
       col.main="green4",font.main=1,cex.main=1.25); grid()
  
  densdt = density(dt)
  lines(densdt,lwd=3,col="blue")


# -------------- Random numbers from normal distribution and histograms --------------
  
  rnorm(20)
  rnorm(20,mean=78,sd=13)
  
  hist(rnorm(100),breaks=seq(-5,5,1),freq=F,col="orange"); grid()

  hist(rnorm(100),breaks=seq(-5,5,1),freq=F,ylim=c(0,0.5),
     col="orange",border="brown",xlab="",
     main="100 N(0,1) random numbers, 10 bins",
     col.main="brown",font.main=1,cex.main=1.25); grid()
  
  hist(rnorm(10000),breaks=seq(-5,5,0.1),freq=F,ylim=c(0,0.5),
       col="orange",border="brown",xlab="",
       main="10K N(0,1) random numbers, 100 bins",
       col.main="brown",font.main=1,cex.main=1.25); grid()

  hist(rnorm(100000),breaks=seq(-5,5,0.1),freq=F,ylim=c(0,0.5),
       col="orange",border="brown",xlab="",
       main="100K N(0,1) random numbers, 100 bins",
       col.main="brown",font.main=1,cex.main=1.25); grid()
  lines(seq(-5,5,0.1),dnorm(seq(-5,5,0.1)),col="blue",lwd=2)     # compare to N(0,1) pdf
  
  hist(rnorm(1000000),breaks=seq(-6,6,0.12),freq=F,ylim=c(0,0.5),
       col="orange",border="brown",xlab="",
       main="1M N(0,1) random numbers, 100 bins",
       col.main="brown",font.main=1,cex.main=1.25); grid()
  lines(seq(-6,6,0.012),dnorm(seq(-6,6,0.012)),col="blue",lwd=2)      # compare to pdf
  
# Digression: What will this produce:
  v = pnorm(rnorm(10000))
  min(v); max(v)
  hist(v,breaks = seq(0,1,0.01),col = "orange",freq=F);grid()
  
  n = 1000000
  hist(pnorm(rnorm(n)),breaks=seq(0,1,0.01),freq=F,ylim=c(0,1.1),
       col="green3",border="green4",xlab="",
       main="1M random numbers, 100 bins. What is this?",
       col.main="green4",font.main=1,cex.main=1.25); grid()

# It seems that:  pnorm(rnorm()) = runif()
# True (sort of): rnorm() = qnorm(runif()), see lecture slide 1.45
  

# -------------- Exploring the Student t(n) distribution --------------
  
# Degrees-of-freedom parameter is named 'df' in R (n in class lectures)  
  dt(0,df=1)                # mode of t(1) pdf (green graph on lecture slide)

# Plot the pdf of t(1) distribution
  x = seq(-4,4,0.001)
  plot(x,dt(x,1),pch=19,cex=0.5,col="purple",ylim=c(0,0.4),
       axes=F,xlab="",ylab=""); grid()
  axis(1,pos=0,lwd=1,at=seq(-4,4),col="gray75")
  axis(2,pos=0,lwd=1,at=seq(0,0.4,0.1),col="gray75",las=1)
  lines(x,dt(x,25),lwd=2,col="orange")   # compare it to pdf of t(25)
  lines(x,dnorm(x),lwd=2,col="red")       # compare it to pdf of N(0,1)
  
# Plot the CDF of t(1) distribution
  plot(x,pt(x,1),pch=19,cex=0.5,col="purple",ylim=c(0,1),
       axes=F,xlab="",ylab=""); grid()
  axis(1,pos=0,lwd=1,at=seq(-4,4),col="gray75")
  axis(2,pos=0,lwd=1,at=seq(0,1,0.1),col="gray75",las=1)
  lines(x,pnorm(x),lwd=2,col="red")       # compare it to CDF of N(0,1)

# Question: how wide is the range of values for a random vector from N(0,1) distr? 
# Note: "the range of values" refers to max(vector)-min(vector)  
  v = rnorm(10000)
  summary(v)
  max(v)-min(v)                           # notice how NARROW the range is
  
# Same question for the Student t(1) distr:
  v = rt(10000,df=1)
  summary(v)
  max(v)-min(v)                           # notice how WIDE the range is

# Question: of these 10000 random numbers how many are outside [-100,100]?
  length(v[v > 100])
  length(v[v < -100])
  length(v[v < -100 | v > 100])

  length(v[v>-100 & v<100])               # how many are inside [-100,100]?

# This is the reason why we get a useless histogram if we let R specify the bins:
  hist(v,100,col="violet")     # 100 for hundred bins

# We can try to specify breaks, but there is a problem
  hist(rt(10000,1),breaks=seq(-100,100,2),col="violet")

# Why? Because there is a large probability that t(1) will produce the value
#      less than -100 or more than 100. We can compute that probability:
  pt(-100,1) + 1 - pt(100,1)      # approx. 0.637% probability

# Hence when we generate 10000 random numbers from t(1)
# roughly 64 numbers will be outside [-100,100] interval.

# Workaround: let R do the breaks but assign a large number of bins.
# Then restrict the x-axis by using 'xlim' option.
# t-distribution pdf's lie below the N(ormal)0,1) pdf, so limit the y-axis as well 
  
  hist(rt(100000,1),1000000,xlim=c(-10,10),ylim=c(0,0.4),freq=F,
       col="violet",border="purple",xlab="",
       main="100K t(1) random numbers, 1M bins",
       col.main="purple",font.main=1,cex.main=1.25); grid()
  
# Setback: bin widths can change noticably, depending on how large the range is.
#          Run the following block multiple times:
  v = rt(100000,1)
  round(max(v)-min(v),0)
  hist(v,1000000,xlim=c(-10,10),ylim=c(0,0.4),freq=F,
       col="violet",border="purple",xlab="",
       main="100K t(1) random numbers, 1M bins",
       col.main="purple",font.main=1,cex.main=1.25); grid()

# now compare the histogram to the standard normal pdf
  x = seq(-10,10,0.2)
  lines(x,dnorm(x),col="red2",lwd=3)
# further, compare the histogram to the t(1) pdf
  lines(x,dt(x,1),col="blue",lwd=3)
  
# run the following block multiple times: it summarizes the analysis above
  x = seq(-6,6,0.2)
  d = 1; n = 100000; bins = 1000000
  v = rt(n,d)
  hist(v,bins,xlim=c(-6,6),ylim=c(0,0.4),freq=F,
       col="violet",border="purple",xlab="",
       main="100K t(1) random numbers, 1M bins",
       col.main="purple",font.main=1,cex.main=1.25); grid()
  lines(x,dnorm(x),col="red2",lwd=3)
  lines(x,dt(x,d),col="blue",lwd=3)
  round(max(v)-min(v),0)
  
# re-run this code for df = 5
  x = seq(-6,6,0.2)
  d = 5; n = 100000; bins = 100000
  v = rt(n,d)
  round(max(v)-min(v),0)
  hist(v,bins,xlim=c(-6,6),ylim=c(0,0.4),freq=F,
       col="green2",border="green4",xlab="",
       main="100K t(5) random numbers, 1M bins",
       col.main="green4",font.main=1,cex.main=1.25); grid()
  lines(x,dnorm(x),col="red2",lwd=3)
  lines(x,dt(x,d),col="blue",lwd=3)

# Why does the histogram look awful?
# Look at the range of this vector: max-min is likely ~40
# Thus we have too many bins for such a small range. Try bins = 400  
  x = seq(-6,6,0.2)
  d = 5; n = 100000; bins = 400
  v = rt(n,d)
  hist(v,bins,xlim=c(-6,6),ylim=c(0,0.4),freq=F,
       col="green2",border="green4",xlab="",
       main="100K t(5) random numbers, 400 bins",
       col.main="green4",font.main=1,cex.main=1.25); grid()
  lines(x,dnorm(x),col="red2",lwd=3)
  lines(x,dt(x,d),col="blue",lwd=3)
  round(max(v)-min(v),0)
  
# try on your own: d=10, d=25

  
# -------------- Briefly on Exponential distribution --------------
  
# Parameter lambda is called 'rate' in R. Default value is rate = 1.
  dexp(0,rate=1)
  dexp(0)
  
# plot the various pdf's and CDF's
  x=seq(0,10,0.001)
  plot(x,dexp(x),cex=0.4,col="blue",bty="n"); grid() # pdf of Exp(1)
  lines(x,dexp(x,rate=0.5),lwd=2,col="green3")       # pdf of Exp(1/2)
  lines(x,dexp(x,rate=2),lwd=2,col="purple")         # pdf of Exp(2)
  
  lines(x,pexp(x),lwd=2,col="red")                   # CDF of Exp(1)
  lines(x,pexp(x,0.5),lwd=2,col="darkred")           # CDF of Exp(1/2)
  lines(x,pexp(x,2),lwd=2,col="pink")                # CDF of Exp(2)
  
# random numbers from Exp(1)
  rexp(10,rate=1)

  v = rexp(10000)
  summary(v)
  max(v)-min(v)
  hist(v,100,
       col="lightblue",border="blue2",xlab="",
       main="10K Exp(1) random numbers, 100 bins",
       col.main="blue2",font.main=1,cex.main=1.25); grid()

# On 'breaks': how likely it is that an Exp(1) random number is larger than 12?  
  1-pexp(12)     # approx. 6 in a million
  
  hist(rexp(10000),seq(0,12,0.1),freq=F,ylim=c(0,1),
       col="lightblue",border="blue2",xlab="",
       main="10K Exp(1) random numbers, 120 bins",
       col.main="blue2",font.main=1,cex.main=1.25); grid()
  lines(x,dexp(x),lwd=2,col="blue")


    
# -------------- Simulating Poisson Arrival Process --------------

# Digression; what is a cumulative sum function in R?
  a = seq(1,5)
  cumsum(a)
  
# exploring basic ideas
  interarr = rexp(1000,rate=10)      # vector of 1000 Exp(10) distributed interarrival times 
  interarr[1:50]                     # see what first 50 interarrival times look like
  arr = cumsum(interarr)             # the corresponding arrival times (cumulative sums)
  arr[1:50]                          # see what first 50 arrival times look like
  
  st = 7; et = 9                     # start and end of the time interval
  arr[st <= arr & arr < et]          # sub-vector of all arrival times between 7 and 9
  length(arr[st <= arr & arr < et])  # count of all arrival times between 7 and 9
  
# Poisson Arrivals function: Number of arrivals within time interval [st,et)
  
  NumArr1 = function(n,lam=1,st=0,et=1)
  {
    arr = cumsum(rexp(n,rate=lam))
    print(max(arr))                  # note max(arr) = arr[n], since arr is an increasing sequance
    return(length(arr[st<=arr & arr<et]))
  }
  
# try it
  NumArr1(1000,lam=10,st=7,et=9)
  NumArr1(20,lam=10,st=7,et=9)
  
# What is the problem?  Need more arrivals, i.e., more than 20 exponential random numbers!
# We can re-design NumArr1 to include a warning:
  
  NumArr2 = function(n,lam=1,st=0,et=1)
  {
    arr = cumsum(rexp(n,rate=lam))
    if (arr[n]<et)
    { return("Warning! Interval end time not reached!") }
    else
    { return(length(arr[st<=arr & arr<et])) }
  }
  
  NumArr2(1000,lam=10,st=7,et=9)
  NumArr2(20,lam=10,st=7,et=9)
  NumArr2(100,lam=10,st=7,et=9)
  
# Digression: Let's add a plot of arrivals, similar to one in lecture slides:
  
  NumArr2wplot = function(n,lam=1,st=0,et=1)
  {
    arr = cumsum(rexp(n,rate=lam))
    # --------------- Plot-related code ------------------------------------------------------
    axcol="lightgray";vaxcol="gray95";iaxcol="orange";lcol="blue";pcol="blue" # coloring (irrelevant)
    y = seq(0,length(arr)); x = c(0,arr); print(round(x,2))
    xint = seq(0,max(1,ceiling(max(arr))))
    plot(x,y,type="s",col=lcol,xlab="Arrival Times", ylab="# Arrivals",axes=F)
    axis(2,pos=0,lwd=1,at=y,col=axcol)            # vert.axis: ticks at integers (counting arrivals)
    axis(1,pos=0,lwd=1,at=x,col=axcol,labels=F)   # horiz.axis: ticks at arrival times, no labels
    axis(1,pos=0,lwd=1,at=xint,col=vaxcol)        # 'integer' horiz.axis: ticks at integers
    abline(h = y, col=axcol, lty="dotted")        # horiz.grid at integers (count of arrivals)
    lines(x,y,type="s",col=lcol)                  # plot lines over horiz.grid (for emphasis)
    abline(v = x, col=vaxcol)                     # vert.grid at arrival times
    abline(v = xint, col=iaxcol)                  # 'integer' vert.grid at integers, orange lines
    points(x,y,col=pcol,pch=19,cex=1)             # 'arrival points' in cartesian system
    # --------------- End of plot-related code ------------------------------------------------
    if (arr[n]<et)
    { return("Warning! Interval end time not reached!") }
    else
    { return(length(arr[st<=arr & arr<et])) }
  }
  
  NumArr2wplot(20,lam=10,st=7,et=9)
  NumArr2wplot(100,lam=10,st=7,et=9)
  
# The real question is: How many exp() random numbers will be enough so that the
# sequence of arrivals exceeds the end time with very high probability?
# In other words: how large n should be?

# The number of arrivals in time interval [0,T] for the Poisson Arrival Process with
#    rate lambda is a Poisson discrete random variable with parameter mu = lambda times T.
#    (see lecture slide)
# Both the mean and the variance of this distribution are mu.
# In our example lambda = 10, T = et = 9, mu = 90.  
#
# We can create a rule:
# If the Poisson parameter mu is:
#   <= 20, take 2.5*mu=50 as a bound since that 1-ppois(50,mu) < 1e-8
#    > 20, take ceiling(2.5*mu) as a bound since 1-ppois(ceiling(2.5*mu),mu) < 1e-8
# This is further explained in optional Session 4+

# The improved version of the NumArr function:
#    n (length of Exp(lam) vector) is computed and is not an input argument anymore.  

  NumArr3 = function(lam=1,st=0,et=1)
  {
    n = max(50,ceiling(2.5*lam*et))
    arr = cumsum(rexp(n,rate=lam))
    if (arr[n]<et)
    { return("Warning! Interval end time not reached!") }
    else
    { return(length(arr[st<=arr & arr<et])) }
  }
  
# try it
  NumArr3(lam=10,st=7,et=9)
  
# Minor improvement of the code: The functions above simulate arrivals
# all the way until end-time et.  However the theory tells us that number of
# arrivals of a Poisson Process depends only on the interval length, and not on 
# the start nor end time.  For example, all of the following intervals have 
# the SAME distribution of arrivals since they all are of length (duration) 2:
#      [0,2], [7,9], [1000000,1000002]
# Notice that, however, our functions above would take much longer time if
# the last interval was used.  To avoid this we have:
  
  NumArr = function(lam=1,delt=1)
  {
    n = max(50,ceiling(2.5*lam*delt))
    arr = cumsum(rexp(n,rate=lam))
    if (arr[n]<delt)
    { return("Warning! Interval end time not reached!") }
    else
    { return(length(arr[arr<delt])) }
  }
  
  NumArr(lam=10,delt=2)
  NumArr(lam=10,delt=200)

