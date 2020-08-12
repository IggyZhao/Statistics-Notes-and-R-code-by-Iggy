#
#  OPIM 5603 RStudio Session 5
#  Wednesday, September 25, 2019
#

# ------- Session 4: Simulating Poisson Arrival Process -------

# Final version of NumArr function
  NumArr = function(lam=1,delt=1)
  {
    n = max(50,ceiling(2.5*lam*delt))
    arr = cumsum(rexp(n,rate=lam))
    if (arr[n]<delt)
    { return("Warning! Interval end time not reached!") }
    else
    { return(length(arr[arr<delt])) }
  }

# Execute the next line multiple times
  NumArr(10,2)     # lam=10, delt=2

# Obvious question: CAN WE REPEAT THIS 100 times without manual execution?

# This is the key to our 'simulations' with R:
#       replicate() function repeats the execution
#       of any function  a desired number of times
# Thus: replicate the line above 100 times:

  replicate(100,NumArr(10,2))
  
# Note: DO NOT CONFUSE replicate() WITH rep() ('repeat') function!
  rep(1,10)
  rep(c(-2,4,44),c(2,3,12))
  rep(c(-2,'banana',44),c(2,3,12))
  rep(100,NumArr(10,2))
  rep(NumArr(10,2),100)
  
# Back to replicating "Number of arrivals" experiment 
  lam=10; delt=2                        # lineup of parameters
  r = replicate(100,NumArr(lam,delt))
# Sort the 'number of arrivals' vector
  sort(r)
  
# what is the largest number of arrivals?
  max(replicate(100,NumArr(lam,delt)))
  max(replicate(100000,NumArr(lam,delt)))

# Recall: theory tells us that the number of Poisson Arrivals in
#         time interval of length delt has Pois(mu) distribution
#         with mu = lam*delt (= 10*2 = 20 in our example)
# This distribution is very unlikely to be larger than 2.5 times mu
#    (this is discussed in Session 4+)  
# Thus we can plot its histogram over integers 0,1,2,3,...,50
  
  hist(replicate(10000,NumArr(lam,delt)),breaks=seq(-0.5,50.5),
       col="violet",border="purple3",xlab="Number of Arrivals",
       main="Frequencies of 100 replicates of NumArr(10,2)",
       col.main="purple3",font.main=1,cex.main=1.1); grid()

  hist(replicate(100,NumArr(lam,delt)),seq(-0.5,50.5),freq=F,ylim=c(0,0.15),
       col="violet",border="purple3",xlab="Number of Arrivals",
       main="Histogram of 100 replicates of NumArr(10,2)",
       col.main="purple3",font.main=1,cex.main=1.25); grid()

  # Note: many input parameters stay the same.
  #       Here is a custom histogram plotting subroutine
  
  myhist = function(v,breaks,freq=F,ylim=NULL,col=NULL,border=NULL,xlab=NULL,main=NULL)
  {
    hist(v,breaks=breaks,freq=freq,ylim=ylim,col=col,border=border,xlab=xlab,main=main,
         col.main=border,font.main=1,cex.main=1.25); grid()
  }

  # ... and the previous command in the new version
  myhist(replicate(100,NumArr(lam,delt)),seq(-0.5,50.5),ylim=c(0,0.15),
         col="violet",border="purple3",xlab="Number of Arrivals",
         main="Histogram of 100 replicates of NumArr(10,2)")
  
# increase the number of replications ('simulations') 
  myhist(replicate(100000,NumArr(lam,delt)),seq(-0.5,50.5),ylim=c(0,0.1),
         col="violet",border="purple3",xlab="Number of Arrivals",
         main="Histogram of 100K replicates of NumArr(10,2)")

# Compare it to the Poisson prob.distr. function dpois for the rate mu=lam*delt=20
  mu = lam*delt
  trueP = dpois(seq(0,50),mu)         # vector of Pois(20) probabilites at 0,1,2,...,50
  points(seq(0,50),trueP,pch=95,cex=1.5,col="blue") # 95? ascii code for '_' (underscore)
  
# Note: it makes sense to check that sum(trueP) = ppois(50,20)
  sum(trueP); ppois(50,20) # sum to 1 
  # Be aware of rounding errors
  1-sum(trueP); 1 - ppois(50,20)
  
# try several times:
  myhist(replicate(100000,NumArr(lam,delt)),seq(-0.5,50.5),ylim=c(0,0.1),
         col="violet",border="purple3",xlab="Number of Arrivals",
         main="Histogram of 100K replicates of NumArr(10,2)")
  points(seq(0,50),trueP,pch=95,cex=1.5,col="blue") 

    
# -------------- Simulating Binomial Trials --------------

# Question: How to create "30 coin toss" experiment using the vector u?
#           I.e., create the outcome of Binomial random variable B(30,0.5)
  
  u = runif(30)
# Idea: If uniform random number is <= 0.5 generate 1, if it is > 0.5 generate 0
  (u <= 0.5)       # you can also use < instead of <=
  
# This produces Booleans TRUE and FALSE. How to convert TRUE to 1 and FALSE to 0?
  1*(u <= 0.5)
# This is the outcome of the 30 Bernoulli experiments with p=0.5.
# The total number of successes, i.e., the outcome of the B(30,0.5) random variable is:
  sum(1*(u <= 0.5)) # the count of 1 in the previous sequence - iggy 
  
# Repeat this with independent draw of 30 uniform random numbers
  u = runif(30)
  b = 1*(u <= 0.5)
  print(c(b, sum(b)))
  
# single line  (try this line multiple times)
  sum(1*(runif(30)<=0.5)) 

# Capture "the sum of the sucesses in n Binomial Trials of prob. p" in a function:
  Bin = function(n,p)
  {
    return(sum(1*(runif(n)<=p)))
  }
  
  Bin(30,0.5)
  Bin(30,0.2)# cannot be negative, cannot be larger than 30, must be intergers - iggy
  
# Replicate Bin 100 times
  n = 30
  replicate(100,Bin(n,0.5))
  
# Finally, what are the resulting frequencies (run this line multiple times)
  myhist(replicate(100,Bin(n,0.5)),seq(-0.5,30.5),freq=T,# freq=F is default in my_hist-iggy
         col="pink2",border="red3",xlab="Number of Succeses",
         main="Frequencies of 100 replicates of 30 Binomial trials")
  
# Increase the number of experiments and set freq=False (use myhist)
  myhist(replicate(100000,Bin(n,0.5)),seq(-0.5,30.5),ylim=c(0,0.25),# prob of success is 0.5
         col="pink",border="red3",xlab="Number of Succeses",
         main="Histogram of 100K simulated B(30,0.5)")
  
# What will we see for p = 0.2 and p = 0.9? # 0.2 move left, 0.9 move right 
  myhist(replicate(100000,Bin(n,0.2)),breaks=seq(-0.5,30.5),ylim=c(0,0.25),
         col="pink3",border="red3",xlab="Number of Succeses",
         main="Histogram of 100K simulated B(30,0.2)")

  myhist(replicate(100000,Bin(n,0.9)),breaks=seq(-0.5,30.5),ylim=c(0,0.25),
         col="pink3",border="red3",xlab="Number of Succeses",
         main="Histogram of 100K simulated B(30,0.9)")

# Let's fix p = 0.5
# Compare the histogram to the true B(n,p) distribution function: dbinom(seq(0,30),n,p)
  p = 0.5
  trueB = dbinom(x=seq(0,n),size=n,prob=p) # set n = 30 then 
  points(seq(0,n),trueB,pch=95,cex=2,col="blue")

# Try again, generate new B(n,p) each time:
  myhist(replicate(100000,Bin(n,p)),breaks=seq(-0.5,30.5),ylim=c(0,0.2),
         col="pink3",border="red3",xlab="Number of Succeses",
         main=paste("Histogram of 100K simulated B(",n,",",p,")",sep=""))
  points(seq(0,n),trueB,pch=95,cex=2,col="blue")
  
# Now replicate B(n,p) 'only' hundred times:
  myhist(replicate(100,Bin(n,p)),breaks=seq(-0.5,30.5),ylim=c(0,0.2),
         col="pink3",border="red3",xlab="Number of Succeses",
         main=paste("Histogram of 100 simulated B(",n,",",p,")",sep=""))
  points(seq(0,n),trueB,pch=95,cex=2,col="blue")


# -------------- Simulate rolling n fair dice --------------

# Question: how do we simulate the outcomes 1,2 3,4,5,6 on THREE fair dice?
# Start with runif(3,0,6): run it several times and observe the outcomes
  d = runif(3,0,6); d # generate three numbers between 0 and 6 - iggy
  ceiling(d) # 0.bala to 1; 2. bala to 3. just bound to next interger, so called ceiling

# Execute several times:
  ceiling(runif(3,0,6))

# Now we can have the function that simulates rolls of n "s-sided dice"
# (for the usual die s = 6) 
  RollDice=function(n,s=6)
  {
    return(ceiling(runif(n,0,s)))
  }
  
  RollDice(2)
  RollDice(12)
  RollDice(14,9)
  
# Sum of two dice rolls (outcomes are 2,3,4,5,6,7,8,9,10,11,12)
  sum(RollDice(2)) # the outcome is b/t 0 and 12 - iggy
  
# Repeat the experiment
  replicate(100,sum(RollDice(2)))
  
# Repeat 'sum of the two dice rolls' experiment 100 times and create a histogram
  myhist(replicate(100,sum(RollDice(2))),seq(1.5,12.5),ylim=c(0,0.25),
         col="yellow2",border="orange3",xlab="Sum of outcomes of two fair dice",
         main="Histogram of 100 simulated sums of two dice rolls")

# recall Quiz 1 problem: true probabilities were 11/36, 9/36, etc.
  trueS = c(1/36,2/36,3/36,4/36,5/36,6/36,5/36,4/36,3/36,2/36,1/36)
  points(seq(2,12),trueS,pch=95,cex=5,col="blue")
  
# Try 100 thousand repetitions and compare to true distribution
  myhist(replicate(100000,sum(RollDice(2))),seq(1.5,12.5),ylim=c(0,0.25),
         col="yellow2",border="orange3",xlab="Sum of outcomes of two fair dice",
         main="Histogram of 100K simulated sums of two dice rolls")
  points(seq(2,12),trueS,pch=95,cex=5,col="blue")
  
# Do the same for 'the MAXIMUM of two dice rolls' (outcomes are 1,2,3,4,5,6)
  myhist(replicate(100000,max(RollDice(2))),seq(-0.5,6.5),ylim=c(0,0.4),
         col="khaki",border="orange3",xlab="Maximum of outcomes of two fair dice",
         main="Histogram of 100K simulated maxima of two dice rolls")
  
# Compare to true probabilities
  trueM = c(1/36,3/36,5/36,7/36,9/36,11/36)
  points(seq(1,6),trueM,pch=95,cex=8,col="blue")
  


# ###################################################################################### #



# -------------- Sample Mean --------------
  
# (Representative of a) SampleMean of random vector: use the standard R function
  mean(runif(2))                    # mean of two independent unif(0,1)'s 
  
# Repeat the experiment 1000 times: we get a vector of length 1000 of the means
#    of two unif(0,1)'s, i.e., thousand representations of the SampleMean
#    random variable of two unif(0.1)'s.
  replicate(1000,mean(runif(2)))
  
# Plot the histogram
  myhist(replicate(1000,mean(runif(2))),seq(0,1,0.01),ylim=c(0,3),
         col="yellow2",border="orange3",
         main="Sample Mean of two indep. Unif(0,1): 1000 simulations")
  
# Shape of the histogram is somewhat ambiguous. Try more replicates
  myhist(replicate(100000,mean(runif(2))),seq(0,1,0.01),ylim=c(0,3),
       col="yellow2",border="orange3",
       main="Sample Mean of two indep. Unif(0,1): 100K simulations")
  
# Recall the EMPIRICAL DENSITY function (intrinsic R function)
  rv = replicate(100000,mean(runif(2)))
  myhist(rv,seq(0,1,0.01),ylim=c(0,3),
         col="yellow2",border="orange3",
         main="Sample Mean of two indep. Unif(0,1): 100K simulations")
  lines(density(rv),lwd=2,col="red")
  
# Beware: density is 'bumpy' for smaller number of repetitions
  rv = replicate(1000,mean(runif(2)))
  myhist(rv,seq(0,1,0.01),ylim=c(0,3),
         col="yellow2",border="orange3",
         main="Sample Mean of two indep. Unif(0,1): 1000 simulations")
  lines(density(rv),lwd=2,col="red")

# Histogram and density in a combined plot
  myhd = function(v,breaks,freq=F,ylim=NULL,col=NULL,border=NULL,xlab=NULL,main=NULL,
                  lwd=2,dcol="red")
  {
    myhist(v,breaks=breaks,freq=freq,ylim=ylim,col=col,border=border,xlab=xlab,main=main)
    lines(density(v),lwd=lwd,col=dcol)
  }
  
# Try it with the SampleMean of two Unif(0,1)
  myhd(replicate(100000,mean(runif(2))),seq(0,1,0.01),ylim=c(0,3),
       col="yellow2",border="orange3",
       main="Sample Mean of two indep. Unif(0,1): 100K simulations")

  myhd(replicate(1000000,mean(runif(2))),seq(0,1,0.01),ylim=c(0,3),
       col="yellow2",border="orange3",
       main="Sample Mean of two indep. Unif(0,1): 1M simulations")

# What if we start with three or more Unif(0,1)?
  myhd(replicate(100000,mean(runif(3))),seq(0,1,0.01),ylim=c(0,3),
       col="yellow2",border="orange3",
       main="Sample Mean of three indep. Unif(0,1): 100K simulations")
  
  myhd(replicate(100000,mean(runif(5))),seq(0,1,0.01),ylim=c(0,3),
       col="yellow2",border="orange3",
       main="Sample Mean of five indep. Unif(0,1): 100K simulations")

  myhd(replicate(100000,mean(runif(90))),seq(0,1,0.01),ylim=c(0,3),
       col="yellow2",border="orange3",
       main="Sample Mean of 90 indep. Unif(0,1): 100K simulations")

  # skip the ylim argument ...
  myhd(replicate(100000,mean(runif(90))),seq(0,1,0.01),
       col="yellow2",border="orange3",
       main="Sample Mean of 90 indep. Unif(0,1): 100K simulations")

  # ... and zoom in on the horizontal axis
  myhd(replicate(100000,mean(runif(90))),seq(0.3,0.7,0.005),
       col="yellow2",border="orange3",
       main="Sample Mean of 90 indep. Unif(0,1): 100K simulations")


# What if we try SampleMean of two independent normal N(0,1)?
  
# Note: Try xl = -4 and xu = 4 for SampleMean values
  myhd(replicate(100000,mean(rnorm(2))),seq(-4,4,0.1),
       col="yellow2",border="orange3",
       main="Sample Mean of 2 indep. N(0,1): 100K simulations")
  
# The histogram and density seem Bell-Curve-shaped
# So we want to compare them to a pdf of a normal N(SMm,SMsd^2)!
#    But for which mean SMm and variance SMsd^2 ?
#    Notation: SMm stands for SampleMean mean and
#             SMsd for SampleMean standard deviation.
# Try with SMm=0 and couple of trials for standard deviation:
  x=seq(-4,4,0.01)                                        
  lines(x,y=dnorm(x,mean=0,sd=1),lwd=3,col="magenta")         # variance = 1
  lines(x,y=dnorm(x,mean=0,sd=sqrt(2)),lwd=3,col="green2")    # variance = 2
  lines(x,y=dnorm(x,mean=0,sd=sqrt(1/2)),lwd=3,col="blue")    # variance = 1/2
  # Therefore variance = 1/2 seems to produce the right fit
  
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

# Rewritten: Whaestimated x-axis limits xl and xu for SampleMeat if we start with normal N(0,1) random numbers?
  
  N = 100000                # the number of replicates
  n = 2                     # sample size
  mu = 0; sigm = 1          # population mean and population standard deviation
  SMm = mu                  # expected value of SampleMean (= population mean)
  SMsd = sigm/sqrt(n)       # standard deviation of SampleMean
  xl = -6; xu = 6; nb=100   # n values

  myhd(replicate(N,mean(rnorm(n,mean=mu,sd=sigm))),seq(xl,xu,(xu-xl)/nb),
       col="yellow2",border="orange3",
       main=paste("Sample Mean of",n,"N(0,1):",N/1000,"K rep"))
  
  x=seq(xl,xu,0.1*(xu-xl)/nb)    # increase 10x resolution on the x-axis for pdf plot
  lines(x,y=dnorm(x,mean=SMm,sd=SMsd),lwd=3,col="blue")

# Recap: This section compares the histogram and empirical density of the
#        simulation obtained SampleMean of normal N(mu,sigm^2) sample to 
#        to the ('theoretical') pdf of Normal N(mu,sigm^2/n) distribution.
#        This is done to 'test' the statement on the lecture slide 2.11

# We would like to re-use the code above for different random number generators:

  SMcomp = function(N,v,mu,sigm,xl,xu,nb=100,dname="rand var's")
  {
    n = length(v)
    SMm = mu; SMsd = sigm/sqrt(n)
    myhd(replicate(N,mean(v)),seq(xl,xu,(xu-xl)/nb),
         col="yellow2",border="orange3",
         main=paste("Sample Mean of ",n," ",dname,": ",N/1000,"K rep",sep=""))
    x=seq(xl,xu,0.1*(xu-xl)/nb)
    lines(x,y=dnorm(x,mean=SMm,sd=SMsd),lwd=3,col="blue")
  }
  
  SMcomp(N,rnorm(n,mu,sigm),mu,sigm,xl,xu,dname="N(0,1)")
  
# What went wrong?
#   In order for 'replicate' to work we cannot pass a CONSTANT VECTOR v.
#   Instead for each replicate new random vector rnorm(n,mean,sd) must be generated.
#
# But we do not want to hard-code random number generator rnorm() inside the
#   procedure since we want to use it for random number generation from other
#   distributions, like runif, rexp, rt, etc.
#  
# This can be accomplished in R via a "do.call()" function that takes any function
#   and a LIST of its arguments and executes it:

  #Example 1:
  log(3)                        # natural logarithm function ln(3) ~ 1.1
  do.call(log,list(3))

  #Example 2:
  sum(3,5)
  do.call(sum,list(3,5))

  #Example 3:
  mysum = function(a,b,c)
  {
    return(a*b+c)
  }
  
  mysum(4,7,3)
  do.call(mysum,list(4,7,3))
  do.call(mysum,list(4,7))      # error: list of arguments not following definition

  #Example 4:
  runif(20,min=1,max=6)
  do.call(runif,list(20,1,6))   # is the same as runif(20,min=1,max=6)

  fn = runif; arglist = list(20,1,6)
  do.call(fn,arglist)

  fn = runif; arglist = list(20)
  do.call(fn,arglist)
  
# Agreement: ALWAYS specify the sample size n as THE FIRST element
#            of the list of arguments: n = arglist[[1]]
# Now we can write a reusable procedure:

  mySMcomp = function(N,fn,arglist,mu,sigm,xl,xu,nb=100,dname="rand var's")
  {
    n = arglist[[1]]
    SMm = mu; SMsd = sigm/sqrt(n)
    myhd(replicate(N,mean(do.call(fn,arglist))),seq(xl,xu,(xu-xl)/nb),
         col="yellow2",border="orange3",
         main=paste("Sample Mean of ",n," ",dname,": ",N/1000,"K rep",sep=""))
    x=seq(xl,xu,0.1*(xu-xl)/nb)
    lines(x,y=dnorm(x,mean=SMm,sd=SMsd),lwd=3,col="blue")
  }

# and try it on the SampleMean of two independent Normal N(0,1):
  mySMcomp(N,rnorm,list(2,0,1),mu=0,sigm=1,xl=-6,xu=6,dname="N(0,1)")
  
# Apply the procedure to independent Uniforms
  min = 0; max = 1
  mu = (min+max)/2               # e.g., Unif(0,1) population mean is 0.5
  sigm = sqrt((max-min)^2/12)    # e.g., Unif(0,1) population variance 1/12
  xl = min; xu = max             # Sample Mean values fall in this interval
  dn = paste("Unif(",min,",",max,")",sep="")

  # Two independent Uniforms; n = 2
  mySMcomp(N,runif,list(2,min,max),mu,sigm,xl,xu,dname=dn)

  # Try n=3, n=5, n=17, n=250.  What do you observe?
  mySMcomp(N,runif,list(3,min,max),mu,sigm,xl,xu,dname=dn)
  mySMcomp(N,runif,list(5,min,max),mu,sigm,xl,xu,dname=dn)
  mySMcomp(N,runif,list(17,min,max),mu,sigm,xl,xu,dname=dn)
  mySMcomp(N,runif,list(250,min,max),mu,sigm,xl,xu,dname=dn)
  mySMcomp(N,runif,list(250,min,max),mu,sigm,0.4,0.6,dname=dn) # zoom-in the x-axis
  
# Apply the procedure to independent Exp(1)'s
  lambda = 1
  mu = 1/lambda     # Exp(lambda) population mean
  sigm = 1/lambda   # Exp(lambda) population standard deviation
  xl = -1; xu = 9   # Sample Mean is positive; it can be large for two indep.Exp(1)'s
  dn = paste("Exp(",lambda,")",sep=""); dn
  
  # Two independent Exponentials; n = 2
  mySMcomp(N,rexp,list(2,lambda),mu,sigm,xl,xu,dname=dn)

  # Try n=3, n=5, n=17, n=250.  What do you observe?
  mySMcomp(N,rexp,list(3,lambda),mu,sigm,xl,xu,dname=dn)
  mySMcomp(N,rexp,list(5,lambda),mu,sigm,xl,xu,dname=dn)
  mySMcomp(N,rexp,list(17,lambda),mu,sigm,xl,xu,dname=dn)
  mySMcomp(N,rexp,list(17,lambda),mu,sigm,0,2.5,dname=dn)      # zoom-in the x-axis
  mySMcomp(N,rexp,list(250,lambda),mu,sigm,0.5,1.5,dname=dn)
  
# Apply the procedure to independent Student t(4)'s
  df = 4
  mu = 0                 # t(df) population mean
  sigm = sqrt(df/(df-2)) # t(df) population standard deviation (df >2)
  xl = -25; xu = 25      # estimated x-axis limits xl and xu for SampleMean values
  dn = paste("t(",df,")",sep=""); dn

  # Two independent t(4)'s; n = 2
  mySMcomp(N,rt,list(2,df),mu,sigm,xl,xu,dname=dn)
  # Try n=3, n=5, n=17, n=250.  What do you observe?
  mySMcomp(N,rt,list(3,df),mu,sigm,xl,xu,dname=dn)
  mySMcomp(N,rt,list(5,df),mu,sigm,-10,10,dname=dn)   # zoom-in the x-axis
  mySMcomp(N,rt,list(17,df),mu,sigm,-10,10,dname=dn)
  mySMcomp(N,rt,list(250,df),mu,sigm,-2,2,dname=dn)   # zoom-in the x-axis
