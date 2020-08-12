
#____________________________________________________FUNCTION MODELS _________________________________________________
# histogram & lines
hist(rnorm(1000000),breaks=seq(-6,6,0.12),freq=F,ylim=c(0,0.5),
     col="orange",border="brown",xlab="",
     main="1M N(0,1) random numbers, 100 bins",
     col.main="brown",font.main=1,cex.main=1.25); grid()
lines(seq(-6,6,0.012),dnorm(seq(-6,6,0.012)),col="blue",lwd=2)  

#-------------------------------
myhist = function(v,breaks,freq=F,ylim=NULL,xlab=NULL,main=NULL,
                  col="yellow",border="orange2")
{
  hist(v,breaks=breaks,freq=freq,ylim=ylim,xlab=xlab,main=main,col=col,border=border,
       col.main=border,font.main=1,cex.main=1.25); grid()
}

#--------------------------------
myhd = function(v,breaks,ylim=NULL,xlab=NULL,main=NULL,
                col="lightyellow",border="orange2",lwd=2,dcol="red")
{
  myhist(v,breaks=breaks,freq=F,ylim=ylim,col=col,border=border,xlab=xlab,main=main)
  lines(density(v),lwd=lwd,col=dcol)
}

# Try it with the SampleMean of two Unif(0,1)
myhd(replicate(100000,mean(runif(2))),seq(0,1,0.01),ylim=c(0,3),
     col="yellow2",border="orange3",
     main="Sample Mean of two indep. Unif(0,1): 100K simulations")

#____________________________________________________Quiz 1_________________________________________________
#Write an R-script that uses the R-function read.csv to load the frequency table from the comma separated
#value file named Coins.csv into a data-frame named coins. Use this data-frame to calculate the expected value and
#the variance of X.

# read the data file that contains columns x and Freq
coins = read.csv("Coins.csv")
# Assuming headers (column names) x and Freq, compute
# the probability distribution vector from the frequencies
x = coins$x
f = coins$freq
p = f/sum(f)
# expected value
m = weighted.mean(x,p) # or m = weighted.mean(x,f)
# variance
var = sum(((x-m)^2)*p)
#Note: the expected value E(X) = 0.21 , and the variance Var(X) = 0.0304.

#____________________________________________________Quiz 2_________________________________________________

#Problem 4: (10 pts each) A fair die is rolled 700 times. Rolling a six is considered a success. 
#Write an R command that computes the probability of:
#(a) no successes at all,
dbinom(0,size=700,prob=1/6)
#(b) exactly 100 successes,
dbinom(100, 700, 1/6)
#(c) at most 100 successes ('at most 100' means 100 or less),
sum(dbinom(seq(0,100), 700, 1/6))
pbinom(100,700,1/6)
#(d) at least 120 successes ('at least 120' means 120 or more),
sum(dbinom(seq(120,700), 700, 1/6))
1-pbinom(119,700,1/6)
#(e) between 100 and 120 successes (including 100 and 120).
sum(dbinom(seq(100,120), 700, 1/6))
pbinom(120,700,1/6)-pbinom(99,700,1/6)


#____________________________________________________Quiz 5 [IMPORTANT]________________________________________________

# Problem 6: (7 pts each) Arrival of vehicles at New Jersey Turnpike toll booth
#            on Saturdays at dawn is modeled as a Poisson Arrival process with
#            a rate of 1.21 vehicles per minute. Let X be a random variable
#            that records the number of arrivals between 4:45AM and 5:15AM.
#
# (a) Simulate X via the NumArr() function provided in Session 5:
#     write an R code that replicates NumArr (with appropriately chosen input
#     arguments) fifty thousand times and create a histogram of the resulting
#     vector via standard hist() function.

NumArr = function(lam=1,delt=1)
{
  n = max(50,ceiling(2.5*lam*delt))
  arr = cumsum(rexp(n,rate=lam))
  if (arr[n]<delt)
  { return("Warning! Interval end time not reached!") }
  else
  { return(length(arr[arr<delt])) }
}

# Note: The rate of customer arrivals per MINUTE is lam = 1.21.
#       The length of time interval in MINUTES is delt = 30 minutes.

r = replicate(50000,NumArr(1.21,30))
hist(r, breaks=seq(-0.5,80.5), freq=F, col="lightblue", border="gray50", main="")
grid()

#--------------------------------------- MY ANSWER
hist(replicate(50000,NumArr(1.21,30)),seq(-0.5,80.5),freq=F,ylim=c(0,0.08),
     col="violet",border="purple3",
     main="Histogram of 50k replicates of NumArr(1.21,30)",
     col.main="purple3",font.main=1,cex.main=1.1); grid()


# (b) Plot over the histogram in part (a) the true probability distribution of X.
x = seq(0,75); y = dpois(x,36.3)
points(x,y,pch=95,cex=1.5,col="blue")

#--------------------------------------- MY ANSWER
mu = 36.3
trueP = dpois(seq(0,91),mu)       
points(seq(0,91),trueP,pch=95,cex=1.5,col="red")


# Problem 7: (7 pts each)
# (a) Write an R function rnormmax() that has three arguments, n, mu, and sigm,
#     and returns the maximum of a vector of n random numbers from the normal
#     distribution with mean mu and standard deviation sigm. Make the arguments
#     mu and sigm optional with default values of 0 and 1, respectively.

rnormmax = function(n,mu=0,sigm=1)
{
  return(max(rnorm(n,mu,sigm)))
}

# (b) Write an R code that replicates rnormmax(n=1000) hundred thousand times
#     and saves it to a vector v.  Plot a histogram of vector v via standard
#     hist() function.

v = replicate(100000,rnormmax(1000))
hist(v, breaks=seq(1,7,0.05), prob=T, col="lightyellow", border="orange2", main="")
grid()

#--------------------------------------- MY ANSWER
v <- replicate(100000, rnormmax(1000))
hist(v,seq(1,7,0.06),freq=F,
     col="violet",border="purple3",
     main="Histogram of vector v",
     col.main="purple3",font.main=1,cex.main=1.25); grid()

# (c) Write an R code that plots the empirical density of a vector v over
#     a histogram obtained in part (b).

lines(density(v),lwd=2,col="red")

# or
myhd(v,seq(1,7,0.06),main="Histogram of vector v")

# Problem 8: (7 pts each)
# (a) Write an R function psm(n,mu) that returns the mean of a vector of n
#     random numbers from Poisson distribution with parameter mu.

psm = function(n,mu)
{
  return(mean(rpois(n,mu)))
}

# (b) Write an R code that replicates psm(350, 20.4) hundred thousand times and
#     creates a histogram of the resulting vector via standard hist() function.

r = replicate(100000,psm(350,20.4))
hist(r, breaks=seq(19,22,0.05), prob=T, col="lightgreen", border="green2", main="")  
grid()  


#--------------------------------------- MY ANSWER
hist(replicate(100000, psm(350, 20.4)),seq(19,22,0.04),freq=F,ylim=c(0,2),
     col="violet",border="purple3",
     main="Histogram of replicating psm(350, 20.4) 100k times",
     col.main="purple3",font.main=1,cex.main=1); grid()

# or
myhist(r,main="Histogram of replicateing psm(350, 20.4)")
myhd(r,main="Histogram of replicates psm(350, 20.4)")
#____________________________________________________Quiz 6________________________________________________
# Problem 2: (10 pts each) 
#
# (a) Write an R function minsumexp () that has two arguments, n and lam, and returns
#     the minimum of a sum of two independent random vectors of length n generated from
#     the exponential distribution with rate lam.
#     Make the argument lam optional with default value of 1.

minsumexp = function(n,lam=1)
{
  return(min(rexp(n,lam)+rexp(n,lam)))
}

# (b) Write an R code that replicates minsumexp(n=40) hundred thousand times and creates
#     a histogram of the resulting vector via standard hist() function.

v <- replicate(100000,minsumexp(40))
hist(v,seq(0,1.05,0.01),freq=F,
     col="violet",border="purple3",
     main="Histogram of 100k replicates of minsumexp(n=40)",
     col.main="purple3",font.main=1,cex.main=1.1); grid()

# Problem 3:  Let X1, X2, ... , X25  be a sample from Exp(10) distribution. 
#
# (a)(5 pts) What is the expected value of its sample variance?
# Answer:
# The lam=10, the population vairance sigma^2 is 1/(lam)^2 = 1/100 = 0.01
# According to E(S^2) = sigma^2,
# the expected value of its sample variance is also 0.01


# (b)(10 pts) Write an R code that generates hundred thousand repetitions of the sample
#     variance and create a histogram of the resulting vector via standard hist() function.

z <- replicate(100000,var(rexp(25,10)))
hist(z,seq(0,0.15,0.0015),freq=F,
     col="violet",border="purple3",
     main="Histogram of 100k replicates of sample variance",
     col.main="purple3",font.main=1,cex.main=1.1); grid(nx=0,ny=NULL)


# (c)(5 pts) Use the density() function to get a sample variance empirical pdf
#     and add it to the plot obtained in part (b).
lines(density(z),lwd=3,col="red")


# Problem 4: (10 pts each) Write an R code that: 
#
# (a) generates hundred thousand repetitions of the sample mean of the sample of size 4
#     from the binomial distribution B(44,0.38) and create a histogram of
#     the resulting vector via standard hist() function.

x = seq(9+1/8, 25-1/8, 1/4) # set break 1/4 can see quarterly scenarios
w <- replicate(100000,mean(rbinom(4,44,0.38)))
hist(w, freq=F,breaks=x,
     col="lightpink1",border="mediumorchid3",
     main="Histogram of 100k replicates of sample mean",
     col.main="mediumorchid3",font.main=1,cex.main=1.1); grid()

#answer
N = 100000; n = 4
size = 44; prob = 0.38; x = seq(9 + 1/8,25 - 1/8, 1/4)
hist(replicate(N,mean(rbinom(n,size,prob))),breaks=x,freq=F,col="lightgreen",
     border='green4'); grid()

# (b) Add to the plot in part (a) a graph of the normal distribution pdf and see how it
#     compares to the histogram. Specify what the parameters of this normal distribution are.

popmean = size*prob; popvar = size*prob*(1-prob)
SMm = popmean; SMsd = sqrt(popvar/n)
xx = seq(9,25,0.001)
lines(xx,dnorm(xx,mean=SMm,sd=SMsd),lwd=2,col="blue")

#-------------------------------------MY ANSWER
x=seq(10,25,l=1001) 
samplesize=4
n=44
p=0.38
q=0.62 
SMm = n*p   
SMsd = sqrt(n*p*q/samplesize)

lines(x,y=dnorm(x,mean=SMm,sd=SMsd),lwd=3,col="purple")

# The histogram has a same shape with normal distribution.
# the normal distribution: N~(16.72,2.5916)
# With parameters mean is n*p = 16.72; variance is n*p*q/samplesize = 2.5916
# and the standard deviation is 1.6098.

# Problem 5: (5 pts each)
#   Quiz6 Data.csv contains the person ID and the height (in centimeters) of the male
#   population sampled. Use read.csv() function to load the file into a dataframe named
#   'data' and write R commands/functions that provide solutions to all the parts below.

# (a) Calculate your best estimate for the population mean.

data = read.csv("Quiz6_Data.csv")
# The population mean is unknown and the best estimate (unbiased estimator) for it is the sample mean.
mean(data$Height)

# (b) Calculate your best estimate for the population variance.

var(data$Height)
# Sample variance is the unbiased estimator of population mean. 


# (c) Create the histogram of the height data by using the hist() function.
hist(data$Height,seq(120,220,1),freq = F,ylim = c(0,0.04),
     col="lightpink1",border="mediumorchid3",
     main="Histogram of height data",
     col.main="mediumorchid3",font.main=1,cex.main=1.1); grid()

# (d) Add to the plot in part (c) a graph of the normal distribution pdf and see how it
#     compares to the histogram. Specify what the parameters of this normal distribution are.

x=seq(120,220,l=101)    
mu =  mean(data$Height)           
sigm = sd(data$Height)  
lines(x,y=dnorm(x,mean=mu,sd=sigm),type="s",lwd=3,col="purple")

# The normal distribution has the same shape with histogram.
# The normal distribution is N~(169.86, 148.72), with parameters:
# mu = 169.86 and variance = 148.72.

#_______________________________________________________MOCK EXAM__________________________________________________

# Problem 1:  Load the library MASS.  Write an R command that: 
library(MASS)
# (a) allows you to view the dataframe called Boston,
View(Boston)

# (b) plots either the histogram or (empirical) density of the age column (contains property age),
hist(Boston$age,col="yellow")
hist(Boston$age,prob=T,col="orange")
plot(density(Boston$age),col="blue",lwd=2)

# optional: you can also load functions from Session 6 (or 6+) and try
myhd(Boston$age)
summary(Boston$age)
myhd(Boston$age,seq(0,100,2.5),main="Boston data")

# (c) calculates the sample mean of the ages,
mean(Boston$age)

# (d) calculates the sample variance of the ages,
var(Boston$age)

# (e) plots the scatter-plot of the weighted distances (dis column) against the age, # y against x
plot(Boston$age,Boston$dis,col="blue",cex=0.5) # right

# (f) extracts the vector of ages, but only for units that are at least 50 years old.
Boston$age[Boston$age>=50]

# ------------------------------------------------------------------------------------------------------------

# Problem 2: Load the data from wages.csv into an R data-frame named w .
w = read.csv("wages.csv")

#  Write an R command that:
#  
# (a) allows you to view the data (either properties/headers or the entire data),
View(w)

# (b) plots either the histogram or (empirical) density of the column that contains employee wages,
hist(w$Wage, col="yellow")
plot(density(w$Wage), col="blue")

# optional:
summary(w$Wage)
myhd(w$Wage,seq(0,100000,2500),main="Wage data")

# (c) calculates the sample mean of the employee wages,
mean(w$Wage)

# (d) calculates the sample variance of the employee wages,
var(w$Wage)

# (e) plots the scatter-plot of employee years-of-experience against employee age,
plot(w$Age,w$YrsExper,pch=19,col="red")

# (f) extracts the vector of employee wages, but only for employees with 20 or more years-of-experience.
w$Wage[w$YrsExper>=20]

# ------------------------------------------------------------------------------------------------------------

# Problem 3:
# (a) Write an R function dubbed DiceSpan with two input parameters, n and s,
#     with the default value for s is 6 (6-sided dice).  The function must return the difference
#     between the maximum and the minimum of the outcomes of n independent dice rolls.

DiceSpan= function(n,s=6)
{
  nRoll = ceiling(runif(n,min=0,max=s))
  return(max(nRoll)-min(nRoll))
}

# (b) List all possible outcomes of DiceSpan(n, 6).    Hint: it is the same for all n >1.
# All possible outcomes are 0,1,2,3,4,and 5 

# (c) Write an R command that creates the vector v of ten thousand repetitions of the function DiceSpan for n = 3.
v = replicate(10000,DiceSpan(3))

# (d) Write an R command that plots the histogram of v.  Hint: use (b).
# by (b) the outcomes are 0,1,2,3,4,5, so histogram breaks should be -0.5,0.5,1.5,2.5,3.5,4.5,5.5
hist(v, breaks=seq(-0.5,5.5,1), col="yellow")

# (e) Write an R function dubbed avgDiceSpan with three input parameters: m, and n and s as in (a).
#     The function must return the average of the m repeated outcomes of DiceSpan(n, s).
avgDiceSpan = function(m,n,s=6)
{
  return(mean(replicate(m,DiceSpan(n,s))))
}

# (f) List all possible outcomes of avgDiceSpan(m = 10, n, s =6).    Hint: it is the same for all n >1.
# For m = 10 we are averaging 10 numbers from the set {0,1,2,3,4,5}.  The outcomes are
#     0, 0.1, 0.2, 0.3, ... ,  4.8, 4.9, 5   or  seq(0,5,0.1)"
avgDiceSpan(10,2,6)# out!-iggy

# (g) Write an R command that creates the vector w of twenty thousand repetitions of the function avgDiceSpan for
#  m = 10 and n = 2.
w = replicate(20000,avgDiceSpan(10,2,6))

# (h) Write an R command that plots the histogram of w. 
# by (f) the outcomes are 0,0.1,0.2,...,4.9,5, so histogram breaks should be -0.05, 0.05, 0.15,0.25, ..., 4.95,5.05"
hist(w, breaks=seq(-0.05,5.05,0.1), col="yellow")

# ------------------------------------------------------------------------------------------------------------

# Problem 4:
#
# (a) Write an R function tsm(n,d) that returns the mean of a vector of n random numbers from the
#     Student t distribution with d degrees of freedom.
tsm = function(n,d)
{
  return(mean(rt(n,d)))
}

# (b) Write an R code that replicates tsm(300, 3) fifty thousand times and creates a histogram of
#     the resulting vector via standard hist function.
#     (Note: experiment with various choices of breaks until you find a good choice)
hist(replicate(50000,tsm(300,3)), breaks=seq(-2,2,0.05), col="yellow")

# ------------------------------------------------------------------------------------------------------------

# Problem 5: Write:
#(a) an R function SumRoots with one input parameter n.  The function must return
#    the sum of the square roots of the outcomes of n independent fair dice rolls.

SumRoots = function(n)
{
  rolls = ceiling(runif(n,0,6))
  sqrts = sqrt(rolls)
  sumsqrts= sum(sqrts)
  return(sumsqrts)
}

# One-liner:
SumRoots = function(n)
{
  return(sum(sqrt(ceiling(runif(n,0,6)))))
}

#(b) an R command that creates the vector v of fifty thousand repetitions of the
#    function SumRoots for n = 16.
v = replicate(50000,SumRoots(16))

#(c) an R command that plots the histogram of v via the standard hist function.
#    (break points must be reasonably chosen; experiment with various choices of
#     breaks until you find a good choice).

min(v); max(v)
hist(v,breaks=seq(18,40,0.2),prob=T,col="yellow2",border="gray60"); grid()

#(d) an R code that plots over the histogram in part (c) the pdf of
#    a normal distribution with parameters mean and sd calculated from
#    the vector in part (c).
SM = mean(v)
SV = var(v)
x = seq(18,40,0.1)
lines(x, dnorm(x,SM,sqrt(SV)),lwd=3,col="blue")

#_______________________________________________________lecture 1 __________________________________________________
# Setting the desired working directory
setwd("c:/Users/Iggy Zhao/Desktop/")

# Importing the dataset
ccdata <- read.csv("c:/Users/Iggy Zhao/Desktop/Data01/CCData.csv") 

# --------------------------------------------------------------------- (rudimentary) Data Visualisation -----------------------------------------
# View the data
View(ccdata)

# Extracting the columns of table (matrix)
ccdata$Cards     
ccdata$Freq    

# Simple plot 
plot(ccdata,col="blue")
plot(ccdata,pch=19,cex=2,col="blue")
plot(ccdata,pch=65,cex=1,col="red")    # ASCII code 65 is the capital letter A
plot(ccdata,type="s",col="blue") # Plotting a quasi-histogram
plot(ccdata,type="h",col="blue",lwd=50)# Plotting almost-a-histogram
barplot(ccdata$Freq,col="blue",names.arg = ccdata$Cards)# Histogram via barplot function 
pie(ccdata$Freq,labels=ccdata$Cards,col = c("red","orange","yellow","green","blue","violet","grey"))# Plotting a pie chart

# ------------------------------------------------------------------------- Average of sequence of numbers --------------------------------------------

u = rep(c(1,2,3.14159),c(4,1,7));u # rep repeates the entries of the first 'vector' number of times specified by second vector
v = rep(ccdata$Cards,ccdata$Freq); v # Expand the card usage frequency to individual's card count (150 observations)

mean(v)
# Histogram of this data vector (search for "hist" function in the Help tab)
hist(v,breaks=seq(-0.5,6.5,1),col="orange",freq=FALSE,ylim=c(0,0.4)); grid()

# ---------------------------------------------------------------------- Expected Value of a discrete r.v. -----------------------------------------

weighted.mean(x,prob)   # expected value of a discrete r.v. is a weighted mean:
weighted.mean(x,freq)
# Note: the expected value of the random variable C is the same as the mean of 150 numbers above

#_________________________________________________lecture 2 ________________________________________________________________
x = ccdata$Cards; x
freq = ccdata$Freq; freq
prob = freq/sum(freq); prob
# ------------------------------------------------------------------------- Variance of a discrete r.v. -------------------------------------------------
mu = weighted.mean(x,prob); mu
weighted.mean(((x-mu)^2),prob) # Var(X) = E((X-mu)^2),
weighted.mean(((x-mu)^2),freq) # same
pdvar = function(x,prob)
{
  mu = weighted.mean(x,prob)
  return(weighted.mean((x-mu)^2,prob))
}
pdvar(x,prob) # variance
pdvar(x,freq)
sqrt(pdvar(x,freq)) # standard deviation

# -------------------------------------------------------------------- Binomial distribution B(n,p) in R -----------------------------------------------

# Probability of exactly 6 successes in n=8 Binomial trials with success prob p=0.82:
# dbinom is the probability density of a binomial random variable  
dbinom(6,8,0.82)

# Probability of AT LEAST 6 successes in n=8 Binomial trials with success prob p=0.82:
dbinom(6,8,0.82) + dbinom(7,8,0.82) + dbinom(8,8,0.82)
1-pbinom(5,8,0.82)
pbinom(5,8,0.82,lower.tail = F)

# Get all the probabilities for the binomial random variable B(8,0.82):
k = seq(0,8)                # values B = B(8,0.82) assumes are 0,1,...,7,8
prob = dbinom(k,8,0.82)     # vector of probabilities P(B=0), P(B=1),..., P(B=7), P(B=8)
sum(prob)

#  probability distribution table
pdtable = data.frame(k,prob)
colnames(pdtable) = c("k","P(B=k)")
View(pdtable)

# Nice way to visualize: histogram with heights equal to the probabilities
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

# Probability distribution table
pdtable = data.frame(k,prob)
colnames(pdtable) = c("k","P(B=k)")
options(digits=16)
View(pdtable)

# We should depict P(B=k) only for k = 610,..,700
k1 = seq(610,702)
prob1 = dbinom(k1,800,0.82)
barplot(prob1,names.arg=k1, col="orange", ylim=c(0,0.04), border=NA); grid()

# what if we increase n? 
k2 = seq(6400,6720)
barplot(dbinom(k2,8000,0.82),names.arg=k2, col="orange", ylim=c(0,0.013), border=NA); grid()

# ---------------------------------------------- Normal distribution in R: dnorm, pnorm, qnorm functions ---------------------------------
pnorm(0);pnorm(-1); pnorm(1); pnorm(1)-pnorm(-1)     # bell-curve mass within one standard deviation

# Plot the standard normal pdf:
x = seq(-4,4,0.01)
v = dnorm(x)
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
rnorm(20,mean=0,sd=1); rnorm(20)      

# School rash example: compute  P(X<3) where X ~ N(6,1.5^2)
pnorm(3,mean=6,sd=1.5)
# transformation to standard normal Z = (X-mu)/sigma
pnorm(-2)

#_________________________________________________lecture 3 ________________________________________________________________

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
2*pnorm(-1)  # Alternative calculation, using the symmetry of N(0,1)
1-(pnorm(2)-pnorm(-2))    #   Two sigma: roughly 4.5%
1-(pnorm(3)-pnorm(-3))    # Three sigma: roughly 3 in 1000

# ------------------------------------------------------------- Back to Binomial distribution B(n,p) in R ----------------------------------
# probability of AT MOST 600 sucesses in B(800,0.82):
sum(dbinom(seq(0,600),800,0.82))
pbinom(600,size=800,prob=0.82)# this can easily be computed via binomial function CDF:

# How do we use CDF to compute probability of AT LEAST 650 sucesses?
# The complementary event of "AT LEAST 650 successes in 800 trials"
#                          is "AT MOST 649 successes in 800 trials"

pbinom(649,size=800,prob=0.82)    # prob of AT MOST 649 successes in 800 trials
1-pbinom(649,size=800,prob=0.82)  # prob of AT LEAST 650 successes in 800 trials
pbinom(649,800,0.82,lower.tail=FALSE)

# Generating binomial random numbers: vector of 7 r.n. from B(800,0.82)
rbinom(7,800,0.82)

# --------------------------------------------------------------- Exploring the Student t(n) distribution ----------------------------------------
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
min(v); max(v); max(v)-min(v)           # notice how NARROW the range is

# Same question for the Student t(1) distr:
v = rt(10000,df=1)
min(v); max(v); max(v)-min(v)           # notice how WIDE the range is

# Question: of these 10000 random numbers how many are outside [-100,100]?##### extract a part of vector

length(v[v > 100])
length(v[v < -100])
length(v[v < -100 | v > 100])
length(v[v>-100 & v<100])               # how many are inside [-100,100]?

#  there is a large probability that t(1) will produce the value
#      less than -100 or more than 100. We can compute that probability:
pt(-100,1) + 1 - pt(100,1)      # approx. 0.637% probability
# Hence when we generate 10000 random numbers from t(1) roughly 64 numbers will be outside [-100,100] interval.


# plot the PDF of t distribution 
hist(v,1000000,xlim=c(-10,10),ylim=c(0,0.4),col="orange",prob=T); grid()
# now compare the histogram to the standard normal pdf & the histogram to the t(1) pdf
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

# try df=5,less bins
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

# ------------------------------------------------------------------ Briefly on Exponential distribution -------------------------------------
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
1-pexp(10,rate=1)  

v = rexp(10000)
min(v);max(v); max(v)-min(v)
hist(v,100,
     col="lightblue",border="blue2",xlab="",
     main="10K Exp(1) random numbers, 100 bins",
     col.main="blue2",font.main=1,cex.main=1.25); grid()

# On 'breaks': how likely it is that an Exp(1) random number is larger than 12?  
1-pexp(12)     # ~ 6 in a million
hist(rexp(10000),seq(0,12,0.1),col="orange",prob=TRUE,ylim=c(0,1)); grid()
lines(x,dexp(x),lwd=2,col="blue")

# What does the following produce  
v = pexp(rexp(10000))
# see min max
min(v); max(v)
# histogram it on your own  
hist(v,col="orange",prob=TRUE,ylim=c(0,1))

#_________________________________________________lecture 4 ________________________________________________________________
Weight = read.csv("Data04/BirthWeight.csv")
View(Weight)
# a quick way to see the quartiles and the average
summary(Weight)
# extract the 'vector' of weights (true data)
w = Weight$kg
min(w); max(w); mean(w)
length(w)
# Empirical Density function in R:
lines(density(w),lwd=3,col="blue")

# Example 2
#   The rate at which the low energy (around 1 GeV) cosmic ray particles
#   arrive at the top of the atmosphere is about one per square centimeter
#   per second.  Rays.csv contains the inter-arrival times of low-energy
#   cosmic ray particles hitting a space station plate of area 1 sqcm
#   during the period of roughly 36 hours.  How is the data distributed?

Rays = read.csv("Data04/Rays.csv")
View(Rays)

summary(Rays)

dt = Rays$deltaT
length(dt)

hist(dt,breaks = seq(0,11,0.05),freq=F,
     col="green2",border="green3",xlab="seconds",
     main="Inter-arrivals of low-energy particles",
     col.main="green4",font.main=1,cex.main=1.25); grid()

densdt = density(dt)
lines(densdt,lwd=3,col="blue")

# ------------------------------------------------ Random numbers from normal distribution and histograms ----------------------------------
rnorm(20)
hist(rnorm(1000000),breaks=seq(-6,6,0.12),freq=F,ylim=c(0,0.5),
     col="orange",border="brown",xlab="",
     main="1M N(0,1) random numbers, 100 bins",
     col.main="brown",font.main=1,cex.main=1.25); grid()
lines(seq(-6,6,0.012),dnorm(seq(-6,6,0.012)),col="blue",lwd=2)  

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
# True (sort of): rnorm() = qnorm(runif())

# ------------------------------------------------------------- Simulating Poisson Arrival Process --------------
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

NumArr(lam=10,delt=2)
NumArr(lam=10,delt=200)

#_________________________________________________lecture 5 ________________________________________________________________
# replicate() function repeats the execution of any function  a desired number of times
# Note: DO NOT CONFUSE replicate() WITH rep() ('repeat') function!

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

myhist = function(v,breaks,freq=F,ylim=NULL,col=NULL,border=NULL,xlab=NULL,main=NULL)
{
  hist(v,breaks=breaks,freq=freq,ylim=ylim,col=col,border=border,xlab=xlab,main=main,
       col.main=border,font.main=1,cex.main=1.25); grid()
}

myhist(replicate(100000,NumArr(lam,delt)),seq(-0.5,50.5),ylim=c(0,0.1),
       col="violet",border="purple3",xlab="Number of Arrivals",
       main="Histogram of 100K replicates of NumArr(10,2)")

# Compare it to the Poisson prob.distr. function dpois for the rate mu=lam*delt=20
mu = lam*delt
trueP = dpois(seq(0,50),mu)         # vector of Pois(20) probabilites at 0,1,2,...,50
points(seq(0,50),trueP,pch=95,cex=1.5,col="blue") # 95? ascii code for '_' (underscore)

# try several times:
myhist(replicate(100000,NumArr(lam,delt)),seq(-0.5,50.5),ylim=c(0,0.1),
       col="violet",border="purple3",xlab="Number of Arrivals",
       main="Histogram of 100K replicates of NumArr(10,2)")
points(seq(0,50),trueP,pch=95,cex=1.5,col="blue") 

# ------------------------------------------------------------------- Simulating Binomial Trials --------------
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

# ------------------------------------------------------------ Simulate rolling n fair dice -----------------

# Question: how do we simulate the outcomes 1,2 3,4,5,6 on THREE fair dice?
# Start with runif(3,0,6): run it several times and observe the outcomes
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
sum(RollDice(2))
replicate(100,sum(RollDice(2)))

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


# ---------------------------------------------------------------------------- Sample Mean ---------------------

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

# Histogram and density in a combined plot
myhd = function(v,breaks,freq=F,ylim=NULL,col=NULL,border=NULL,xlab=NULL,main=NULL,
                lwd=2,dcol="red")
{
  myhist(v,breaks=breaks,freq=freq,ylim=ylim,col=col,border=border,xlab=xlab,main=main)
  lines(density(v),lwd=lwd,col=dcol)
}

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

# skip the ylim argument ...and zoom in on the horizontal axis
myhd(replicate(100000,mean(runif(90))),seq(0.3,0.7,0.005),
     col="yellow2",border="orange3",
     main="Sample Mean of 90 indep. Unif(0,1): 100K simulations")

# try SampleMean of two independent normal N(0,1)?
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

#    verify that SMm = mu and SMsd = sigm/sqrt(n)
#        This section compares the histogram and empirical density of the
#        simulation obtained SampleMean of normal N(mu,sigm^2) sample to 
#        to the ('theoretical') pdf of Normal N(mu,sigm^2/n) distribution.
#        This is done to 'test' the statement on the lecture slide 2.11

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



#   In order for 'replicate' to work we cannot pass a CONSTANT VECTOR v.
#   Instead for each replicate new random vector rnorm(n,mean,sd) must be generated.# This can be accomplished in R via a "do.call()" function that takes any function
#   and a LIST of its arguments and executes it:
fn = runif; arglist = list(20)
do.call(fn,arglist)
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


#---------- Apply the procedure to independent Uniforms
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


# ----------Apply the procedure to independent Exp(1)'s
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

# ------------Apply the procedure to independent Student t(4)'s
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


#_________________________________________________lecture 6 ________________________________________________________________

# ------------------------------------ Functional and aesthetic improvements to myhist() and myhd() --------- 
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


# ----------------------------------- Plotting Sample Mean histogram and empirical density (cont.) ---------

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

# We would like to re-use the code above for different random number generators:

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

# ---------------------------- Plotting Transformed Sample Mean histogram and empirical density ---------
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

# ------------------------------------------ Illustration of CLT via empirical CDF function in R ---------

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

v = runif(10000)
plot(ecdf(v),xlim=c(-0.1,1.1),col="red",lwd=3); grid()
# compare it to CDF of Unif(0,1)
lines(x=c(0,1),y=c(0,1),col="blue",lwd=3)

# We illustrate the Central Limit Theorem, namely the convergence of 
#   Transformed Sample Means to a N(0,1), by plotting the empirical CDFs of
#   Transformed Sample Means and comparing them to the Standard Normal CDF (pnorm)
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

# ------------------------------------------------------- Biased vs. Unbiased Sample Variance ---------

# Create a function "SampleVariances" that returns a pair
#   (biased Sample Variance, unbiased Sample Variance)

SVs = function(v)
{
  SM = mean(v)
  ssq = sum((v-SM)^2)
  ssize = length(v)
  return(ssq/(ssize-1))
}

# Try it
SVs(runif(4,0,1))
SVs(runif(30,0,1))
# Replicate 7 times
replicate(7,SVs(runif(4)))
replicate(7,SVs(runif(30)))
































































