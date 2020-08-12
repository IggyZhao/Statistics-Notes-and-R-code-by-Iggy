
# OPIM 5603 RStudio Session 12
# Wednesday, November 20, 2019

setwd("C:/Users/Iggy Zhao/Desktop")

  source("myFunctions.R")     # custom functions


# --------------- Simple Linear Regression (cont) ---------------

# HeightWeight.csv contains a sample of heights and weights
  hw = read.csv("HeightWeight.csv")
  
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
  # try Multilinear Regression with x^2 as term (use I(x^2))
  r2 = lm(y~x+I(x^2)); summary(r2)

# Scenario 4: Nonlinearity 2nd case
  x = runif(500,0,10)
  y = ifelse(x<7,100+2*x+rnorm(500),100+5*x+rnorm(500))
  r = mylm(x,y)
  # residual plots:
  myreg(r)
  
# Scenario 5: Ugly outliers in predictor x
  x = runif(500,0,10)
  y = 250 + x + rnorm(500,0,10)
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
#   a normal random variable with mean beta1 and variance sigm^2/SSX
#   Can we verify this with simulation in R?

# Create a function with inputs:
#    vector x,
#    regression slope (beta1),
#    regression intercept (beta0),
#    square root of regression variance (sigm)
# and that returns the coefficients b0 and b1, assuming linear model
#    Y = beta0 + beta1*X + N(0,sigm^2)

  f = function(x,beta0,beta1,sigm)
  {
    n = length(x)
    y = beta0 + beta1*x + rnorm(n,0,sigm)
    r = lm(y~x)
    return(r$coefficients)
  }

# Try it:
  beta0 = 3; beta1 = 7; sigm = 2
  x=runif(100,0,4)
  f(x,beta0,beta1,sigm)

# What do we get if we replicate this function 
  # options(digits=4)
  x=runif(100,0,4)
  sdv = replicate(10,f(runif(100,0,4),beta0,beta1,sigm)); sdv

# To verify that E(b1) = beta1 (and E(b0) = beta0) we need rowMeans function:
  sdv = replicate(10000,f(runif(100,0,4),beta0,beta1,sigm))
  rowMeans(sdv)
# notice that the 'x' rowMean is very close to beta1
#     and (Intercept) rowMean is very close to beta0
  
# In particular, if we want to extract just the slopes from the sdv
  sdv[2,]

# To verify that the variance of b1 is sigm^2/SSX, we need to use the fixed x vector
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
  
# Finally, compare it to the theoretical distribution N(beta1,sigm^2/SSX)
  t = seq(6,8,0.001)
  lines(t,dnorm(t,beta1,sigm/sqrt(ssx)),lwd=2,col="blue")

  
# While we're at it: the intercept sampling distribution
  intcpts = sdv[1,]
  mysdci(intcpts,pcol="yellowgreen",
         main="Regression intercept sampling distribution")
  
# Compare it to the theoretical distribution N(beta0,(mean(x)*sigm)^2/SSX)
  t = seq(1.5,4.5,0.001)
  lines(t,dnorm(t,beta0,sm*sigm/sqrt(ssx)),lwd=2,col="blue")

  
# ----------------------------------------------------------------------------------------
  
# --------------- Multiple Linear Regression ---------------

# Intro Example: Boston dataset
  
  library(MASS)
  help(Boston)
  View(Boston)
  plot(Boston)
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
  mr = lm(y~x1+x2)
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
  