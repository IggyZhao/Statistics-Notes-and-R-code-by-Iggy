
# OPIM 5603 RStudio Session 11
# Wednesday, October 13, 2019
setwd("C:/Users/Iggy Zhao/Desktop")
  source("myFunctions.R")

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
  abline(r1,lwd=1.5,col="red") # - trend line - iggy 

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
  
  r = mylm(women$height,women$weight)
  r = mylm(women$height,women$weight,fitted=TRUE)
  
# Plotting other regression related information
  
  par(mfrow=c(2,2))           # set 2x2 plots ...
  plot(r1,pch=19,col="blue")  # ... and plot r1
  par(mfrow=c(1,1))           # back to single plot

# make it a 'procedure'
  myreg = function(regress)
  {
    par(mfrow=c(2,2))
    plot(regress,pch=19,cex=0.75,col="blue")
    par(mfrow=c(1,1))
  }

  myreg(r1)
  
# Note: The scatter-plot residuals against predictor (height) looks
#   the same as the scatter=plot residuals againts fitted values.
#   The only difference is the scale on the x-axis
  
  plot(h,r1$residuals,frame.plot=F,pch=19,col="blue");grid()  
  plot(r1$fitted.values,r1$residuals,frame.plot=F,pch=19,col="green4");grid()  
  plot(h,w,axes=F,pch=19,cex=1,xlim=c(0,80),ylim=c(0,200),
       col="blue",xlab="Height",ylab="Weight")
  myaxes(0,0)
  
  # Digression: To suppress the intercept (set it to zero) use option -1 in 'formula'h-1)
  r0 = lm(formula = weight~height -1,data = women)  # or r0 = lm(w~
  summary(r0)
  abline(r0,lwd=1.5,col="orange")
  abline(r,lwd=1.5,col="red")
  

# Example: Exam 1 data
  
  exam = read.csv("Data11/Exam1.csv"); View(exam)
  r = mylm(exam$Part2,exam$Part1,xlim=c(0,55),ylim=c(0,45),fitted=T)
  # abline(v = seq(0,55),lwd=0.5,col="gray90")       # denser grid on x-axis
  r$residuals
  
# Residuals are not ordered (neither is the original data)
  exam1 = exam[order(exam$Part2),]      # sort the data frame ascendingly by Part2 score
  r1 = mylm(exam1$Part2,exam1$Part1,xlim=c(0,55),ylim=c(0,45))
  # abline(v = seq(0,55),lwd=0.5,col="gray90")
  r1$residuals
  sum(r1$residuals)
  
  myreg(r)   # myreg(r1) has the same summary output and identical plots
  

# ----------------------------------------------------------------------------------------
  
# Simulations that generate samples (Xi,Yi) from populations X and Y
# Using R we can construct various scenarios of Y dependence on X

# Scenario 1: Textbook case simple linear regression
  x = runif(500,1,100)
  y = 250 + 1*x + rnorm(500,0,10)    # Beta0 = 250, Beta1 = 1, error ~ N(0,sd=10)
  r = mylm(x,y)
  # residual plots:
  myreg(r)

# Scenario 2: Heteroscedasticity
  x = runif(500,1,20)
  y = 100 + 2*x + x*rnorm(500,0,1) # Beta0 = 250, Beta1 = 1, error ~ x*N(0,1)
  r = mylm(x,y)
  # residual plots:
  myreg(r)

# Scenario 3: Nonlinearity 1st case: quadratic dependence
  x = runif(500,1,20)
  y = 100 + 2*x + 3*x^2 + rnorm(500,0,20)
  r = mylm(x,y)
  # residual plots:
  myreg(r)
  # try Multilinear Regression with x^2 as term (use I(x^2))
  r2 = lm(y~x+I(x^2)); summary(r2)

# Scenario 4: Nonlinearity 2nd case
  x = runif(500,1,20)
  y = ifelse(x<15,100+2*x+rnorm(500),100+5*x+rnorm(500))
  r = mylm(x,y)
  # residual plots:
  myreg(r)
  
# Scenario 5: Ugly outliers in predictor x
  x = runif(500,1,100)
  y = 250 + x + rnorm(500,0,10)
  x[500] = 860
  r = mylm(x,y)
  
  # exclude the outlier (x[500],y[500]):
  x1 = x[-500]; y1 = y[-500]
  r1 = lm(y1 ~ x1); summary(r1)
  abline(r1,lwd=3,col="green2")
  
# Handling robust linear regression with "rlm" function from MASS package
  library(MASS)
  r2 = rlm(y~x); summary(r2)
  abline(r2,lwd=3,col="orange",lty=3)
  # Check the "Summaries" on the console output: rlm output is not the same as for exclusion of the outlier
  
  # residual plots for Scenario 5
  myreg(r)
  
  
# Scenario 6: Ugly outliers in dependent var y
  x = runif(500,1,100)
  y = 250 + x + rnorm(500,0,10)
  y[500] = 2000
  r = mylm(x,y)
  
  r2 = rlm(y~x); summary(r2)
  abline(r2,lwd=2,col="orange")
  # residual plots:
  myreg(r)
