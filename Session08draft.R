
# OPIM 5603 RStudio Session 8
# Wednesday, October 23, 2019


# Recall: plotting densities with polygon function (load myFunctions.R)

  rv = abs(rnorm(1000000))
  myed(rv,main="Abs. value of N(0,1)",pcol="lightyellow")

  u1 = runif(1000000,min=1,max=10)
  u2 = runif(1000000,min=1,max=10)
  myed(u1+u2,main="Sum of two independent U(1,10)")

  u1 = runif(1000000,min=1,max=10)
  u2 = runif(1000000,min=1,max=4)
  myed(u1+u2,main="Sum of independent U(1,10) and U(1,4)")
  

# ----- Classical (one-sample) two-tailed t-test -----

# (Motivation: Quiz 7 Problem 3)
# A botanist is trying to determine whether an experimental growth substance
# applied to plant seeds affected the average height of plants. The standard
# average height is 306.9 mm. The botanist treated a random sample of 420 seeds
# with the extract and subsequently obtained the height data provided in the
# file PlantHeight.csv. She decides to make a conclusion in the following way:
# if the height population mean is inside 95% confidence interval generated
# by her sample she will conclude that the substance did not affect the growth
# of plants.  Otherwise, she will conclude the substance affected the growth.

  plants = read.csv("Data08/PlantHeight.csv")
  
# (a) Plot the empirical density of plants' height data.

  ph = plants$Height
  myed(ph,main="Plant heights empirical density",xlab="Height [mm]")

# (b) Plot over the graph in (a) the pdf of a Normal distribution with
#     parameters appropriately chosen from the sample.

  x = seq(295,320,0.01)
  lines(x,dnorm(x,mean(ph),sd(ph)),lwd=2,col="blue")
  
# (c) Calculate the 95% confidence interval.

  # Session 7 function:
  CIunkep = function(v,conf=0.95)
  {
    n = length(v) 							       # Sample size
    SM = mean(v)							         # SampleMean
    SV = var(v)                        # Sample Variance
    tC = qt(conf+(1-conf)/2,n-1)		   # tC value for the given confidence
    hw = tC*sqrt(SV/n)	    			     # half-width of the confidence interval
    return(c(SM-hw,SM+hw))				     # endpoints of conf interval
  }

  CI = CIunkep(ph); CI

# The 95% conf.interval is (306.9435, 307.5332)
#  => the substance affects the growth.
  
# -----------------------------------------------------------------------------
  
# Lecture slide example: Two-tailed t-test calculation, unknown pop.variance

  v = c(7.85, 6.7, 9.83, 4, 8.61, 6.05, 5.39, 7.82, 5.63, 7.07, 11.2, 6.8)
  
  sm = mean(v); sm                                 # Sample Mean
  sv = var(v); sv                                  # Sample Variance
  n = length(v); n                                 # sample size
  alpha = 0.1                                      # significance level
  mu0 = 7                                          # Null-Hypothesis H0: mu = 7
  T = (sm-mu0)/(sqrt(sv)/sqrt(n)); T               # T statistic value
  pval = pt(-abs(T),n-1) + 1-pt(abs(T),n-1); pval  # p-value
  accept = pval > alpha; accept                    # if TRUE, accept H0
                                                   #   Otherwise reject.
  
# wrap this into a two-tailed t test (ttttest) function:
  ttttest = function(v,mu0,alpha)
  {
    sm = mean(v)                                   # Sample Mean
    sv = var(v)                                    # Sample Variance
    n = length(v)                                  # sample size
    T = (sm-mu0)/(sqrt(sv)/sqrt(n))                # T statistic value
    pval = pt(-abs(T),n-1) + 1-pt(abs(T),n-1)      # p-value
    if (pval>alpha)
    { return(paste("p-value equals",round(pval,5),">",alpha,
                   "; accept Null-Hypothesis: population mean equals",mu0)) }
    else
    { return(paste("p-value equals",round(pval,5),"<=",alpha,
                   "; reject Null-Hypothesis in favor of the Alternative:",
                   "population mean is not equal",mu0)) }
  }
  
  ttttest(v,7,0.1)
  
# Recall the example on lecture slide 2.30 (also in Session 7):
# 90% conf.interval (unknown variance case) for sample given by vector v is
#
#             (6.21587, 8.275797).
#
# Since mu0 = 7 is inside this interval
# the two-tailed t-test accepts the null-hypothesis
# See what happens to the following mu0 values:  

  ttttest(v,8,0.1)
  ttttest(v,6,0.1)
  ttttest(v,6.215,0.1)
  ttttest(v,6.216,0.1)

# -----------------------------------------------------------------------------
  
# Apply the t-test function to the botanist data:

  ttttest(ph,306.9,0.05)

# the 95% conf.interval was (306.9435, 307.5332)

  ttttest(ph,306.943,0.05)
  ttttest(ph,306.944,0.05)
  
# -----------------------------------------------------------------------------
  
# Another example: student admissions data frame
  adm <- read.csv("Data08/Admission.csv")

# review the data frame
  adm            # "prints" the frame in a console
  View(adm)      # the entire frame in a separate window
  head(adm)      # initial rows only
  summary(adm)   # basic stats
  
# plot the empirical density of the GMAT data
  myed(adm$GMAT,main="GMAT data",xlab="GMAT score",pcol="khaki",dcol="brown")
  
  ## dGMAT = density(adm$GMAT)
  ## plot(dGMAT,lwd=3,col="red",main="GMAT data",xlab="GMAT score")
  ## polygon(dGMAT,col="lightgreen")
  
# Apply ttttest() function on the GMAT data:
# we test if population mean equals, say, 510
  # H0: The GMAT score population mean equals 510
  # Ha: The GMAT score population mean is NOT equal 510

  ttttest(adm$GMAT,510,0.5)
  # We reject H0 at significance level 0.05 in favor of Ha
  
  mean(adm$GMAT)  
  ttttest(adm$GMAT,500,0.5)

# Compare to built-in t-test
# Note: it uses confidence level instead of the significance
  t.test(adm$GMAT,mu=510,conf.level=0.95)   # 0.95 default conf.level
  t.test(adm$GMAT,mu=510)

# Take a look at the confidence interval
  t.test(adm$GMAT,mu=471)     # 471 is inside the interval
  t.test(adm$GMAT,mu=470)     # 470 is outside the interval

  
# -----------------------------------------------------------------------------
  
# Classical one-tailed t-test: Salinity example
  
  # H0: The salinity of the lake water is less than or equal to 0.5
  #     (grams of salt in 1 kg of water)
  # Ha: The salinity of the lake water is strictly greater than 0.5
  
  u = read.csv("Data08/Salinity.csv")
  View(u)
  mean(u$Salinity)
  
  # Plot the empirical density of the collected salinity data
  myed(u$Salinity,xlab = "Salinity [ g/l ]",main="Salinity measurements",
       pcol="lightblue",dcol="blue3")
  abline(v=0.5,col="red")
  
  t.test(u$Salinity,mu=0.5,alternative="greater")   # Upper-tailed t-test
  t.test(u$Salinity, mu=0.5)                        # Two-tailed t-test
  t.test(u$Salinity, mu=0.5, alternative="less")    # Lower-tailed t-test
  
  # Compare the value of the t-statistic with t(57) quantiles
  qt(c(0.025,0.05,0.95,0.975),57)
  
# -----------------------------------------------------------------------------
  
# Now apply the built-in t.test function to the botanist data:
#
# H0: The mean height of treated population is less than or equal to 306.9
# Ha: The mean height of treated population is strictly greater than 306.9
  
# Plot again the empirical density (dh above) of the collected height data

  myed(ph,main="Plant heights empirical density",xlab="Height [mm]")
  abline(v=306.9,col="blue")
  
  t.test(ph,mu=306.9,alternative="greater")   # Upper-tailed t-test
  t.test(ph,mu=306.9)                         # Two-tailed t-test
  t.test(ph,mu=306.9, alternative="less")     # Lower-tailed t-test
  
