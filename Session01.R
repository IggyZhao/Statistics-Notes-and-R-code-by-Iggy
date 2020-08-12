#
#  OPIM 5603 RStudio Session 1
#  Wednesday, August 28, 2019
#

# -------------- Setting up the working directory --------------

# Check what is your working directory in this RStudio session
  getwd()

# Setting the desired working directory
  setwd("your_path")


# -------------- (rudimentary) Data Visualisation --------------

# Importing the "number of credit card survey" dataset
  ccdata <- read.csv("Data01/CCData.csv")     # assignment via '<-' operator

# View the data
  ccdata
  View(ccdata)

# Digression: assignment via '=' operator
  ccdata1 = read.csv("Data01/CCData.csv")
  View(ccdata1)

# Simple plot (search for "plot" function in the Help tab on the right)
  plot(ccdata)
  plot(ccdata,col="blue")
  plot(ccdata,pch=19,cex=2,col="blue")
  plot(ccdata,pch=65,cex=1,col="red")    # ASCII code 65 is the capital letter A

# Plotting a quasi-histogram
  plot(ccdata,type="s",col="blue")

# Plotting almost-a-histogram
  plot(ccdata,type="h",col="blue")
  plot(ccdata,type="h",col="blue",lwd=50)

# Extracting the columns of ccdata table (matrix)
  ccdata$Cards     # the column (vector) of numbers of cards students owe: (0,1,2,3,4,5,6)
  ccdata$Freq      # the column (vector) of frequencies: (12,42,57,24,9,4,2)

# Histogram via barplot function (search for "barplot" function in the Help tab)
  barplot(ccdata$Freq,col="blue",names.arg = ccdata$Cards)

# Plotting a pie chart (search for "pie" function in the Help tab)
  pie(ccdata$Freq,labels=ccdata$Cards)
  pie(ccdata$Freq,labels=ccdata$Cards,col = c("red","orange","yellow","green","blue","violet","grey"))


# -------------- Average of sequence of numbers --------------

# Note: rep repeates the entries of the first 'vector' number of times specified by second vector
# Example:
  u = rep(c(1,2,3.14159),c(4,1,7))
  u

# Expand the card usage frequency to individual's card count (150 observations)
  v = rep(ccdata$Cards,ccdata$Freq); v

# Computing the mean of the 150 observations
  mean(v)

# Histogram of this data vector (search for "hist" function in the Help tab)
  hist(v)
  hist(v,breaks=seq(-0.5,6.5,1))
  hist(v,breaks=seq(-0.5,6.5,1),col="orange")
  hist(v,breaks=seq(-0.5,6.5,1),col="orange",freq=FALSE)
  hist(v,breaks=seq(-0.5,6.5,1),col="orange",freq=FALSE,ylim=c(0,0.4))
  grid()
  
# try
  hist(v,breaks=seq(-0.5,4.5,1))

# -------------- Expected Value of a discrete r.v. --------------

# Let C denote the number of credit cards random variable. Substitute:
  x = ccdata$Cards
  freq = ccdata$Freq

# Computing the expected value of C by using weighted mean function
  prob <- freq/sum(freq)           # probabilities of the outcomes of C
  sum(prob)

# Useful R function to calculate the expected value is weighted.mean
  weighted.mean(c(2,3,4),c(5,4,1))
  
# Recall: this is the same as the average of the sequence
  rep(c(2,3,4),c(5,4,1))
  mean(rep(c(2,3,4),c(5,4,1)))
  
# See lecture slides: expected value of a discrete r.v. is a weighted mean:
  weighted.mean(x,prob)           # expected value of C

# Note: the expected value of the random variable C is the same as the mean of 150 numbers above
  mean(v)
  
# Note: same result is obtained if the frequencies are used instead of prob
# This is simply consequence of definition of weighted mean
  weighted.mean(x,freq)

