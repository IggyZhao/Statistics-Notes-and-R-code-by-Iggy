#
#  OPIM 5603 RStudio Optional Session 6+
#  Wednesday, October 2, 2019
#

# From Session 6: axpos(), myaxes(), myhist(), myhd(), myhdnd(), and TSM() functions

  axpos = function(intv)
    return(intv[1] * !(intv[1]<=0 & tail(intv,1)>=0))


  myaxes = function(xaxpos,yaxpos,lwd=2,col="gray50",las=1)
  {
    grid()
    axis(1,pos=xaxpos,lwd=lwd,col=col,las=las)
    axis(2,pos=yaxpos,lwd=lwd,col=col,las=las)
  }


  myhist = function(v,breaks="Sturges",freq=F,ylim=NULL,xlab=NULL,main=NULL,
                    col="yellow2",border="orange2",axes=T)
  {
    main=paste(main,": ",format(length(v),big.mark=",",digits=0)," data points",sep="")
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


  myhdnd = function(v,mean=0,sd=1,breaks="Sturges",ylim=NULL,xlab=NULL,main="rv's",
                    col="lightyellow",border="orange2",
                    dlwd=2,dcol="red",bclwd=2,bccol="blue",axes=T,
                    hd=myhd)
  {
    h = hd(v,breaks=breaks,ylim=ylim,xlab=xlab,main=main,col=col,border=border,
           dlwd=dlwd,dcol=dcol,axes=F)
    x=seq(h$breaks[1],tail(h$breaks,1),l=10*length(h$breaks))
    lines(x,y=dnorm(x,mean,sd),lwd=bclwd,col=bccol)
    if (axes)
      myaxes(0,axpos(h$breaks))
  }


  TSM = function(v,mu,sigm)
  {
    n = length(v)
    SMm = mu; SMsd = sigm/sqrt(n)
    return((mean(v)-SMm)/SMsd)
  }

# As in Session 6:
  
  rpl = replicate; p = paste
  
# Apply plotting procedures myhd() and myhdnd() to a sample from some discrete r.v.
# For example, apply the procedure(s) to a sample from Poisson distribution Pois(mu)
  
  N = 100000
  mu = 15.6   # arbitrary value
  tt = p(" Pois(",mu,") Sample Mean",sep="")

  n=2; myhd(rpl(N,mean(rpois(n,mu))),breaks=200,main=p(n,tt))
  n=2; myhd(rpl(N,mean(rpois(n,mu))),breaks=20,main=p(n,tt))
  n=5; myhd(rpl(N,mean(rpois(n,mu))),breaks=20,main=p(n,tt))

# For some charts histogram bars and/or empirical density function look wierd
# Problem is casued by discrete set of values the mean of Poisson's attains:

  table(rpl(N,mean(rpois(2,mu))))

  # An easier way to see values and their frequencies
  View(data.frame(table(rpl(N,mean(rpois(2,mu))))))
  View(data.frame(table(rpl(N,mean(rpois(3,mu))))))
  View(data.frame(table(rpl(N,mean(rpois(4,mu))))))

# Conclusion:
#    For sample from Poisson distribution (in fact, any discrete)
#    the Sample Mean also has discrete distribution and the vector
#    of replicates will contain many repetitions of same numbers

# Idea: how many "unique" values do we get?
  mu = 15.4
  v = rpl(N,mean(rpois(2,mu)))
  uniqv = unique(v); uniqv     # all unique values
  length(uniqv)                # and their count
  sort(uniqv)                  # sorted unique values
  diff(sort(uniqv))            # their consecutive differences
  min(diff(sort(uniqv)))       # mininum of the consecutive differences
  
  # For three Poissons:
  v = rpl(N,mean(rpois(3,mu)))
  uniqv = unique(v); length(uniqv); min(diff(sort(uniqv)))
  
  # For four Poissons:
  v = rpl(N,mean(rpois(4,mu)))
  uniqv = unique(v); length(uniqv); min(diff(sort(uniqv)))
  
  # For four N(0,1)'s:
  v = rpl(N,mean(rnorm(4)))
  uniqv = unique(v); length(uniqv); min(diff(sort(uniqv)))
  
# For N(0,1) (in fact, any continuous distribution) number of unique
# values in a set of replicates will be huge (equal to vector length)

# The following update to myhd() function uses the following criteria
# to detect if the data is discrete:
#   if length(unique(v)) <= 0.9*length(v), then the data is discrete
#   otherwise data is continuous

  myhd2 = function(v,breaks="Sturges",ylim=NULL,xlab=NULL,main=NULL,
                  col="lightyellow",border="orange2",dlwd=2,dcol="red",axes=T)
  {
    uniqv = unique(v)
    if (length(uniqv) > 0.9*length(v))     # continuous data case: use myhd()
      h = myhd(v,breaks=breaks,ylim=ylim,xlab=xlab,
               main=main,col=col,border=border,axes=F)
    else                                   # discrete data case
    {
      if (breaks[1]=="Sturges")            # if breaks are not assigned
      {                                    # calculate breaks for discrete data
        arg = sort(uniqv)
        del = min(diff(arg))
        breaks = seq(min(arg)-del/2,max(arg)+del/2,del)
      }
      h = myhist(v,breaks=breaks,freq=F,ylim=ylim,xlab=xlab,
                 main=paste(main,"\n(discrete data)"),col=col,border=border,axes=F)
    }
    if (axes)
      myaxes(0,axpos(h$breaks))
    invisible(h)
  }
  
  # Try it:
  N = 100000
  mu = 15.6   # arbitrary value
  tt = p(" Pois(",mu,") Sample Mean",sep="")
  
  n=2; myhd2(rpl(N,mean(rpois(n,mu))),main=p(n,tt))
  
  # You can choose your brakes (carefully though)
  n=2; myhd2(rpl(N,mean(rpois(n,mu))),breaks=seq(2.75,32.25,0.5),main=p(n,tt))
  
  n=17; myhd2(rpl(N,mean(rpois(n,mu))),main=p(n,tt))
  
  # "Backwards compatibility": does it work for continuous variables?
  tt = p(" Unif(0,1) Sample Mean",sep="")
  n=2; myhd2(rpl(N,mean(runif(n))),breaks=seq(0,1,l=201),main=p(n,tt))
  n=5; myhd2(rpl(N,mean(runif(n))),breaks=seq(0,1,l=201),main=p(n,tt))
  n=5; myhd2(rpl(N,mean(runif(n))),main=p(n,tt))
  

# We can apply this procedure to myhdnd() as well
  
  N = 100000
  mu = 15.6
  sigm = sqrt(mu)        # Pois(mu) population standard deviation
  
  tt = p(" Pois(",mu,") Sample Mean",sep="")
  n=2; myhdnd(rpl(N,mean(rpois(n,mu))),mu,sigm/sqrt(n),main=p(n,tt),hd=myhd2)
  n=5; myhdnd(rpl(N,mean(rpois(n,mu))),mu,sigm/sqrt(n),main=p(n,tt),hd=myhd2)
  
  tt = p(" Pois(",mu,") Transformed Sample Mean",sep="")
  n=2;  myhdnd(rpl(N,TSM(rpois(n,mu),mu,sigm)),main=p(n,tt),hd=myhd2)
  n=3;  myhdnd(rpl(N,TSM(rpois(n,mu),mu,sigm)),main=p(n,tt),hd=myhd2)
  n=5;  myhdnd(rpl(N,TSM(rpois(n,mu),mu,sigm)),main=p(n,tt),hd=myhd2)
  n=17; myhdnd(rpl(N,TSM(rpois(n,mu),mu,sigm)),main=p(n,tt),hd=myhd2)
  n=30; myhdnd(rpl(N,TSM(rpois(n,mu),mu,sigm)),main=p(n,tt),hd=myhd2)

  # Check backwards compatibility again   
  mu = 0; sigm = 1
  tt = p(" N(",mu,",",sigm,") Transformed Sample Mean",sep="")
  n=2; myhdnd(rpl(N,TSM(rnorm(n),mu,sigm)),main=p(n,tt),hd=myhd2)
  n=2; myhdnd(rpl(N,TSM(rnorm(n),mu,sigm)),breaks=seq(-5,5,l=101),main=p(n,tt),hd=myhd2)


  