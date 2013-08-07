library(fields)
library(ncdf)

# Load in the data
data <- open.ncdf("/glade/p/work/lenssen/output/tmax.nc")

# Read in the lat lon parameters and make an xplot
x <- get.var.ncdf(data,"lon")
y <- get.var.ncdf(data,"lat")
xPlot <- x-360

# Create the empty list that will have the yearly mean for each month
monthMat <- matrix(NA, nrow=111, ncol=12)

# Load the tmax data and extract the jan..dec data
tmax <- get.var.ncdf(data, "tmax") #SLOW STEP!!!!

index <- seq(0,by=12,length=111)

for(i in 1:12){
  for(j in 1:111){
    monthMat[j,i] <- mean(tmax[,,index[j]+i],na.rm=T)
  }
}

# Now perform a moving average (simple with width 6 with even weighting)
maMat <- matrix(NA, nrow=111, ncol=12)
for(i in 1:12){
  for(j in 1:111){
    start <- j-2
    end <- j+2
    if(start < 1) start <- 1
    if(end > 111) end <- 111

    maMat[j,i] <- mean(monthMat[start:end,i])
  }
}

# Try something a little different: standardize each month with its mean
monthStdMat <- matrix(NA, nrow=111, ncol=12)
for(i in 1:12){
  monthStdMat[,i] <- monthMat[,i]-mean(monthMat[,i])
}

t <- seq(from=1895, to=2005, length=111*12)
stdTmax <- c(t(monthStdMat))
plot(t,stdTmax)

# Now try a moving average on the standardized data
maStdTmax <- rep(NA, length=111*12)
for(i in 1:length(stdTmax)){
  start <- i-2
  end <- i+2
  if(start < 1) start <- 1
  if(end > 111*12) end <- 111*12

  maStdTmax[i] <- mean(stdTmax[start:end])
}

# Make some plots for presentation to show difficulties
seq <- seq(from=2,to=111*12, by=12)
plot(t[-seq],maStdTmax[-seq])
points(t[seq],maStdTmax[seq],col="red")
res <- lm(maStdTmax~t)
abline(res, col="blue",lwd=2)

#split into two plots
breakpt <- which(t==1985)
t1 <- t[1:breakpt]
t2 <- t[breakpt:(111*12)]
m1 <- maStdTmax[1:breakpt]
m2 <- maStdTmax[breakpt:(111*12)]
set.panel(1,2)
plot(t1,m1)
res1 <- lm(m1~t1)
abline(res1,col="blue",lwd=2)
plot(t2,m2)
res2 <- lm(m2~t2)
abline(res2, col="blue", lwd=2)

#Plot the data and superimpose a yearly mean
plot(t,c(t(monthMat)),type="l")
yearMean <- rep(NA, 111)
for(i in 0:110){
  yearMean[i] <- mean(c(t(monthMat))[(12*i+1):(12*i+12)])
}
year <- 1895:2005
points(year,yearMean,type="l",col="red", lwd=3)
