library(fields)
library(ncdf)

# Load in the data
data <- open.ncdf("/glade/p/work/lenssen/output/tmax.nc")

# Get all of the January indicies
jan <- seq(1,by=12,length=111)
jul <- seq(7,by=12,length=111)

# Read in the lon lat parameters and make x for plotting with US()
x <- get.var.ncdf(data,"lon")
y <- get.var.ncdf(data,"lat")
xPlot <- x-360

# Extract the January data for tmax (This is a really slow way,
# there has got to be a better way that doesn't involve pulling the
# whole ncdf file to memory first
tmax <- get.var.ncdf(data, "tmax")
tjan <- tmax[,,jan]
tjul <- tmax[,,jul]
rm(tmax)

#get the monthly means
janMeans <- rep(NA, length(jan))
julMeans <- rep(NA, length(jul))
for (i in 1:length(jan)){
  janMeans[i] <- mean(tjan[,,i],na.rm=T)
  julMeans[i] <- mean(tjul[,,i],na.rm=T)
}

set.panel(1,2)
years <- 1895:2005
plot(years, janMeans, type="l")
plot(years, julMeans, type="l")

# Standardize to mean 0
sJanMeans <- janMeans-mean(janMeans)
sJulMeans <- julMeans-mean(julMeans)

set.panel(1,2)
plot(years, sJanMeans, type="l")
plot(years, sJulMeans, type="l")
