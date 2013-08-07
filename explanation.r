#======================================================================
# explanation.r
#
# Need to graphically explain the linear model.
# 1) scatterplot:
#    Y axis is the PRISM data, 
#    X axis is the 'pseudo' coarse data after it has been 
#    splined to same locations as PRISM data.
#
# 2) same scatterplot w/ straight line fit through it
#    Y = Mx+b
#
# 3) replace X axis with GCM data splined to PRISM locations.
#
#======================================================================

library(ncdf)
library(fields)
library(lattice)

# for coral

xdir = '/ptmp/thoar/downscaling'
ddir = '/ptmp/thoar/downscaling/TAS'
ydir = '/ptmp/thoar/downscaling/prism100'

# for fisher (with .png)

xdir = '/project/gsp/thoar'
ddir = '/project/gsp/thoar'
ydir = '/project/gsp/thoar'

monstr = 'July'
month  = 7

#----------------------------------------------------------------------
# Get GCM grid 
#----------------------------------------------------------------------

source("/fs/image/home/thoar/downscaling/GetPredDomain.r")

preddomain    = GetPredDomain()
nlonsindarray = (1:preddomain$nlons)
nlatsindarray = (1:preddomain$nlats)

coarsefname   = sprintf("%s/tas_A1.20C3M_1.CCSM.atmm.1870-01_cat_1999-12.nc",ddir)
coarsenc      = open.ncdf(coarsefname)
coarselons    = get.var.ncdf(coarsenc, "lon")
coarselats    = get.var.ncdf(coarsenc, "lat")
ncoarselons   = length(coarselons)
ncoarselats   = length(coarselats)

# get (raw) east texas data for later
#lon(190) == 265.78125
#lat(88) == 32.9180072

EastTexas = get.var.ncdf( coarsenc,'tas', start=c(190,88,1), count=c(1,1,-1)) - 273.15

close.ncdf(coarsenc)

lftmost = preddomain$WESN[1]
rhtmost = preddomain$WESN[2]
botmost = preddomain$WESN[3]
topmost = preddomain$WESN[4]

loninds = (1:ncoarselons)[(coarselons >= lftmost) & (coarselons <= rhtmost)]
latinds = (1:ncoarselats)[(coarselats >= botmost) & (coarselats <= topmost)]

ncoarselonconus = length(loninds)
ncoarselatconus = length(latinds)

coarsegrid = expand.grid(lon = coarselons[loninds], lat = coarselats[latinds])
Ngridcells = ncoarselonconus * ncoarselatconus

cat(paste("There are",Ngridcells,"coarse grid cells over the continental US"),"\n")

#----------------------------------------------------------------------
# Get PRISM GRID and 12 locations to explore (3 lats, 4 lons)
# prism is celsius
# smoothed & GCM are kelvin
#----------------------------------------------------------------------

prismvar = 'tave'
Yfname   = sprintf("%s/prism_1895_2005_tave.nc",ydir)
Xfname   = sprintf("%s/PRISM_splined_tave_1895_2005.nc",xdir)
Mfname   = sprintf("%s/downscaling/coeffs_tave_1895_2005.nc",xdir)

smoothnc          = open.ncdf(Xfname)
prismnc           = open.ncdf(Yfname)
modelnc           = open.ncdf(Mfname)
prismlons         = get.var.ncdf(prismnc, "lon")
prismlats         = get.var.ncdf(prismnc, "lat")
prismtimes        = get.var.ncdf(prismnc, "time")
prismtimeorigin   = att.get.ncdf(prismnc, "time", "origin")$value
nprismlons        = length(prismlons)
nprismlats        = length(prismlats)
nprismtimes       = length(prismtimes)

# indices picked from "ncview us_25m.nc"

loninds = c(  38,  51, 132, 455, 455, 455, 733, 733, 733, 1325, 1073, 1041)
latinds = c( 566, 407, 286, 557, 361, 200, 552, 361, 200,  507,  335,   96)
locations = list(x = prismlons[loninds], y = prismlats[latinds])

#----------------------------------------------------------------------
# The locator map
#----------------------------------------------------------------------

mai=c(0.1,0.1,0.1,0.1)
omi=c(0.1,0.1,0.1,0.1)
mgp=c(1,0,0)

par(mai=mai,omi=omi,mgp=mgp)
pdf(file='locatormap.pdf',width=6.0,height=4.5)
#png(file='locatormap.png',width=1000,height=700)
plot(coarsegrid, xlab="", ylab="",lwd=0.5)
US(add=TRUE,shift=TRUE,lwd=2.0,col=4)
points(locations,cex=2.0,col=2,pch=19)
dev.off()

#----------------------------------------------------------------------
# harvest the time series for each location
#----------------------------------------------------------------------

Y1 = array(0,c(length(loninds),nprismtimes))
X1 = array(0,c(length(loninds),nprismtimes))
slope1 = array(0,c(length(loninds),1))
xtrcp1 = array(0,c(length(loninds),1))

for (i in 1:length(loninds)){
cat(sprintf("Getting point %d of %d\n",i,length(loninds)))
Y1[i,] = get.var.ncdf( prismnc,prismvar, start=c(loninds[i],latinds[i],1), count=c(1,1,-1))
X1[i,] = get.var.ncdf(smoothnc,prismvar, start=c(loninds[i],latinds[i],1), count=c(1,1,-1)) - 273.15

slope1[i] = get.var.ncdf(modelnc,'m', start=c(loninds[i],latinds[i],month), count=c(1,1,1))
xtrcp1[i] = get.var.ncdf(modelnc,'b', start=c(loninds[i],latinds[i],month), count=c(1,1,1))

}

close.ncdf(smoothnc)
close.ncdf( prismnc)
close.ncdf( modelnc)

# subset out all monthly temps

inds         = rep(F,12)
inds[month]  = T
nyears       = 2005-1895+1
monthindices = rep(inds,nyears)
prism        = c(Y1[,monthindices])
smoothed     = c(X1[,monthindices])
gcm          = EastTexas[monthindices]

#----------------------------------------------------------------------
# trellis plot of those locations
# Since I started with 1 represting the Pacific NW ... and the
# trellis function starts with 1 being the lower left plot ...
#----------------------------------------------------------------------

lat      = rep( c(3:1,3:1,3:1,3:1) ,nyears)
lon      = rep(sort(c(1:4,1:4,1:4)),nyears)

pdf(file=sprintf("Prism_smooth_%s_trellis.pdf",monstr),width=7.5,height=7.5)
#png(file=sprintf("Prism_smooth_%s_trellis.png",monstr),width=800,height=1100)
xyplot(prism ~ smoothed | lon*lat, aspect=1.0, type = "p", 
     xlab  = list(label="spline estimates",cex=2.0),
     ylab  = list(label="Prism data", cex=2.0),
     main  = list(label=sprintf("1895-2005 %s Monthly Mean Daily Average Temperature",monstr),cex=1.5),
     panel = function(x, y) { 
       panel.grid(h=-1, v=-1) 
       panel.xyplot(x, y) 
     }
)
dev.off()

pdf(file=sprintf("Prism_smooth_%s_trellis_lm.pdf",monstr),width=7.5,height=7.5)
#png(file=sprintf("Prism_smooth_%s_trellis_lm.png",monstr),width=800,height=1100)
xyplot(prism ~ smoothed | lon*lat, aspect=1.0, type = "p", 
     xlab  = list(label="spline estimates",cex=2.0),
     ylab  = list(label="Prism data", cex=2.0),
     main  = list(label=sprintf("1895-2005 %s Monthly Mean Daily Average Temperature",monstr),cex=1.5),
     panel = function(x, y) {
       panel.grid(h=-1, v=-1) 
       panel.xyplot(x, y) 
       panel.lmline(x,y,col=2,lwd=2)
    }
)
dev.off()

#----------------------------------------------------------------------
# Demonstrate the effect of regression
#----------------------------------------------------------------------

montana = 4;
x     = X1[montana,monthindices]
y     = Y1[montana,monthindices]
mymod = lm(y ~ x)
slope = mymod$coefficients["x"]
xtrcp = mymod$coefficients["(Intercept)"]
xx    = c(min(x),max(x))
yy    = slope*xx + xtrcp

# determining the regression

pdf(file="downscaling_get_model.pdf",width=7.5,height=7.5)
plot(x, y, type='p', asp=1.0, cex=1.5, cex.axis=1.5,
      xlab='spline estimates', cex.lab=1.5,
      ylab='Prism Temperature',
      main=sprintf("some Montana location (for %s)",monstr)
)
grid()
points(xx,yy,type='l',col=2,lwd=2)
text(19.5,26.00,sprintf("slope     = %2.3f",slope),col=2,cex=1.5)
text(19.5,25.25,sprintf("intercept = %2.3f",xtrcp),col=2,cex=1.5)
dev.off()

# appication of the regression relationship

pdf(file="downscaling_apply_model.pdf",width=7.5,height=7.5)
plot(xx, yy, type='l', asp=1.0, col=2, lwd=2, cex.axis=2.0,
      xlab='splined (Scenario) Temperature', cex.lab=1.5,
      ylab='Downscaled Temperature',
      main=sprintf("some Montana location (for %s)",monstr)
)
grid()
xhat = c(22,22,par()$usr[1])
yhat = c(par()$usr[3],22*slope + xtrcp, 22*slope+xtrcp)
points(xhat,yhat,type='b',col=4,lwd=3,cex=2.0)
text(19.5,26.00,sprintf("slope     = %2.3f",slope),col=2,cex=1.5)
text(19.5,25.25,sprintf("intercept = %2.3f",xtrcp),col=2,cex=1.5)
text(17.5,22,sprintf("%f",yhat[3]),col=4,cex=2.0)
dev.off()
