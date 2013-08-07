"CoeffsToRData" <- function ( year1=2000, yearN=2009, gcmvar="ts",
     fname="ts_A1.SRESA1B_1.CCSM.atmm.2000-01_cat_2099-12.nc", 
     gcmdir="/fs/image/home/thoar/downscaling/gcm",
     dircoeff="/project/gsp/thoar/downscaling", prismvar="tave" )
{
#
#   $Id: CoeffsToRData.r,v 1.7 2007/02/19 23:52:25 thoar Exp $
#
#   gcmdir     location of GCM data to downscale
#   fname      specific file to downscale
#   prismvar   one of ['tave' 'dtr' 'tmin' 'tmax' 'ppt']
#   dircoeff   directory holding the .RData with the coefficients
#
#   year1    = 2010
#   yearN    = 2019
#   gcmvar   = "ts"
#   gcmdir   = "/fs/image/home/thoar/downscaling/gcm"
#   fname    = "ts_A1.SRESA1B_1.CCSM.atmm.2000-01_cat_2099-12.nc"
#   prismvar = "tave"
#   dircoeff = "/project/gsp/thoar/downscaling"
#

cat(sprintf("year1    is %d\n",year1))
cat(sprintf("yearN    is %d\n",yearN))
cat(sprintf("gcmvar   is %s\n",gcmvar))
cat(sprintf("fname    is %s\n",fname))
cat(sprintf("gcmdir   is %s\n",gcmdir))
cat(sprintf("prismvar is %s\n",prismvar))
cat(sprintf("dircoeff is %s\n",dircoeff))

library(ncdf)
library(fields)

#-------------------------------------------------------------------------------
# Get regression coefficients (and metadata) derived from the prism data.
#
# $means    [41,19,1332] the prism estimate of the GCM value (only over conUS)
# $gcmlons    [41]       longitudes for these estimates [236.25,gcmlons,292.5]
# $gcmlats    [19]       latitudes  for these estimates [24.513,gcmlats,49.73]
# $gcmloninds [41]       the indices that _should_ correspond to the indexing
#                        in the GCM netcdf file. 
# $gcmlatinds [19]       likewise
# $lons    [1405]        longitudes at prediction locations
# $lats         [621]    latitudes  at prediction locations
# $times          [1332] times of prism estimates of GCM values
# $m       [1405,621,12] slope coefficients for the prediction locations
# $b       [1405,621,12] intercepts for the prediction locations
# $sse     [1405,621,12] sum of the squared error from regression
# $gcmN    [1405,621,12] # of data in regression
# $gcmxi   [1405,621]    index into subsetted GCM data [1,gcmxi,41]
# $gcmyj   [1405,621]    index into subsetted GCM data [1,gcmyj,19]
# $gcmlon  [1405,621]    closest gcm longitude for this prism location
# $gcmlat  [1405,621]    closest gcm latitude  for this prism location
#-------------------------------------------------------------------------------

fnamecoeff = sprintf("%s/prism_%s.RData",dircoeff,prismvar)
load(fnamecoeff)
hires = coefficients
rm(coefficients)

dim(hires$means)
length(hires$gcmlons)
length(hires$gcmlats)
length(hires$gcmloninds)
length(hires$gcmlatinds)
length(hires$lons)
length(hires$lats)
length(hires$times)
dim(hires$m)
dim(hires$b)
dim(hires$sse)
dim(hires$gcmN)
dim(hires$gcmxi)
dim(hires$gcmyj)
dim(hires$gcmlon)
dim(hires$gcmlat)

hires$nlon = dim(hires$m)[1]
hires$nlat = dim(hires$m)[2]
hires$nmon = dim(hires$m)[3]

hires$xi   = (1:hires$nlon)
hires$yj   = (1:hires$nlat)

if ( 1 == 2 ){
   ndevices = length(dev.list())
   if (ndevices < 2){ for (idev in (ndevices+1):2) x11() }

   dev.set(2)
   #image.plot(hires$xi,hires$yj,hires$gcmN[,,1])

   dev.set(3)
   image.plot(hires$gcmlons,hires$gcmlats,hires$means[,,1])
}

lonrange   = range(hires$gcmlons,na.rm=TRUE)
latrange   = range(hires$gcmlats,na.rm=TRUE)
westmost   = lonrange[1]
eastmost   = lonrange[2]
southern   = latrange[1]
northern   = latrange[2]

Ngridcells = hires$nlon * hires$nlat
cat(paste("There are",Ngridcells,"prism grid cells over the continental US"),"\n")

#---------------------------------------------------------------------
# Get the GCM data to be downscaled. 
# Since we know the lat/lon bounds already, just ingest what we need.
#---------------------------------------------------------------------

gcmnc    = open.ncdf(paste(gcmdir,fname,sep="/")) 
gcmunits = att.get.ncdf(gcmnc,gcmvar,"units")$value
gcmlname = att.get.ncdf(gcmnc,gcmvar,"long_name")$value
gcmlons  = get.var.ncdf(gcmnc, "lon")
gcmlats  = get.var.ncdf(gcmnc, "lat")
gcmtime  = get.var.ncdf(gcmnc, "time")
gcmNlons = length(gcmlons)
gcmNlats = length(gcmlats)
gcmNtime = length(gcmtime)

nlonsindarray = (1:gcmNlons)
nlatsindarray = (1:gcmNlats)
ntimeindarray = (1:gcmNtime)

loninds = nlonsindarray[(gcmlons >= westmost) & (gcmlons <= eastmost)]
latinds = nlatsindarray[(gcmlats >= southern) & (gcmlats <= northern)]

if ( sum(loninds - hires$gcmloninds) > 0) {
   stop("ERROR ... lons not what we expect them to be.\n")
}
if ( sum(latinds - hires$gcmlatinds) > 0) {
   stop("ERROR ... lats not what we expect them to be.\n")
}
if ( gcmunits != "K" ) {
   cat(sprintf('gcm   variable has units ... %s \n',gcmunits))
   cat(sprintf('regression     had units ... %s \n',hires$units))
}

Ngridcells   = length(loninds)*length(latinds)
cat(paste("There are",Ngridcells,"GCM grid cells over the continental US"),"\n")

# these should pertain to the dates in the GCM netcdf file

yeararr  = (2000:2099)
monarr   = (1:12)
dates    = expand.grid(months=monarr,years=yeararr)
timeind  = ntimeindarray[(dates$years >= year1) & (dates$years <= yearN)]
Ntimeind = length(timeind)

rm(nlonsindarray, nlatsindarray, ntimeindarray)

start       = c(     loninds[1],      latinds[1], timeind[1] )
count       = c(length(loninds), length(latinds),   Ntimeind )
datmat      = get.var.ncdf(gcmnc,gcmvar,start,count)
gcmtimebnds = get.var.ncdf(gcmnc,'time_bnds',c(1,timeind[1]),c(-1,Ntimeind))
close.ncdf(gcmnc)

# The time in this dataset is new to me. No leap years.
# the time_bnds seem to contain the dates of the averaging period.
# The 'time' seems to contain the average of the two.

if ( gcmtimebnds[1,1] != 365*year1) {
   stop("ERROR ... times not what we expect them to be.\n")
}

# subset to just the time we are interested in.

gcmtime   = gcmtime[timeind[1]:(timeind[1]+Ntimeind-1)]

rm(loninds,latinds,dates,timeind)

timeindex = rep(1:12,Ntimeind/12)

#---------------------------------------------------------------------
# Loop over all predicted locations and 'downscale'.
#---------------------------------------------------------------------

cellcount = 0

hiresinds = expand.grid(ilon = hires$xi  , ilat = hires$yj  )
hiresgrid = expand.grid(lons = hires$lons, lats = hires$lats)
hiresgrid = cbind(hiresgrid,hiresinds)
rm(hiresinds)

Npredictions = length(hiresgrid$lons)
predictions  = matrix(NA,nrow=Npredictions, ncol=Ntimeind)

# i = 523500 over Illinois
for (i in 1:Npredictions){

   cellcount = cellcount + 1

   if ( floor(cellcount/1000) == cellcount/1000 ) {
      cat(paste("Working on cell ",cellcount,"of",Npredictions),"\n")
   }

   #------------------------------------------------------------------
   # When making the regression coefficients, the locations of the
   # pertinent GCM gridbox were logged.
   #------------------------------------------------------------------

   predlon  = hiresgrid$lons[i]  # longitude at this prediction loc
   predlat  = hiresgrid$lats[i]  # latitude  at this prediction loc
   predilon = hiresgrid$ilon[i]  # index into prediction lon array [1,1405]
   predilat = hiresgrid$ilat[i]  # index into prediction lat array [1, 621]

   # These pertain to the GCM grid box closest to prediction location
   gcmilon = hires$gcmxi[predilon,predilat] # index into GCM lon array [1,41]
   gcmilat = hires$gcmyj[predilon,predilat] # index into GCM lat array [1,19]
   gcmlon  = hires$gcmlons[gcmilon]         # GCM lon closest to pred loc
   gcmlat  = hires$gcmlats[gcmilat]         # GCM lat closest to pred loc

#  plot(predlon,predlat,xlim=c(westmost,eastmost),ylim=c(southern,northern))
#  US(add=TRUE,shift=TRUE)

   for (imon in 1:Ntimeind){

      #---------------------------------------------------------------
      # Grab the separate pieces 
      #---------------------------------------------------------------

      monthindex =                 timeindex[imon]   # mod[1,12]
      gcmdata    =  datmat[ gcmilon, gcmilat,imon]
      slope      = hires$m[predilon,predilat,monthindex]
      xtrcp      = hires$b[predilon,predilat,monthindex]

      predictions[i,imon] = slope*gcmdata + xtrcp
#     predictions[i,imon] = slope*273.15 + xtrcp

   }
}

#---------------------------------------------------------------------
# reshape predictions into [lon,lat,t]
#---------------------------------------------------------------------
rm(hiresgrid)

predmat = array(predictions, c(hires$nlon,hires$nlat,Ntimeind),
                  dimnames=c("lon","lat","time"))

val = list( lons=hires$lons, lats=hires$lats, time=gcmtime, year1=year1,
            yearN=yearN, prismvar=prismvar, predictions=predmat, 
            timebnds=t(gcmtimebnds), parentfname=fname, parentvar=gcmvar, 
            parentvarunits=gcmunits, parentvarlongname=gcmlname,
            parentlons=hires$gcmlons, parentlats=hires$gcmlats,
            parentdata=datmat )

return(val)

}
