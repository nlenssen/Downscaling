"CoeffsEveryToRData" <- function ( year1=1895, yearN=1895, 
     prismvar="tave", coarsevar="tas",
     coarsefile="tas_A1.20C3M_1.CCSM.atmm.1870-01_cat_1999-12.nc", 
     ddir="/ptmp/thoar/downscaling/TAS",
     dircoeff="/ptmp/thoar/downscaling",
     yeararr=(1870:1999) )
{
#
#   $Id: PresentationSpline.r,v 1.3 2008/04/24 03:57:35 thoar Exp $
#
#   ddir          location of GCM data to downscale
#   coarsefile    specific file to downscale
#   prismvar      one of ['tave' 'dtr' 'tmin' 'tmax' 'ppt']
#   dircoeff      directory holding the netCDF file of coefficients
#
#   year1      = 2010
#   yearN      = 2019
#   coarsevar  = "tas"
#   ddir       = "/fs/image/home/thoar/downscaling/gcm"
#   coarsefile = "tas_A1.SRESA1B_1.CCSM.atmm.2000-01_cat_2099-12.nc"
#   prismvar   = "tave"
#   dircoeff   = "/project/gsp/thoar/downscaling"
#

cat(sprintf("year1      is %d\n",year1))
cat(sprintf("yearN      is %d\n",yearN))
cat(sprintf("prismvar   is %s\n",prismvar))
cat(sprintf("coarsevar  is %s\n",coarsevar))
cat(sprintf("coarsefile is %s\n",coarsefile))
cat(sprintf("ddir       is %s\n",ddir))
cat(sprintf("dircoeff   is %s\n",dircoeff))

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

fnamecoeff  = sprintf("%s/coeffs_%s_1895_2005.nc",dircoeff,prismvar)
coeffnc     = open.ncdf(fnamecoeff, write=FALSE)
nctimes     = get.var.ncdf(coeffnc, "time")
nccalendar  = att.get.ncdf(coeffnc, "time", "calendar") 
nclat       = get.var.ncdf(coeffnc, 'lat')
nclon       = get.var.ncdf(coeffnc, 'lon')

prism = list(fnamecoeff = fnamecoeff,
             lat        = nclat,
             lon        = nclon,
             nlat       = length(nclat),
             nlon       = length(nclon),
             nmonths    = length(nctimes),
             months     = nctimes)

rm(nclat, nclon, nctimes)

Ngridcells = prism$nlon * prism$nlat
cat(paste("There are",Ngridcells,"prism grid cells over the continental US"),"\n")

#---------------------------------------------------------------------
# Get the PRISM (high-resolution) domain
#---------------------------------------------------------------------

preddomain    = GetPredDomain()

#---------------------------------------------------------------------
# This block subsets and interpolates to the prediction grid.
# This should match 'SplinePredict.r', for the most part. The GCM
# data has different netCDF attributes.
#---------------------------------------------------------------------
# Get the coarse grid from the netCDF file, subset to conUS+
# Since the coarse grid is bigger than the prediction domain,
# I will use a bit more of the data to create the spline surface
# in hopes of reducing the edge effects.
#---------------------------------------------------------------------

coarsefname   = sprintf("%s/%s",ddir,coarsefile)
coarsenc      = open.ncdf(coarsefname)
coarselons    = get.var.ncdf(coarsenc, "lon")
coarselats    = get.var.ncdf(coarsenc, "lat")
coarseunits   = att.get.ncdf(coarsenc, coarsevar,         "units")$value
coarsestdname = att.get.ncdf(coarsenc, coarsevar, "standard_name")$value
coarselngname = att.get.ncdf(coarsenc, coarsevar,     "long_name")$value
coarsetimes   = get.var.ncdf(coarsenc, "time")
timeunits     = att.get.ncdf(coarsenc, "time",         "units")$value
timecalendar  = att.get.ncdf(coarsenc, "time",      "calendar")$value
timestdname   = att.get.ncdf(coarsenc, "time", "standard_name")$value
timeaxis      = att.get.ncdf(coarsenc, "time",          "axis")$value
timelongname  = att.get.ncdf(coarsenc, "time",     "long_name")$value

# should check to see if the coarseunits match the intent of the coeffs
# TIM - next generation of coefficient file must have intended units.

ncoarselons   = length(coarselons)
ncoarselats   = length(coarselats)
ncoarsetimes  = length(coarsetimes)
ntimeindarray = (1:ncoarsetimes)

# get an idea of the size of the grid cells 

meandx = mean(diff(coarselons))
meandy = mean(diff(coarselats))

# use an expanded domain - NOT

meandx = 0
meandy = 0

lftmost = preddomain$WESN[1] - 2*meandx
rhtmost = preddomain$WESN[2] + 2*meandx
botmost = preddomain$WESN[3] - 2*meandy
topmost = preddomain$WESN[4] + 2*meandy

loninds = (1:ncoarselons)[(coarselons >= lftmost) & (coarselons <= rhtmost)]
latinds = (1:ncoarselats)[(coarselats >= botmost) & (coarselats <= topmost)]

ncoarselonconus = length(loninds)
ncoarselatconus = length(latinds)  
   
coarsegrid = expand.grid(lon = coarselons[loninds], lat = coarselats[latinds])
Ngridcells = ncoarselonconus * ncoarselatconus

cat(paste("There are",Ngridcells,"coarse grid cells over the continental US"),"\n")

#---------------------------------------------------------------------
# Because we want to distribute the work among many jobs, we will grab
# just the data for a range of years. 
#---------------------------------------------------------------------

monarr   = (1:12)
dates    = expand.grid(months=monarr,years=yeararr)
timeind  = ntimeindarray[(dates$years >= year1) & (dates$years <= yearN)]
Ntimeind = length(timeind)

rm(ntimeindarray)

start          = c(     loninds[1],      latinds[1], timeind[1] )
count          = c(length(loninds), length(latinds),   Ntimeind )
datmat         = get.var.ncdf(coarsenc,coarsevar,start,count)
coarsetimebnds = get.var.ncdf(coarsenc,'time_bnds',c(1,timeind[1]),c(-1,Ntimeind))

# The time in this dataset is new to me. No leap years.
# the time_bnds seem to contain the dates of the averaging period.
# The 'time' seems to contain the average of the two.

if ( coarsetimebnds[1,1] != 365*year1) {
   stop("ERROR ... times not what we expect them to be.\n")
}

# subset to just the time we are interested in.

coarsetimes   = coarsetimes[timeind[1]:(timeind[1]+Ntimeind-1)]

ntimes = (yearN - year1 + 1)*12
if ( length(coarsetimes) != ntimes ){
   str1 = sprintf('(%d %d %d %d)',year1,yearN,length(coarsetimes),ntimes)
   stop(sprintf('ERROR ... length of time %s unexpected',str1))
}

#---------------------------------------------------------------------
# create output storage
#---------------------------------------------------------------------

val = list( lons           = prism$lon, 
            lats           = prism$lat, 
	    time           = coarsetimes,
            timeunits      = timeunits,
            timebnds       = coarsetimebnds,
            timecalendar   = timecalendar,
            timestdname    = timestdname,
            timeaxis       = timeaxis,
            timelongname   = timelongname,
	    year1          = year1,
            yearN          = yearN,
	    varname        = coarsevar,
	    varunits       = coarseunits,
	    varlongname    = coarseunits,
	    data     = array(NA,c(prism$nlon,prism$nlat,Ntimeind)), 
	    parentfname    = coarsefile,
	    parentvar      = coarsevar, 
            parentvarunits = coarseunits,
            standardname   = coarsestdname,
            longname       = coarselngname,
            parentlons     = coarselons[loninds],
	    parentlats     = coarselats[latinds],
            parentdata     = datmat,
            coeffname      = fnamecoeff )

#---------------------------------------------------------------------
# expand.grid strings the grid into a state-space form, with longitude
# varying fastest. Creating the vector of dependent variables means
# stringing out the coarsedata similarly. 
#---------------------------------------------------------------------

timeindex = rep(1:12,ntimes/12)

for (i in 1:ntimes){

   cat(paste("Working on timestep ",i,"of",ntimes),"\n")

   coarsedata = datmat[,,i] 
   yg         = c(coarsedata)
   splinefit  = Tps(x=coarsegrid,Y=yg) 
   psurf      = predict.surface(splinefit, grid=list(x=prism$lon,y=prism$lat),
                   extrap=TRUE, lambda=0)

   val$data[,,i] = psurf$z

}

return(val)

}

#---------------------------------------
# Here is the procedure
#---------------------------------------

   source("/fs/image/home/thoar/downscaling/GetPredDomain.r")
   source("/fs/image/home/thoar/downscaling/SimpleRDataToNetCDF.r")

   library(fields)
   library(ncdf)

   dircoeff   = "/ptmp/thoar/downscaling"
   ddir       = "/ptmp/thoar/downscaling/TAS"
   coarsefile = "tas_A1.20C3M_1.CCSM.atmm.1870-01_cat_1999-12.nc"
   ofname     = "tas_A1.20C3M_1.CCSM.atmm.1896-01_cat_1896-12.splined.nc"

   var = CoeffsEveryToRData( year1=1896, yearN=1896, prismvar='tave',
         coarsevar='tas', coarsefile=coarsefile, ddir=ddir, 
         dircoeff=dircoeff, yeararr=(1870:1999) )

   bob = SimpleRDataToNetCDF(var  = var,
                             ddir = ".",
                             file = ofname)
