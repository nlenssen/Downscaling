"CoeffsEveryToRData" <- function ( year1=2000, yearN=2009, 
     prismvar="tave", coarsevar="tas",
     coarsefile="tas_A1.SRESA1B_1.CCSM.atmm.2000-01_cat_2099-12.nc", 
     ddir="/fs/image/home/thoar/downscaling/gcm",
     dircoeff="/project/gsp/thoar/downscaling",
     yeararr=(2000:2099) )
{
#
#   $Id: CoeffsEveryToRData.r,v 1.17 2008/04/24 03:57:35 thoar Exp $
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

subset = FALSE


cat(sprintf("year1      is %d\n",year1))
cat(sprintf("yearN      is %d\n",yearN))
cat(sprintf("prismvar   is %s\n",prismvar))
cat(sprintf("coarsevar  is %s\n",coarsevar))
cat(sprintf("coarsefile is %s\n",coarsefile))
cat(sprintf("ddir       is %s\n",ddir))
cat(sprintf("dircoeff   is %s\n",dircoeff))

library(ncdf)
library(fields)


# Debugging feature
debug = FALSE
print(paste("Debugging:",debug))


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

# This was written when the coefficients were in NetCDF format
# We now have them in RData and must adjust accordingly
#
# fnamecoeff  = sprintf("%s/prism_%s.RData",dircoeff,prismvar)
# coeffnc     = open.ncdf(fnamecoeff, write=FALSE)
# nctimes     = get.var.ncdf(coeffnc, "time")
# nccalendar  = att.get.ncdf(coeffnc, "time", "calendar") 
# nclat       = get.var.ncdf(coeffnc, 'lat')
# nclon       = get.var.ncdf(coeffnc, 'lon')
# targetvar   = att.get.ncdf(coeffnc, 0, 'target_variable_name')$value
# targetunits = att.get.ncdf(coeffnc, 0, 'target_variable_units')$value

# Currently hardcoded for testing
fnamecoeff  = sprintf("%s/NL_TPS_prism_AR5_%s.RData",dircoeff,prismvar)
# fnamecoeff = sprintf("%s/test.RData",dircoeff)
load(fnamecoeff)
nctimes     = coefficients$times
# nccalendar never used
nclat       = coefficients$lats
nclon       = coefficients$lons
targetvar   = coefficients$varname
targetunits = coefficients$units

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

preddomain    = GetPredDomain(ddir,
                              "/glade/p/work/lenssen/output/tave_1895_1899.nc")

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

#---------------------------------------------------------------------
# GCM data are in degrees Kelvin, the PRISM data is degrees celsius.
# GCM precip are fluxes, PRISM data are 'mm'
#---------------------------------------------------------------------
bob      = ConvertUnits(from=coarseunits,to=targetunits)
offset   = bob$offset
scale    = bob$scale
rm(bob)

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
onethird = 1.0/3.0
monarr   = (1:12)
dates    = expand.grid(months=monarr,years=yeararr)
timeind  = ntimeindarray[(dates$years >= year1) & (dates$years <= yearN)]
Ntimeind = length(timeind)

rm(ntimeindarray)

start          = c(     loninds[1],      latinds[1], timeind[1] )
count          = c(length(loninds), length(latinds),   Ntimeind )
datmat         = get.var.ncdf(coarsenc,coarsevar,start,count) # *scale + offset
coarsetimebnds = get.var.ncdf(coarsenc,'time_bnds',c(1,timeind[1]),c(-1,Ntimeind))

# The time in this dataset is new to me. No leap years.
# the time_bnds seem to contain the dates of the averaging period.
# The 'time' seems to contain the average of the two.

if ( coarsetimebnds[1,1] != 365*year1) {
#   stop("ERROR ... times not what we expect them to be.\n")
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

if (prismvar == "ppt") {
   varunits     = "mm"
   varlongname  = "precipitation_amount"
   standardname = "precipitation_amount"
} else {
   varunits     = targetunits
   varlongname  = coarselngname
   standardname = coarsestdname
}

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
	    varunits       = varunits,
	    varlongname    = varlongname,
            standardname   = standardname,
	    data     = array(NA,c(prism$nlon,prism$nlat,Ntimeind)), 
	    parentfname        = coarsefile,
	    parentvar          = coarsevar, 
            parentvarunits     = varunits,  # changed at get.var.ncdf()
            parentstandardname = coarsestdname,
            parentlongname     = coarselngname,
            parentlons         = coarselons[loninds],
	    parentlats         = coarselats[latinds],
            parentdata         = datmat,
            coeffname          = fnamecoeff )

#---------------------------------------------------------------------
# expand.grid strings the grid into a state-space form, with longitude
# varying fastest. Creating the vector of dependent variables means
# stringing out the coarsedata similarly. 
#---------------------------------------------------------------------

timeindex = rep(1:12,ntimes/12)

# Build a land indicator for prism
prismLandIndicator <- !is.na(coefficients$m[,,1])
prismLocLand <- prismLocLand <- as.matrix(expand.grid(
                                prism$lon,prism$lat))[prismLandIndicator,]

# Build a gcm land indicator
gcmLandIndicator <- !is.na(datmat[,,1])
gcmLocLand <- as.matrix(expand.grid(coarselons[loninds],
                        coarselats[latinds]))[gcmLandIndicator,]

# Run the Tps
tpsObject <- Tps(x=coarsegrid, Y=c(datmat[,,1]), lambda=0)

if(debug) {
  ntimes = 1
}

for (i in 1:ntimes){

   cat(paste("Working on timestep ",i,"of",ntimes),"\n")

   if (coarseunits == "kg m-2 s-1"){
      # Must convert precip from "kg m-2 s-1" to "mm"
      # (i.e. precipitation_flux) to precipitation amounts.
      # From Gary Strand: "Take the original values in kg m-2 s-1, 
      # multiply by s month-1 (either 28, 30, or 31 * 24 * 3600) to get mm month-1
      # (Note that dividing the original values by the density of
      # water (1000 kg m-3) gives m s-1, and multiplying by 1000
      # gives mm s-1; the numerical values of these two steps cancel.)"
      ndays      = coarsetimebnds[2,i] - coarsetimebnds[1,i] 
      coarsedata = datmat[,,i] * 3600 * 24 * ndays
      myunits = 'mm'
   } else {
      coarsedata = datmat[,,i] 
      myunits = varunits
   }

   # A switch that allows running the code on a small spatial subset of
   # the data   
   if(subset){
      prismlons <- prism$lon
      prismlats <- prism$lat
      nlons     <- length(prismlons)
      nlats     <- length(prismlats)

      nlonsindarray = (1:nlons)
      nlatsindarray = (1:nlats)

      sublons <- prismlons[(prismlons<242) & (prismlons>240)]
      sublats <- prismlats[(prismlats< 37) & (prismlats> 35)]
      sublonsinds <- nlonsindarray[(prismlons<242) & (prismlons>240)]
      sublatsinds <- nlatsindarray[(prismlats< 37) & (prismlats> 35)]

      daGrid <- list(x=sublons,y=sublats)

   } else{
      daGrid <- list(x=prism$lon,y=prism$lat)
   
   }

   psurf <- predict(tpsObject, x=prismLocLand,
			    yM=c(coarsedata), lambda=0)

   tempMat2 <- matrix(NA,length(prism$lon),length(prism$lat))
   tempMat2[prismLandIndicator] <- psurf
   psurf <- tempMat2
   
   if(debug){
     set.panel(1,2)
     image.plot(coarselons[loninds],coarselats[latinds],coarsedata)
     US(add=TRUE,shift=TRUE)

     image.plot(x=prism$lon,y=prism$lat,psurf)
     US(add=TRUE,shift=TRUE)
   }
   #------------------------------------------------------------------
   # Get the coefficients for this month, and apply
   #------------------------------------------------------------------

   imon  = timeindex[i]
   start = c( 1,  1, imon)
   count = c(-1, -1,    1)
   m     = coefficients$m[,,imon]
   b     = coefficients$b[,,imon]
   if (myunits == 'mmm'){

      #---------------------------------------------------------------
      # the spline can result in negative amounts ... not realistic
      #---------------------------------------------------------------

      norainlocs = psurf < 0.0
      psurf[norainlocs] = 0.0

      #---------------------------------------------------------------
      # Must convert precip from "mm" to cuberoot. This entails
      # masking off bits, apply the regression in transformed space, 
      # untransforming and correcting for the expected bias.
      # If this results in dopey totals, threshhold.
      #---------------------------------------------------------------

      N   = coefficients$gcmN[,,imon]
      sse = coefficients$sse[,,imon]

      bias = matrix( 0,nrow=nrow(N),ncol=ncol(N))
      xhat = matrix(NA,nrow=nrow(N),ncol=ncol(N))
      observedlocations = N > 0

      xhat[observedlocations] = psurf[observedlocations]
      xhat = (xhat^onethird)*m + b

      bias[observedlocations] = sse[observedlocations]/(N[observedlocations]-2)
      bias = 3.0*xhat*bias

      xhat = xhat^3 + bias

      inds = is.finite(xhat) & xhat < 1.0
      if (sum(inds) > 0){
         cat(sprintf('month %2d has %6d locations below 1.0mm \n',i,sum(inds)))
      }
      xhat[inds] = 0.0
      val$data[,,i] = xhat

   #  GIS people want precip amounts, actually.
   #  if (coarseunits == "kg m-2 s-1"){
   #     # Must convert precip from "mm" back to "kg m-2 s-1"
   #     # (i.e. precipitation amounts to precipitation_flux) 
   #     ndays      = coarsetimebnds[2,i] - coarsetimebnds[1,i] 
   #     val$data[,,i] = val$data[,,i] / 3600 * 24 * ndays
   #     myunits = 'kg m-2 s-1'
   #  }

   } else {
      val$data[,,i] = psurf*m + b + offset
   }

}

return(val)

}
