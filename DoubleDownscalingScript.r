library(fields)
library(ncdf)

source("/glade/u/home/lenssen/downscaling/GetPredDomain.r")
source("/glade/u/home/lenssen/downscaling/ConvertUnits.r")

plots <- TRUE

nyears <- 1
year1 <- 2006 
yearN <- 2010
prismvar <- "tave"
coarsevar <- "tas"
coarsefile <- "tas_Amon_CCSM4_rcp26_r1i1p1_200601-210012.nc"
ddir <- "/glade/p/work/lenssen/AR5/TAS/"
dircoeff <- "/glade/p/work/lenssen/output/"
yeararr <- 2006:2100

###
# From CoeffsEveryToRData.r to downscale a year for the first time
###
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
coarsetimes   = coarsetimes[timeind[1]:(timeind[1]+Ntimeind-1)]


if(plots){
   dev.new()
   set.panel(3,4)
   for(i in 1:12){
      image.plot(coarselons[loninds],coarselats[latinds],datmat[,,i],main="OG data")
      US(add=T,shift=T,col="white")
   }
}


# Manually forced to the nyear var
ntimes = 12 * nyears

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
            data     = array(NA,c(prism$nlon,prism$nlat,ntimes)),
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

   daGrid <- list(x=prism$lon,y=prism$lat)

   psurf <- predict(tpsObject, x=prismLocLand,
                            yM=c(coarsedata), lambda=0)

   tempMat2 <- matrix(NA,length(prism$lon),length(prism$lat))
   tempMat2[prismLandIndicator] <- psurf
   psurf <- tempMat2

   #------------------------------------------------------------------
   # Get the coefficients for this month, and apply
   #------------------------------------------------------------------

   imon  = timeindex[i]
   start = c( 1,  1, imon)
   count = c(-1, -1,    1)
   m     = coefficients$m[,,imon]
   b     = coefficients$b[,,imon]

   val$data[,,i] = psurf*m + b 
}

firstDS <- val$data

if(plots){
   dev.new()
   set.panel(3,4)
   for(i in 1:12){
      image.plot(val$lon,val$lat,firstDS[,,i],main="FirstDS")
      US(add=T,shift=T,col="white")
   }
}

###
# Now switch over to setup GenerateCoeffsV2.r to aggregate firstDS back down
# Note that this is not optimized and may be quite redundant. I am trying
# To reduce human hours for this process
###

fname <- "/glade/p/work/lenssen/output/tave_1895_2005.nc"
prismvar <- "tave"
ddir <- "/glade/p/work/lenssen/AR5"

prismnc    = open.ncdf(fname)
prismlons  = get.var.ncdf(prismnc, "lon")
prismlats  = get.var.ncdf(prismnc, "lat")
prismtime  = get.var.ncdf(prismnc, "time")
prismunits = att.get.ncdf(prismnc, prismvar, "units")$value

topmost   = att.get.ncdf(prismnc,0,"North_Bounding_Coordinate")$value
botmost   = att.get.ncdf(prismnc,0,"South_Bounding_Coordinate")$value
lftmost   = att.get.ncdf(prismnc,0, "West_Bounding_Coordinate")$value
rhtmost   = att.get.ncdf(prismnc,0, "East_Bounding_Coordinate")$value

nlons     = length(prismlons)
nlats     = length(prismlats)

nlonsindarray = (1:nlons)
nlatsindarray = (1:nlats)

#---------------------------------------------------------------------
# Get the GCM grid from the netCDF file, subset to conUS
# retain the original GCM numbering [288,192]
#---------------------------------------------------------------------

gcmfname = "TAS/tas_Amon_CCSM4_rcp26_r1i1p1_200601-210012.nc"
gcmnc    = open.ncdf(paste(ddir,gcmfname,sep="/"))
gcmlons  = get.var.ncdf(gcmnc, "lon")
gcmlats  = get.var.ncdf(gcmnc, "lat")
gcmtimes = get.var.ncdf(gcmnc, "time")
gcmunits = att.get.ncdf(gcmnc, "tas", "units")$value
close.ncdf(gcmnc)

ngcmlons = length(gcmlons)
ngcmlats = length(gcmlats)

loninds = (1:ngcmlons)[(gcmlons >= lftmost) & (gcmlons <= rhtmost)]
latinds = (1:ngcmlats)[(gcmlats >= botmost) & (gcmlats <= topmost)]

ngcmlonconus = length(loninds)
ngcmlatconus = length(latinds)

gcmgrid = expand.grid(lon = gcmlons[loninds], lat = gcmlats[latinds])
gcminds = expand.grid(lonind = loninds,             latind = latinds)
gcmgrid = cbind(gcmgrid,gcminds)
gcminds = expand.grid(gcmxi = (1:ngcmlonconus),gcmyj = (1:ngcmlatconus))
gcmgrid = cbind(gcmgrid,gcminds)
rm(gcminds)

Ngridcells = nrow(gcmgrid)
gcmdx      = mean(diff(gcmlons[loninds]))
gcmdy      =      diff(gcmlats[latinds])

cat(paste("There are",Ngridcells,"GCM grid cells over the continental US"),"\n")

prism = list(means = array(NA, c(ngcmlonconus,
                                ngcmlatconus,
                                length(prismtime))),
           gcmlons    = gcmlons[loninds],
           gcmlats    = gcmlats[latinds],
           gcmloninds = loninds,
           gcmlatinds = latinds,
           lons       = prismlons,
           lats       = prismlats,
           times      = prismtime,
           varname    = prismvar,
           units      = prismunits,
           m          = array(NA,c(nlons,nlats,12)), # slope
           b          = array(NA,c(nlons,nlats,12)), # intercept
           sse        = array(NA,c(nlons,nlats,12)), # sum(resid^2)
           gcmN       = array(NA,c(nlons,nlats,12)), # count
           gcmxi      = array(NA,c(nlons,nlats)),    # applicable gcmlon index
           gcmyj      = array(NA,c(nlons,nlats)),    # applicable gcmlat index
           gcmlon     = array(NA,c(nlons,nlats)),    # applicable gcmlon
           gcmlat     = array(NA,c(nlons,nlats)),    # applicable gcmlat
           aggStats   = NA)
# implicitly relies on prismtime(1) == january
timeindex = rep(1:12,length(prismtime)/12)

cellcount = 0

firstDSAgg <- array(NA, dim=c(length(loninds),length(latinds),nyears*12))
# Now aggregate firstDS
for(t in 1:(dim(firstDS)[3])){
   for(i in 1:Ngridcells){
      targetlon = gcmgrid$lon[i]
      targetlat = gcmgrid$lat[i]
      gcmxi     = gcmgrid$gcmxi[i]
      gcmyj     = gcmgrid$gcmyj[i]

      x1 = targetlon - gcmdx/2.0
      x2 = targetlon + gcmdx/2.0
      y1 = targetlat - max(gcmdy)/2.0
      y2 = targetlat + min(gcmdy)/2.0

      #------------------------------------------------------------------
      # Find the prism neighbor locations corresponding to the 
      # particular GCM (prediction) locations.
      #------------------------------------------------------------------

      loninds = nlonsindarray[(prismlons < x2) & (prismlons >= x1)]
      latinds = nlatsindarray[(prismlats < y2) & (prismlats >= y1)]
      pgrid   = expand.grid(loninds=loninds, latinds=latinds)

      # Get the prism data corresponding to this prediction location.

      # start   = c(      loninds[1],      latinds[1], t )
      # count   = c( length(loninds), length(latinds), 1 )
      datmat  = firstDS[loninds,latinds,t]
    
      # sometimes, datmat is NA ... need to skip these
      # help(Control)   to find info on 'for'
      # use 'next' to halt the processing of the current iteration and 
      # advance the looping index.

      if ( ! any(is.finite(datmat)) ) next

      firstDSAgg[gcmxi,gcmyj,t] <- mean(datmat,na.rm=TRUE)
   }
}

if(plots){
   dev.new()
   set.panel(3,4)
   for(i in 1:12){
      image.plot(prism$gcmlons,prism$gcmlats,firstDSAgg[,,i],main="firstDSAgg")
      US(add=T,shift=T,col="white")
   }
}

###
# Now downscale again
###
nyears <- 1
year1 <- 2006
yearN <- 2010
prismvar <- "tave"
coarsevar <- "tas"
coarsefile <- "tas_Amon_CCSM4_rcp26_r1i1p1_200601-210012.nc"
ddir <- "/glade/p/work/lenssen/AR5/TAS/"
dircoeff <- "/glade/p/work/lenssen/output/"
yeararr <- 2006:2100

###
# From CoeffsEveryToRData.r to downscale a year for the first time
###
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
datmat         = firstDSAgg
coarsetimebnds = get.var.ncdf(coarsenc,'time_bnds',c(1,timeind[1]),c(-1,Ntimeind))
coarsetimes   = coarsetimes[timeind[1]:(timeind[1]+Ntimeind-1)]

# Manually forced to the nyear var
ntimes = 12 * nyears

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
            data     = array(NA,c(prism$nlon,prism$nlat,ntimes)),
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
gcmLandIndicator <- !is.na(datmat[,,12])
gcmLocLand <- as.matrix(expand.grid(coarselons[loninds],
                        coarselats[latinds]))[gcmLandIndicator,]

# Run the Tps
subTempMat <- matrix(firstDSAgg[,,1])[gcmLandIndicator]
tpsObject <- Tps(x=gcmgrid[gcmLandIndicator,1:2],Y=subTempMat,lambda=0)


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

   daGrid <- list(x=prism$lon,y=prism$lat)

   psurf <- predict(tpsObject, x=prismLocLand,
                            yM=c(coarsedata)[gcmLandIndicator], lambda=0)

   tempMat2 <- matrix(NA,length(prism$lon),length(prism$lat))
   tempMat2[prismLandIndicator] <- psurf
   psurf <- tempMat2

   #------------------------------------------------------------------
   # Get the coefficients for this month, and apply
   #------------------------------------------------------------------

   imon  = timeindex[i]
   start = c( 1,  1, imon)
   count = c(-1, -1,    1)
   m     = coefficients$m[,,imon]
   b     = coefficients$b[,,imon]

   val$data[,,i] = psurf*m + b 
}
secondDS <- val$data

if(plots){
   dev.new()
   set.panel(3,4)
   for(i in 1:12){
      image.plot(val$lon,val$lat,secondDS[,,i],main="SecondDS")
      US(add=T,shift=T,col="white")
   }
}

diffDS <- secondDS-firstDS

if(plots){
   dev.new()
   set.panel(3,4)
   for(i in 1:12){
      image.plot(val$lon,val$lat,diffDS[,,i],main="DiffDS")
      US(add=T,shift=T,col="white")
   }
}



