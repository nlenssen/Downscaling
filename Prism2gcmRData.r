"Prism2gcmRData" <- function ( ddir = "/fs/image/home/thoar/downscaling",
     prismdir="/ptmp/thoar/downscaling/prism100", locator = FALSE,
     prismvar="tmin", gcmvar="tas", year1=1895, yearN=1899 )
{
#
# $Id: Prism2gcmRData.r,v 1.11 2008/04/24 03:57:35 thoar Exp $
#
# Generate GCM-equivalent spatial field from prism dataset.
#
#---------------------------------------------------------------------
# Get the GCM grid  [0,360],[-90 90]
#---------------------------------------------------------------------

gcmfname = sprintf(ddir,"%s/gcm/%s_A1.SRESA1B_1.CCSM.atmm.2000-01_cat_2099-12.nc",ddir,gcmvar)
gcmnc    = open.ncdf(gcmfname) 
gcmlons  = get.var.ncdf(gcmnc, "lon")
gcmlats  = get.var.ncdf(gcmnc, "lat")
gcmtimes = get.var.ncdf(gcmnc, "time")
gcmunits = att.get.ncdf(gcmnc, gcmvar, "units")$value
close.ncdf(gcmnc)

ngcmlons = length(gcmlons)
ngcmlats = length(gcmlats)

#---------------------------------------------------------------------
# Get prism data, but leave file open.
#---------------------------------------------------------------------

fname      = sprintf("%s_%d_%d.nc",prismvar,year1,yearN)
prismfname = paste(prismdir,fname,sep="/")
prismnc    = open.ncdf(prismfname) 

prismlons  = get.var.ncdf(prismnc, "lon")
prismlats  = get.var.ncdf(prismnc, "lat")
nprismlons = length(prismlons)
nprismlats = length(prismlats)

prismvarunits     = att.get.ncdf(prismnc, prismvar, "units")$value
prismvarstdname   = att.get.ncdf(prismnc, prismvar, "standard_name")$value
prismvarlongname  = att.get.ncdf(prismnc, prismvar, "long_name")$value
prismtime         = get.var.ncdf(prismnc, "time")
prismtimeunits    = att.get.ncdf(prismnc, "time", "units")$value
prismtimeorigin   = att.get.ncdf(prismnc, "time", "origin")$value
prismtimeexample  = att.get.ncdf(prismnc, "time", "example1")$value
prismtimebnds     = get.var.ncdf(prismnc, "time_bnds")
prismtimecalendar = "gregorian"
prismtimestdname  = "time"
prismtimelongname = "time"
prismtimeaxis     = "T"
nprismtime        = length(prismtime)

northern   = att.get.ncdf(prismnc,0,"North_Bounding_Coordinate")$value
southern   = att.get.ncdf(prismnc,0,"South_Bounding_Coordinate")$value
western    = att.get.ncdf(prismnc,0, "West_Bounding_Coordinate")$value
eastern    = att.get.ncdf(prismnc,0, "East_Bounding_Coordinate")$value

prismlonsindarray = (1:nprismlons)
prismlatsindarray = (1:nprismlats)

Nprismcells = nprismlons * nprismlats

cat(paste("There are",Nprismcells,"prism grid cells over the continental US"),"\n")

#---------------------------------------------------------------------
# GCM data are in degrees Kelvin, the prism data is degrees celsius.
# All other units are unchanged.
#---------------------------------------------------------------------
if (prismvarunits == 'mm'){
   offset = 0.0
   scale  = 1.0
   units  = prismvarunits
}else{
   bob    = ConvertUnits(from=prismvarunits,to=gcmunits)
   offset = bob$offset
   scale  = bob$scale
   units  = bob$units
   rm(bob)
}

#---------------------------------------------------------------------
# Subset the GCM grid to the Continental US (prism region)
#---------------------------------------------------------------------

loninds = (1:ngcmlons)[(gcmlons >=  western)&(gcmlons <=  eastern)]
latinds = (1:ngcmlats)[(gcmlats >= southern)&(gcmlats <= northern)]

 gcmlonconus = gcmlons[loninds]
 gcmlatconus = gcmlats[latinds]
ngcmlonconus = length(gcmlonconus)
ngcmlatconus = length(gcmlatconus)

gcmgrid = expand.grid(lon = gcmlonconus, lat = gcmlatconus)
gcminds = expand.grid(gcmxi = (1:ngcmlonconus),gcmyj = (1:ngcmlatconus))
gcmgrid = cbind(gcmgrid,gcminds)
rm(gcminds)

Ngcmcells = nrow(gcmgrid)
gcmdx     = mean(diff(gcmlonconus))
gcmdy     =      diff(gcmlatconus)

if (prismvarunits == 'mm'){
   gcmdx     = 2.0*gcmdx
   gcmdy     = 2.0*gcmdy
}else{
   gcmdx     = 1.0*gcmdx
   gcmdy     = 1.0*gcmdy
}

cat(paste("There are",Ngcmcells,"GCM grid cells over the continental US"),"\n")

#---------------------------------------------------------------------
# create output structure
#---------------------------------------------------------------------

prism = list(data = array(NA, c(ngcmlonconus,
                                ngcmlatconus,
                                nprismtime)),
     gcmN         = array(NA,c(ngcmlonconus,ngcmlatconus,nprismtime)),
     year1        = year1,
     yearN        = yearN,
     lons         = gcmlonconus,
     lats         = gcmlatconus,
     time         = as.single(prismtime),
     timebnds     = as.single(prismtimebnds),
     timeunits    = prismtimeunits,
     timecalendar = prismtimecalendar,
     timestdname  = prismtimestdname,
     timelongname = prismtimelongname,
     timeexample  = prismtimeexample,
     timeaxis     = prismtimeaxis,
     parentfname  = prismfname,
     parentlons   = prismlons,
     parentlats   = prismlats,
     parentvar    = prismvar,
     parentvarunits = units,
     varname      = prismvar,
     standardname = prismvarstdname,
     longname     = prismvarlongname,
     varunits     = units)

# implicitly relies on prismtime(1) == january
timeindex = rep(1:12,length(prismtime)/12)
cellcount = 0

#---------------------------------------------------------------------
# Loop over all possible GCM locations and create
# the prism equivalent for each GCM location. 
#
# i =   1 is full of nothing.
# i = 190 is on the Mississippi coast.
# i = 430 is right in the middle of Kansas.
# i = 739 is about Vancouver Island ... has ONE prism cell
#---------------------------------------------------------------------

for (i in 1:Ngcmcells){

   cellcount = cellcount + 1
   cat(paste("Working on cell ",cellcount,"of",Ngcmcells),"\n")

   # Find the prism neighbor locations corresponding to the 
   # particular GCM (prediction) locations.

   targetlon = gcmgrid$lon[i]
   targetlat = gcmgrid$lat[i]
   gcmxi     = gcmgrid$gcmxi[i]
   gcmyj     = gcmgrid$gcmyj[i]

   x1 = targetlon - gcmdx/2.0
   x2 = targetlon + gcmdx/2.0
   y1 = targetlat - max(gcmdy)/2.0
   y2 = targetlat + min(gcmdy)/2.0

   loninds = prismlonsindarray[(prismlons < x2) & (prismlons >= x1)]
   latinds = prismlatsindarray[(prismlats < y2) & (prismlats >= y1)]

   # Get the prism data corresponding to this prediction location.

   start   = c(      loninds[1],      latinds[1],                 1 )
   count   = c( length(loninds), length(latinds), length(prismtime) )
   datmat  = get.var.ncdf(prismnc, prismvar, start, count)*scale + offset

   prism$gcmlons[gcmxi] = targetlon
   prism$gcmlats[gcmyj] = targetlat

   # sometimes, datmat is NA ... need to skip these

   if ( ! any(is.finite(datmat))      ) next

   ngood = sum(is.finite(datmat))/length(prismtime) # on average
#  if ( ngood < 20 ) next

   # Form the spatial mean(as a function of time) -- this is the
   # prism estimate of the value for the gcm grid cell. 

   prismmeans = apply (datmat, 3, mean, na.rm=TRUE)
   prismndata = apply (datmat, 3, is.finite)

   # TJH Sep 5 2007
   # Instead of a log transform, we're going with a cube-root filter
   # AFTER the spline portion, but before the linear model fit.

   # If these are precip amounts, transform with log()
   #if (prismvarunits == 'mm*100'){
   #   prism$varunits = 'log(mm*100)'
   #   zeroindices = prismmeans <= 0.0
   #   prismmeans = log(prismmeans)
   #   prismmeans[zeroindices] = 0.0
   #   infindices = is.infinite(prismmeans)
   #   numinfs = sum(infindices)
   #   if (numinfs > 0 ){
   #     cat(sprintf('cat - There were %d infinite vals',numinfs))
   #     warning(sprintf('There were %d infinite vals',numinfs))
   #   }
   #}

   prism$data[gcmxi,gcmyj,] = prismmeans
   prism$gcmN[gcmxi,gcmyj,] = colSums(prismndata)

   if ( locator ){

      ndevices = length(dev.list())
      if (ndevices < 4){ for (idev in (ndevices+1):4) x11() }
   
      dev.set(2)
      plot(targetlon,targetlat,xlim=c(235,293),ylim=c(24,50))
      lines(x=c(x1,x2,x2,x1,x1),y=c(y1,y1,y2,y2,y1),type='l',lty=1,lwd=2,col='red')
      points(gcmlonconus[gcmxi],gcmlatconus[gcmyj],pch=19)
      US(add=TRUE,shift=TRUE)
      title('Region being estimated')

      dev.set(3)
      image.plot(prismlons[loninds], prismlats[latinds], datmat[,,1])
      title('Data subsetted through start,count')

      dev.set(4)
      bob   = get.var.ncdf(prismnc, prismvar)*scale + offset
      image.plot(prismlons[loninds], prismlats[latinds], bob[loninds,latinds,1])
      title('Data obtained as a whole and then subsetted')

      dev.set(5)
      plot(prismmeans,xlab="month")
      lines(prismmeans)
      title(sprintf('Mean %s as a function of time',prismvar))

   }
}

close.ncdf(prismnc)

return(prism)

}
