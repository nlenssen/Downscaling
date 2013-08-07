"ExploreArtifacts" <- function ( ddir="/ptmp/thoar/downscaling/prism100",
     prismfile="ppt_1895_2005.nc", 
     pvar="ppt", year1=1895, yearN=2005 )
{
#
# $Id: ExploreArtifacts.r,v 1.3 2007/10/11 18:06:33 thoar Exp $
#
# The RMSE fields of (PRISM - SPLINED(AGGREGATED(PRISM)) show some
# latitudinal 'ringing' that is unexpected. Must explore where it is
# coming from.
#

#---------------------------------------------------------------------
# Create the variance by month for the raw prism data.
#---------------------------------------------------------------------

prismfname = sprintf("%s/%s",ddir,prismfile)
pnc           = open.ncdf(prismfname)
plons         = get.var.ncdf(pnc, "lon") 
plats         = get.var.ncdf(pnc, "lat")
punits        = att.get.ncdf(pnc, pvar,           "units")$value
ptimes        = get.var.ncdf(pnc, "time")
ptimeunits    = att.get.ncdf(pnc, "time",         "units")$value
ptimecalendar = att.get.ncdf(pnc, "time",      "calendar")$value
ptimestdname  = att.get.ncdf(pnc, "time", "standard_name")$value
ptimeaxis     = att.get.ncdf(pnc, "time",          "axis")$value
ptimelongname = att.get.ncdf(pnc, "time",     "long_name")$value
ptimeexample  = att.get.ncdf(pnc, "time",       "example")$value
ptimebnds     = get.var.ncdf(pnc, "time_bnds")

nlons         = length(plons)
nlats         = length(plats)
ntimes        = length(ptimes)

yeararr  = (year1:yearN)
monarr   = (1:12)
dates    = expand.grid(months=monarr,years=yeararr)
timeind  = (1:ntimes)[(dates$years >= year1) & (dates$years <= yearN)]
Ntimeind = length(timeind)

jan  = (1:ntimes)[(dates$months ==  1)]
feb  = (1:ntimes)[(dates$months ==  2)]
mar  = (1:ntimes)[(dates$months ==  3)]
apr  = (1:ntimes)[(dates$months ==  4)]
may  = (1:ntimes)[(dates$months ==  5)]
jun  = (1:ntimes)[(dates$months ==  6)]
jul  = (1:ntimes)[(dates$months ==  7)]
aug  = (1:ntimes)[(dates$months ==  8)]
sep  = (1:ntimes)[(dates$months ==  9)]
oct  = (1:ntimes)[(dates$months == 10)]
nov  = (1:ntimes)[(dates$months == 11)]
dec  = (1:ntimes)[(dates$months == 12)]

#---------------------------------------------------------------------
# output data structure
#---------------------------------------------------------------------

bobo = list(
           datamean = array(NA, c(nlons, nlats, 12)),
           datastd  = array(NA, c(nlons, nlats, 12)),
           dataskew = array(NA, c(nlons, nlats, 12)),
           parentfname  = prismfname,
           year1        = year1,
           yearN        = yearN,
           nlons        = nlons,
           nlats        = nlats,
           ntimes       = 12,
           lons         = plons,
           lats         = plats,
           time         = (1:12),
           timeunits    = "month index",
           timeexample  = "Jan==1, Feb==2",
           varname      = pvar,
           varunits     = punits)

#---------------------------------------------------------------------
# Loop over all possible locations 
#---------------------------------------------------------------------
for (i in 601:1080){
   cat(sprintf('lon index %3d of %4d\n',i,nlons))

   start = c( i,     1,      1)
   count = c( 1, nlats, ntimes)
   y = get.var.ncdf(pnc, pvar, start=start, count=count)

   for (j in 1:nlats){
   
      x = y[j,]
      ngood = sum(is.finite(x))
      if ( ngood < ntimes ) next
    
      x[x < 0.0] = 0.0
   
      bobo$datamean[i,j, 1] = mean(x[jan],na.rm=T)
      bobo$datamean[i,j, 2] = mean(x[feb],na.rm=T)
      bobo$datamean[i,j, 3] = mean(x[mar],na.rm=T)
      bobo$datamean[i,j, 4] = mean(x[apr],na.rm=T)
      bobo$datamean[i,j, 5] = mean(x[may],na.rm=T)
      bobo$datamean[i,j, 6] = mean(x[jun],na.rm=T)
      bobo$datamean[i,j, 7] = mean(x[jul],na.rm=T)
      bobo$datamean[i,j, 8] = mean(x[aug],na.rm=T)
      bobo$datamean[i,j, 9] = mean(x[sep],na.rm=T)
      bobo$datamean[i,j,10] = mean(x[oct],na.rm=T)
      bobo$datamean[i,j,11] = mean(x[nov],na.rm=T)
      bobo$datamean[i,j,12] = mean(x[dec],na.rm=T)
   
      bobo$datastd[i,j, 1] = sqrt(var(x[jan],na.rm=T))
      bobo$datastd[i,j, 2] = sqrt(var(x[feb],na.rm=T))
      bobo$datastd[i,j, 3] = sqrt(var(x[mar],na.rm=T))
      bobo$datastd[i,j, 4] = sqrt(var(x[apr],na.rm=T))
      bobo$datastd[i,j, 5] = sqrt(var(x[may],na.rm=T))
      bobo$datastd[i,j, 6] = sqrt(var(x[jun],na.rm=T))
      bobo$datastd[i,j, 7] = sqrt(var(x[jul],na.rm=T))
      bobo$datastd[i,j, 8] = sqrt(var(x[aug],na.rm=T))
      bobo$datastd[i,j, 9] = sqrt(var(x[sep],na.rm=T))
      bobo$datastd[i,j,10] = sqrt(var(x[oct],na.rm=T))
      bobo$datastd[i,j,11] = sqrt(var(x[nov],na.rm=T))
      bobo$datastd[i,j,12] = sqrt(var(x[dec],na.rm=T))
   
      bobo$dataskew[i,j, 1] = skewness(x[jan],na.rm=T)
      bobo$dataskew[i,j, 2] = skewness(x[feb],na.rm=T)
      bobo$dataskew[i,j, 3] = skewness(x[mar],na.rm=T)
      bobo$dataskew[i,j, 4] = skewness(x[apr],na.rm=T)
      bobo$dataskew[i,j, 5] = skewness(x[may],na.rm=T)
      bobo$dataskew[i,j, 6] = skewness(x[jun],na.rm=T)
      bobo$dataskew[i,j, 7] = skewness(x[jul],na.rm=T)
      bobo$dataskew[i,j, 8] = skewness(x[aug],na.rm=T)
      bobo$dataskew[i,j, 9] = skewness(x[sep],na.rm=T)
      bobo$dataskew[i,j,10] = skewness(x[oct],na.rm=T)
      bobo$dataskew[i,j,11] = skewness(x[nov],na.rm=T)
      bobo$dataskew[i,j,12] = skewness(x[dec],na.rm=T)
   }
}

close.ncdf(pnc)

return(bobo)

}
