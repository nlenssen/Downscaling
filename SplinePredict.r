"SplinePredict" <- function ( ddir="/fs/image/home/thoar/downscaling",
     coarsefile="gcm/tas_A1.SRESA1B_1.CCSM.atmm.2000-01_cat_2099-12.nc", 
     coarsevar="tas", year1=1895, yearN=1899 )
{
#
# $Id: SplinePredict.r,v 1.5 2008/04/24 03:57:36 thoar Exp $
#
# Fit splines to the coarse data over the prism domain and then
# interpolates to the prism locations.
#
# ddir        directory where data reside
# coarsefile  string of netCDF file containing the low resolution data
# coarsevar   string of netCDF variable to interpolate
#
# Example:
# library(ncdf)
# library(fields)
# ddir = "/ptmp/thoar/downscaling/aggregated"
# coarsefile = "PrismAggregated_tave_1895_2005.nc"
# coarsefile = "prismAggregated_tave_1895_1899.nc"
# coarsevar  = "tave"
# bob = SplinePredict(ddir,coarsefile,coarsevar)

#---------------------------------------------------------------------
# Get the PRISM (high-resolution) domain
#---------------------------------------------------------------------

preddomain    = GetPredDomain()
nlonsindarray = (1:preddomain$nlons)
nlatsindarray = (1:preddomain$nlats)

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
coarseunits   = att.get.ncdf(coarsenc, coarsevar,        "units")$value
coarsesname   = att.get.ncdf(coarsenc, coarsevar,"standard_name")$value
coarselname   = att.get.ncdf(coarsenc, coarsevar,    "long_name")$value
coarsetimes   = get.var.ncdf(coarsenc, "time")
timeunits     = att.get.ncdf(coarsenc, "time",         "units")$value
timecalendar  = att.get.ncdf(coarsenc, "time",      "calendar")$value
timestdname   = att.get.ncdf(coarsenc, "time", "standard_name")$value
timeaxis      = att.get.ncdf(coarsenc, "time",          "axis")$value
timelongname  = att.get.ncdf(coarsenc, "time",     "long_name")$value
timeexample   = att.get.ncdf(coarsenc, "time",       "example")$value
timebnds      = get.var.ncdf(coarsenc, "time_bnds")

ncoarselons   = length(coarselons)
ncoarselats   = length(coarselats)
ncoarsetimes  = length(coarsetimes)

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
# Loop over all possible coarse locations and create
# the prism equivalent for each coarse location. 
#---------------------------------------------------------------------

hiresdata = list(data = array(NA, c(preddomain$nlons,
                                    preddomain$nlats,
                                    ncoarsetimes)),
           year1        = year1,
           yearN        = yearN,
           nlons        = preddomain$nlons,
           nlats        = preddomain$nlats,
           ntimes       = ncoarsetimes,
           WESN         = preddomain$WESN,
           lons         = preddomain$lons,
           lats         = preddomain$lats,
           time         = coarsetimes,
           timeunits    = timeunits,
           timecalendar = timecalendar,
           timestdname  = timestdname,
           timelongname = timelongname,
           timeexample  = timeexample,
           timeaxis     = timeaxis,
           timebnds     = timebnds,
           parentfname  = coarsefname,
           parentlons   = coarselons[loninds],
           parentlats   = coarselats[latinds],
           parentvarunits = coarseunits,
           varname      = coarsevar,
           varstdname   = coarsesname,
           varlongname  = coarselname,
           varunits     = coarseunits)

#---------------------------------------------------------------------
# expand.grid strings the grid into a state-space form, with longitude
# varying fastest. Creating the vector of dependent variables means
# stringing out the coarsedata similarly. 
#---------------------------------------------------------------------

# xg    = coarsegrid[,1:2]

for (i in 1:ncoarsetimes){

   start = c(     loninds[1],      latinds[1], i)
   count = c(length(loninds), length(latinds), 1)

   cat(paste("Working on timestep ",i,"of",ncoarsetimes),"\n")

   coarsedata = get.var.ncdf(coarsenc, coarsevar, start=start, count=count)
   yg = c(coarsedata) 
   # image.plot(coarselons[loninds],coarselats[latinds],coarsedata)
   # US(add=TRUE,shift=TRUE)
   # quilt.plot(coarsegrid,yg)
   splinefit = Tps(x=coarsegrid,Y=yg)

   # Ysanity = predict(splinefit,x=coarsegrid,lambda=0)
   # quilt.plot(coarsegrid,Ysanity)
   # US(add=TRUE,shift=TRUE)

   psurf = predict.surface(splinefit, 
              grid=list(x=preddomain$lons,y=preddomain$lats),
              extrap=TRUE, lambda=0)

   hiresdata$data[,,i] = psurf$z

}

close.ncdf(coarsenc)

return(hiresdata)

}
