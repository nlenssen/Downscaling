"GenerateCoeffsV3" <- function ( fname="prism100/tmin_1895_2005.nc", 
     prismvar="tmin", ddir="/fs/image/home/thoar/downscaling/gcm",
     locator=FALSE, timebegin, timeend )
{
#
# $Id: GenerateCoeffs.r,v 1.22 2008/04/24 03:57:35 thoar Exp $
#
# Generate downscaling coefficients.
# Use the entire prism record to detemine the relationship
# between the prism locations and the GCM location.
#
# The outline is something like:
#
# partition up the domain so that each prism location can relate to
# only one GCM location.
# For EACH GCM location:
#---------------------------------------------------------------------
# 1) aggregate the prism locations to an equivalent GCM resolution. 
# 2) determine the regression relation between the 
#    prism locations and the mean of those locations. 
#

# We add a debugging feature. This will generate key plots along 
# the way to help determine what is happening

debug = FALSE

# Get prism data and metadata. Will leave netCDF file open
# because we will be continually extracting subsets of the data
# from the file.

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
# retain the original GCM numbering [256,128]
#---------------------------------------------------------------------

gcmfname = "ts_A1.SRESA1B_1.CCSM.atmm.2000-01_cat_2099-12.nc"
gcmnc    = open.ncdf(paste(ddir,gcmfname,sep="/"))
gcmlons  = get.var.ncdf(gcmnc, "lon") 
gcmlats  = get.var.ncdf(gcmnc, "lat")
gcmtimes = get.var.ncdf(gcmnc, "time")
gcmunits = att.get.ncdf(gcmnc, "ts", "units")$value
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

#---------------------------------------------------------------------
# GCM temperature data are in degrees Kelvin PRISM is degrees celsius.
# GCM precipitation is flux ... PRISM is mm*100 (accumulation).
#---------------------------------------------------------------------

bob        = ConvertUnits(from=prismunits,to=gcmunits)
scale      = bob$scale
offset     = bob$offset
rm(bob)

#---------------------------------------------------------------------
# Loop over all possible GCM locations and create
# the prism equivalent for each GCM location. 
#---------------------------------------------------------------------

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
           gcmlat     = array(NA,c(nlons,nlats)))    # applicable gcmlat

# implicitly relies on prismtime(1) == january
timeindex = rep(1:12,length(prismtime)/12)

cellcount = 0

timesteps <- timebegin:timeend

prismAdj <- TPSAggregation(prismlons,prismlats,timesteps,gcmgrid,Ngridcells,
                           prismnc,prismvar,locator,gcmdx,gcmdy,scale,offset,
			   nlons, nlats)               		   

close.ncdf(prismnc)
return(PrismAdj)
