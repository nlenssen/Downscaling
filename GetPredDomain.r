"GetPredDomain" <- function ( ddir="/fs/image/home/thoar/downscaling",
                              file="prism100/tave_1895_1899.nc")
{

#---------------------------------------------------------------------
#
# $Id: GetPredDomain.r,v 1.2 2007/02/19 23:52:25 thoar Exp $
#
# Get the Prism domain
#---------------------------------------------------------------------

# Edited for NJL's file structure
# fname = sprintf("%s/%s",ddir,file)
fname = file

prismnc   = open.ncdf(fname) 
prismlons = get.var.ncdf(prismnc,"lon") 
prismlats = get.var.ncdf(prismnc,"lat")
topmost   = att.get.ncdf(prismnc,0,"North_Bounding_Coordinate")$value
botmost   = att.get.ncdf(prismnc,0,"South_Bounding_Coordinate")$value
lftmost   = att.get.ncdf(prismnc,0, "West_Bounding_Coordinate")$value
rhtmost   = att.get.ncdf(prismnc,0, "East_Bounding_Coordinate")$value
close.ncdf(prismnc)

nlons     = length(prismlons)
nlats     = length(prismlats)

nlonsindarray = (1:nlons)
nlatsindarray = (1:nlats)

prism = list(nlons = nlons,
             nlats = nlats,
             lons  = prismlons,
             lats  = prismlats,
             WESN  = c(lftmost,rhtmost,botmost,topmost))

return(prism)

}
