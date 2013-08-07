"ReadPortalCCSMtxt" <- function(fname="tas_07_1970_1989_20C3M-124.0_-67.0_24.0_50.0.txt" )
{
#
# $Id: ReadPortalCCSMtxt.r,v 1.3 2008/04/24 03:57:36 thoar Exp $
#
# USAGE:
# gcm = ReadPortalCCSMtxt(fname="gcm/tas_07_1970_1989_20C3M-124.0_-67.0_24.0_50.0.txt")
#
# prism coordinates
# :West_Bounding_Coordinate = -125.020833333333 ;
# :East_Bounding_Coordinate = -66.47916757 ;
# :North_Bounding_Coordinate = 49.9375 ;
# :South_Bounding_Coordinate = 24.0625 ;
#
# From the GIS portal site, the extent is determined such that the
# entire GCM dataset is within the prism boundaries.
#
# maxlat =   50.0
# minlat =   24.0
# maxlon =  -67.0
# minlon = -124.0
#

# ---------------------------------------------------------------------
# Read the file. The first line is the column headers.
# ---------------------------------------------------------------------

cat(paste('Reading ',fname,'\n'))

connec     = file(fname) 
metastring = scan( connec, what="character", n=1)
metadata   = unlist(strsplit(metastring,","))
datarr     = scan( connec, skip=1, what=numeric(1), sep=',')
close(connec)

if (charmatch("lat", metadata[ 1] )) latind = 1
if (charmatch("lon", metadata[ 2] )) lonind = 2

ncols  = length(metadata)
nrows  = floor(length(datarr)/ncols)
datmat = matrix(datarr[1:(nrows*ncols)], ncol=ncols, byrow=TRUE)

lonarr = datmat[,lonind]
latarr = datmat[,latind]
lons   = unique(lonarr)
lats   = unique(latarr)

plot(lonarr, latarr, main=fname)
US( add=TRUE, col="red")

# ---------------------------------------------------------------------
# The data are simply arranged as they are in the file, which seems
# to be (1,1) = southwest most
# then proceed east at same latitude, 
# wrap to westernmost edge at next higher latitude ... i.e.
# (row-major) West-to-East, South-to-North.
# ---------------------------------------------------------------------

# ---------------------------------------------------------------------
# Create the output data structure
# ---------------------------------------------------------------------

z = list(filename     = fname, 
         nlocations   = nrows,
         nmonths      = ncols-2,
         months       = metadata[3:ncols],
         datmat       = datmat[,(3:ncols)], 
         UniqueLons   = lons,
         UniqueLats   = lats,
         lonarr       = lonarr,
         latarr       = latarr
         )

return(z)
}
