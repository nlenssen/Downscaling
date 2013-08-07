"ReadArcInfoAsciiGrid" <- function(fname="us_tmax_1995.01.gz", 
                   gzipped=TRUE, scaleby=1.0)
{
#
# $Id: ReadArcInfoAsciiGrid.r,v 1.3 2007/02/19 23:52:26 thoar Exp $
#
# USAGE:
# prism = ReadArcInfoAsciiGrid(fname="us_tmax_1995.01.gz", 
#        gzipped=TRUE, tvalid=c(month=1,year=1995), scaleby=100.0)
#

# The temp_metadata.txt file contains useful information ... like units.
# "Attribute_Units_of_Measure: degrees Celsius * 100"

# Read the first 12 pieces of whitespace-separated metadata.
# The order of the metadata is assumed to be fixed. 
# If the file is compressed with the gzip algorithm, it can be
# read directly by treating it as a connection.

cat(paste('Reading ',fname,'\n'))

ncols     = NA
nrows     = NA
xllcorner = NA
yllcorner = NA
cellsize  = NA

if ( gzipped == TRUE ){
   connec = gzfile(fname)
} else {
   connec = file(fname)
}

metadata = scan( connec, what="character", n=12)
datarr   = scan( connec, skip=6, what=integer(1))
close(connec)

if (charmatch("ncols",        metadata[ 1] ))        ncols = as.numeric(metadata[ 2])
if (charmatch("nrows",        metadata[ 3] ))        nrows = as.numeric(metadata[ 4])
if (charmatch("xllcorner",    metadata[ 5] ))    xllcorner = as.numeric(metadata[ 6])
if (charmatch("yllcorner",    metadata[ 7] ))    yllcorner = as.numeric(metadata[ 8])
if (charmatch("cellsize",     metadata[ 9] ))     cellsize = as.numeric(metadata[10])
if (charmatch("NODATA_value", metadata[11] )) NODATA_value = as.numeric(metadata[12])

xedges = seq( xllcorner, by=cellsize, length=ncols+1)
yedges = seq( yllcorner, by=cellsize, length=nrows+1)

lon_bnds = cbind(xedges[1:ncols],xedges[2:(ncols+1)])
lat_bnds = cbind(yedges[1:nrows],yedges[2:(nrows+1)])
xcen     = rowSums(lon_bnds)/2
ycen     = rowSums(lat_bnds)/2

# ---------------------------------------------------------------------
# try to parse the date from the filename
# () remove any compression extension
# () break apart directory structure
# () grab the filename
# () split the filename into pieces
# () ...
# ---------------------------------------------------------------------

formatok = grep('_[[:digit:]]{4}.[[:digit:]]{2}',fname)
if (length(formatok) > 0) formatok = TRUE else formatok = FALSE

if (formatok == TRUE){
   bob        = sub(".gz","",fname)
   tom        = unlist(strsplit(bob,"/"))
   filename   = tom[length(tom)]
   bob        = unlist(strsplit(filename,"\\."))
   validmonth = as.numeric(bob[length(bob)])
   tom        = unlist(strsplit(bob[1],"_"))
   validyear  = as.numeric(tom[length(tom)])
   tvalid     = c(month=validmonth, year=validyear)
   torigin    = c(day=1, month=1, year=1601)
   
   JulianDate = julian(d=1, x=tvalid[["month"]], y=tvalid[["year"]], origin=torigin)
} 

# ---------------------------------------------------------------------
# Read the data.
# Replace missing data with NA.
# Reshape into a matrix ... convert from column-major to row-major.
# Original data is 1405 (ncols) columns, 621 (nrows) rows.
# Must flip up/down so that 1,1 is lower left in image.plot
# ---------------------------------------------------------------------

datarr[datarr==NODATA_value] = NA

datmat = matrix( datarr/scaleby, nrow=ncols, ncol=nrows)
datmat = datmat[,nrows:1]

#image.plot(x, y, datmat)
#US( add=TRUE, col="red")

# ---------------------------------------------------------------------
# Create the output data structure
# ---------------------------------------------------------------------

z = list(filename     = fname, 
         nrows        = ncols,    # deliberately swapped
         ncols        = nrows,    # deliberately swapped
         datmat       = datmat, 
         xllcorner    = xllcorner,
         yllcorner    = yllcorner,
         cellsize     = cellsize,
         xcenters     = xcen,
         ycenters     = ycen,
         lon_bnds     = lon_bnds,
         lat_bnds     = lat_bnds, 
         xlonEdges    = xedges,
         ylatEdges    = yedges, 
         missingdata  = NA
         )

if (formatok == TRUE){
   z$ValidDate    = tvalid
   z$chron.origin = torigin
   z$JulianDate   = JulianDate
}

return(z)
}
