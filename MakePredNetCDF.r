"GCMdownscaling" <- function(ddir='/project/gsp/thoar/downscaling',
                    year1=1895, yearN=1931, downscalevar='tave', GCMvar='ts',
                    GCMfname='ts_A1.SRESA1B_1.CCSM.atmm.2000-01_cat_2099-12.nc')
{
#
# $Id: MakePredNetCDF.r,v 1.2 2007/02/19 23:52:26 thoar Exp $
#
# USAGE:
#
# ddir  = '/ptmp/thoar/downscaling'
# ddir  = '/project/gsp/thoar/downscaling'
# year1 = 1895
# yearN = 1931
# downscalevar = 'Tmin'
# GCMvar='ts'
# GCMfname='/fs/image/home/thoar/downscaling/gcm/ts_A1.SRESA1B_1.CCSM.atmm.2000-01_cat_2099-12.nc'
#
# bob = GCMdownscaling(ddir,year1,yearN,downscalevar,GCMvar,GCMfname)

library(ncdf)
library(chron)

# ---------------------------------------------------------------------
# load the regression coefficients
# names(tmin)
# "means"   [  41  19 1332]  prism estimate of GCM resolution
# "m"       [1405 621 12]    slope
# "b"       [1405 621 12]    intercept
# "sse"     [1405 621 12]    sum of squared residuals
# "gcmN"    [1405 621 12]    number of data 
# "gcmxi"   [1405 621]       which GCM longitude grid cell 
# "gcmyj"   [1405 621]       which GCM latitude grid cell
# "gcmlon"  [1405 621]       GCM longitude
# "gcmlat"  [1405 621]       GCM latitude
#
# 1332 = 111 years @ 12 months per ... year 1 is 1895, year 111 is 2005
#
# GCM data is [0,360], prism data is entirely in western hemisphere,
# so we can just add 360 to convert [-180 0] to [180 360].
# ---------------------------------------------------------------------

load(paste(ddir,"PrismTmin.RData",sep="/"))

tmin$gcmlon = 360 + tmin$gcmlon

LATrange = range(tmin$gcmlat,na.rm=TRUE)
LONrange = range(tmin$gcmlon,na.rm=TRUE)
Nprismlons = dim(tmin$gcmlon)[1]
Nprismlats = dim(tmin$gcmlon)[2]

# ---------------------------------------------------------------------
# GCM netcdf file spans whole globe. We are only interested in conUS.
# ---------------------------------------------------------------------

nc          = open.ncdf(GCMfname, write=FALSE) 
gcmtime     = get.var.ncdf(nc, "time")
gcmunits    = att.get.ncdf(nc, "time", "units")
gcmcalendar = att.get.ncdf(nc, "time", "calendar")
gcmlat      = get.var.ncdf(nc, 'lat')
gcmlon      = get.var.ncdf(nc, 'lon')
gcmvar      = get.var.ncdf(nc, GCMvar) 
close(GCMfname) 

# convert time to same base as coefficients

# find spatial intersection


multiply it out ...


# ---------------------------------------------------------------------
# Create the new netcdf files.
# ---------------------------------------------------------------------

fname = sprintf("%s/tave_%d_%d.nc",ddir,year1,yearN)

newnc = create.ncdf(fname,list(elev,var1)) 

att.put.ncdf(newnc,"time","origin"," month=1, day=1, year=0 ")

texample = julian(d=1,x=1,y=1895,origin=c(1,1,xxxx))
att.put.ncdf( newnc,"time","example1",paste("1 Jan 1895 ==",texample))
texample = julian(d=25,x=12,y=2000,origin=c(1,1,xxxx))
att.put.ncdf( newnc,"time","example2",paste("25 Dec 2000 ==",texample))

att.put.ncdf( newnc,0,"DataSource","PRISM Group, Oregon State University")
att.put.ncdf( newnc,0,"DataURL","http://www.ocs.orst.edu/prism/")
att.put.ncdf( newnc,0,"DataOrigin","ftp.ncdc.noaa.gov/pub/data/prism100")
att.put.ncdf( newnc,0,"DataRetrievalDate1_1895_1997","30 Aug 2006")
att.put.ncdf( newnc,0,"DataRetrievalDate2_1998_2005","20 Oct 2006")
att.put.ncdf( newnc,0,"netCDFCreatedBy","Tim Hoar - thoar@ucar.edu")
att.put.ncdf( newnc,0,"CreationDate",date())

put.var.ncdf( newnc,elev,elevations)

# ---------------------------------------------------------------------
#
# ---------------------------------------------------------------------

att.put.ncdf(tavenc,var1,"long_name" ,"Average Daily Temperature")

# ---------------------------------------------------------------------
#
# ---------------------------------------------------------------------

for (i in 1:ntimes){

   cat(paste("working on time index ",i," of ",ntimes),"\n")

   start   = c(    1,    1,      i )
   count   = c( nlon, nlat, ntimes )
   tmin    = get.var.ncdf(tminnc, 'tmin', start, count)
   tmax    = get.var.ncdf(tmaxnc, 'tmax', start, count)

   dtr     = abs(tmax-tmin)
   tave    = (tmax+tmin)/2.0

   put.var.ncdf(newnc, var1, tave, start, count)

}

close.ncdf(newnc)

}
