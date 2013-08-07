#
# $Id: BuildNC.r,v 1.3 2007/02/19 23:52:25 thoar Exp $
#

rm(list=ls())

library(ncdf)
library(chron)
library(fields)

source("ReadArcInfoAsciiGrid.r")
source("ArcInfoToNetCDF.r")

#---------------------------------------------------------------------
# Tmin
#---------------------------------------------------------------------

ddir       = '/ptmp/thoar/downscaling/prism100'
NCvar      = 'tmin'
NCunits    = 'celsius'
NClongname = 'Monthly Mean Minimum Daily Temperature'
year1      = 1982
yearN      = 1983

ArcInfoToNetCDF(ddir,NCvar,NCunits,NClongname,year1,yearN)

#---------------------------------------------------------------------
# Tmax
#---------------------------------------------------------------------

ddir       = '/ptmp/thoar/downscaling/prism100'
NCvar      = 'tmax'
NCunits    = 'celsius'
NClongname = 'Monthly Mean Maximum Daily Temperature'
year1      = 1980
yearN      = 1980

ArcInfoToNetCDF(ddir,NCvar,NCunits,NClongname,year1,yearN)

#---------------------------------------------------------------------
# ppt
#---------------------------------------------------------------------

ddir       = '/ptmp/thoar/downscaling/prism100'
NCvar      = 'ppt'
NCunits    = 'mm'
NClongname = 'Monthly Mean Total Precipitation'
year1      = 1980
yearN      = 1980

ArcInfoToNetCDF(ddir,NCvar,NCunits,NClongname,year1,yearN)
