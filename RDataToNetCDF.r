"RDataToNetCDF" <- function ( var, ddir="/project/gsp/thoar/downscaling" )
{
#
#   $Id: RDataToNetCDF.r,v 1.7 2007/02/19 23:52:26 thoar Exp $
#
#   gcmdir  = "/fs/image/home/thoar/downscaling/gcm"
#   fname = "ts_A1.SRESA1B_1.CCSM.atmm.2000-01_cat_2099-12.nc"
#   year1 = 2010
#   yearN = 2019

#-------------------------------------------------------------------------------
# The named components of 'var' are as follows:
# lons        [1405]  longitude array for downscaled data
# lats        [621]   latitude  array for downscaled data
# time       ~[120]   time/date array for downscaled data
# year1       [1]     first year 
# yearN       [1]     last year 
# var         [1]     prism variable used to derive coefficients
# predictions [1405,621,120]  the downscaled data
# timebnds    [2,120] the first/last dates of temporal averaging 
# parentfname [1]     filename of 'parent' (GCM) data (that was downscaled)
# parentvar           variable name in 'parent' (GCM) data
# parentvarunits
# parentvarlongname
# parentlons  [41]    longitudes of parent data
# parentlats  [19]    latitudes  of parent data
# parentdata  [41,19,120]  the actual parent data.
#-------------------------------------------------------------------------------

library(ncdf)

#---------------------------------------------------------------------
cat("Getting the prism metadata ...\n")
#---------------------------------------------------------------------

metadata = GetArcInfoMetadata(ddir='/fs/image/home/thoar/downscaling',
                              fname='temp_metadata.txt')

#---------------------------------------------------------------------
# Set up netCDF output 
#---------------------------------------------------------------------

cat("Setting up dimensions and variables ...\n")

# ---------------------------------------------------------------------
# Define the netCDF coordinate variables -- these have values! 
# ---------------------------------------------------------------------

dim1 = dim.def.ncdf("lon"      ,"degrees_east"       , var$lons)
dim2 = dim.def.ncdf("lat"      ,"degrees_north"      , var$lats)
dim3 = dim.def.ncdf("bnds"     ,"bounds"             ,    (1:2))
dim4 = dim.def.ncdf("time"     ,"days since 0000-1-1", var$time, unlim=FALSE)
dim5 = dim.def.ncdf("parentlon","degrees_east"       , var$parentlons)
dim6 = dim.def.ncdf("parentlat","degrees_north"      , var$parentlats)

# ---------------------------------------------------------------------
# Define the netCDF variables -- these are empty! 
# ---------------------------------------------------------------------

var1 = var.def.ncdf(    "lon_bnds","", list(dim3,dim1),      -999., prec="double")
var2 = var.def.ncdf(    "lat_bnds","", list(dim3,dim2),      -999., prec="double")
var3 = var.def.ncdf(   "time_bnds","", list(dim3,dim4),      -999., prec="double")
var4 = var.def.ncdf(var$parentvar,"K", list(dim1,dim2,dim4),1.0E20, prec="single")
var5 = var.def.ncdf( "parent_data","", list(dim5,dim6,dim4), -999., prec="single")

# ---------------------------------------------------------------------
# Open a netCDF file containing desired variables.
# ---------------------------------------------------------------------

cat("Opening the netCDF file ...\n")

fname = sprintf("%s/downscaled_%s_%d_%d.nc",ddir,var$prismvar,var$year1,var$yearN)
newnc = create.ncdf(fname,list(var1,var2,var3,var4,var5))

cat("Filling Attributes ...\n")

att.put.ncdf( newnc,0,"PrismDataSource","PRISM Group, Oregon State University")
att.put.ncdf( newnc,0,"PrismDataURL","http://www.ocs.orst.edu/prism/")
att.put.ncdf( newnc,0,"PrismDataOrigin","ftp.ncdc.noaa.gov/pub/data/prism100")
att.put.ncdf( newnc,0,"PrismDataRetrievalDate1_1895_1997","30 Aug 2006")
att.put.ncdf( newnc,0,"PrismDataRetrievalDate2_1998_2005","20 Oct 2006")
att.put.ncdf( newnc,0,"ParentDataFileName",var$parentfname)
att.put.ncdf( newnc,0,"netCDFCreatedBy","Tim Hoar - thoar@ucar.edu")
att.put.ncdf( newnc,0,"CreationDate",date())

att.put.ncdf( newnc,"lat","axis","Y")
att.put.ncdf( newnc,"lat","standard_name","latitude")
att.put.ncdf( newnc,"lat","bounds","lat_bnds")
att.put.ncdf( newnc,"lat","long_name","latitude")

att.put.ncdf( newnc,"lon","axis","X")
att.put.ncdf( newnc,"lon","standard_name","longitude")
att.put.ncdf( newnc,"lon","bounds","lon_bnds")
att.put.ncdf( newnc,"lon","long_name","east_longitude")

att.put.ncdf( newnc,"parentlat","axis","Y")
att.put.ncdf( newnc,"parentlat","standard_name","latitude")
att.put.ncdf( newnc,"parentlat","long_name","latitude")

att.put.ncdf( newnc,"parentlon","axis","X")
att.put.ncdf( newnc,"parentlon","standard_name","longitude")
att.put.ncdf( newnc,"parentlon","long_name","east_longitude")

att.put.ncdf( newnc,"time","calendar","noleap")
att.put.ncdf( newnc,"time","standard_name","time")
att.put.ncdf( newnc,"time","axis","T")
att.put.ncdf( newnc,"time","bounds","time_bnds")
att.put.ncdf( newnc,"time","year_first",as.integer(var$year1))
att.put.ncdf( newnc,"time","year_last",as.integer(var$yearN))
att.put.ncdf( newnc,"time","example","1Jan2000 = 365*2000")

att.put.ncdf( newnc,var4,"_FillValue" ,1.0E20)
att.put.ncdf( newnc,var4,"cell_methods","time: mean (interval: 1 month)")
att.put.ncdf( newnc,var4,"original_units" ,var$parentvarunits)
att.put.ncdf( newnc,var4,"original_name" ,var$parentvarlongname)
att.put.ncdf( newnc,var4,"standard_name" ,var$parentlongname)
att.put.ncdf( newnc,var4,"long_name" ,var$parentvarlongname)
att.put.ncdf( newnc,var4,"cell_method" ,"time: mean")
att.put.ncdf( newnc,var4,"parent_filename" ,var$parentfname)

nlons = length(var$lons)
nlats = length(var$lats)
ntime = length(var$time)

cat("Calculating edges ...\n")

dlon         = mean(diff(var$lons))
dlat         = mean(diff(var$lats))

lonedges     = matrix(NA,nrow=nlons,ncol=2)
latedges     = matrix(NA,nrow=nlats,ncol=2)

lonedges[,1] = var$lons - dlon/2.0
lonedges[,2] = var$lons + dlon/2.0
latedges[,1] = var$lats - dlat/2.0
latedges[,2] = var$lats + dlat/2.0

cat("Filling Variables ...\n")

put.var.ncdf( newnc, var1, lonedges       )
put.var.ncdf( newnc, var2, latedges       )
put.var.ncdf( newnc, var3, var$timebnds   )
put.var.ncdf( newnc, var4, var$predictions)
put.var.ncdf( newnc, var5, var$parentdata )

cat("Closing netCDF file.\n")

close.ncdf(newnc)

}
