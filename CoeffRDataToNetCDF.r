"CoeffRDataToNetCDF" <- function ( vrbl, 
                             ddir="/project/gsp/thoar/downscaling",
                             fbase="downscaled")
{
#
#   $Id: CoeffRDataToNetCDF.r,v 1.5 2007/09/13 17:28:53 thoar Exp $
#
#   ddir  = "/fs/image/home/thoar/downscaling"
#   fbase = "prismaggregated"
#
#-------------------------------------------------------------------------------
# The named components of 'vrbl' are as follows:
# lons        [1405]  longitude array for downscaled data
# lats        [621]   latitude  array for downscaled data
# varname     [1]     variable character string
# year1       [1]     first year 
# yearN       [1]     last year 
# time        [12]    each month has one regression model
# timeunits    [1]    time/date unit character string
# timecalendar [1]    character string describing calendar
# timestdname  [1]    character string for standard name
# timelongname [1]    character string for long name
# timeaxis     [1]    character string for netcdf axis attribute
# timeexample  [1]    character string of time conversion example
# m       [1405,621,12] slopes for this location,month
# b       [1405,621,12] intercepts for this location,month
# sse     [1405,621,12] sum squared error of regression
# Ndata   [1405,621,12] Number of data going into the regression
#-------------------------------------------------------------------------------

library(ncdf)

fname = sprintf("%s/%s_%s_%d_%d.nc",ddir,fbase,vrbl$varname,vrbl$year1,vrbl$yearN)
cat(sprintf("Opening new file %s\n",fname))

#---------------------------------------------------------------------
# Set up netCDF output 
#---------------------------------------------------------------------

cat("Setting up dimensions and variables ...\n")

dim1 = dim.def.ncdf("lon"      ,"degrees_east"       , vrbl$lons)
dim2 = dim.def.ncdf("lat"      ,"degrees_north"      , vrbl$lats)
dim3 = dim.def.ncdf("time"     ,vrbl$timeunits       , vrbl$time)

var1 = var.def.ncdf(    "m","", list(dim1,dim2,dim3), -999., prec="double")
var2 = var.def.ncdf(    "b","", list(dim1,dim2,dim3), -999., prec="double")
var3 = var.def.ncdf(  "sse","", list(dim1,dim2,dim3), -999., prec="single")
var4 = var.def.ncdf("Ndata","", list(dim1,dim2,dim3), -999., prec="single")

newnc = create.ncdf(fname,list(var1,var2,var3,var4))

cat("Filling Attributes ...\n")

att.put.ncdf( newnc,0,"usage_note","The data should be interpolated to prediction grid before application of these coefficients.")
if(vrbl$varname == "ppt"){
   att.put.ncdf( newnc,0,"usage_note1","A cube-root transform must them be applied before using these coefficients.")
}
att.put.ncdf( newnc,0,"target_variable_name", vrbl$varname)
att.put.ncdf( newnc,0,"target_variable_units",vrbl$units)
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

att.put.ncdf( newnc,"time",     "calendar",vrbl$timecalendar)
att.put.ncdf( newnc,"time","standard_name",vrbl$timestdname)
att.put.ncdf( newnc,"time",         "axis",vrbl$timeaxis)
att.put.ncdf( newnc,"time",    "long_name",vrbl$timelongname)
att.put.ncdf( newnc,"time",      "example",vrbl$timeexample)

att.put.ncdf( newnc,var1,"standard_name","slope")
att.put.ncdf( newnc,var2,"standard_name","intercept")
att.put.ncdf( newnc,var3,"standard_name","sum of the squared error")
att.put.ncdf( newnc,var3,"comment","estimated 2 parameters, so normalize by Ndata-2 to get variance")

cat("Filling Variables ...\n")

put.var.ncdf( newnc, var1 , vrbl$m )
put.var.ncdf( newnc, var2 , vrbl$b )
put.var.ncdf( newnc, var3 , vrbl$sse )
put.var.ncdf( newnc, var4 , vrbl$Ndata )

cat("Closing netCDF file.\n")
close.ncdf(newnc)

}
