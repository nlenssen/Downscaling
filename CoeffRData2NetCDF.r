"CoeffRData2NetCDF" <- function ( vrbl, 
                             ddir="/project/gsp/thoar/downscaling",
                             fbase="downscaled")
{
#
#   $Id: CoeffRData2NetCDF.r,v 1.2 2007/09/14 21:04:59 thoar Exp $
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

#---------------------------------------------------------------------
# Create netCDF file for the error characteristics of the model.
#---------------------------------------------------------------------

fname = sprintf("%s/%s_%s_rmse_%d_%d.nc",ddir,fbase,vrbl$varname,vrbl$year1,vrbl$yearN)
cat(sprintf("Opening new file %s\n",fname))

cat("Setting up dimensions and variables ...\n")

dim1 = dim.def.ncdf("lon"      ,"degrees_east"       , vrbl$lons)
dim2 = dim.def.ncdf("lat"      ,"degrees_north"      , vrbl$lats)
dim3 = dim.def.ncdf("time"     ,vrbl$timeunits        , vrbl$time)

var1  = var.def.ncdf("orgrms"   ,"mm", list(dim1,dim2,dim3), -999., prec="single")
var2  = var.def.ncdf("nobiasrms","mm", list(dim1,dim2,dim3), -999., prec="single")
var3  = var.def.ncdf("biasrms"  ,"mm", list(dim1,dim2,dim3), -999., prec="single")

var4  = var.def.ncdf("orgCV"    ,"percent", list(dim1,dim2,dim3), -999., prec="single")
var5  = var.def.ncdf("nobiasCV" ,"percent", list(dim1,dim2,dim3), -999., prec="single")
var6  = var.def.ncdf("biasCV"   ,"percent", list(dim1,dim2,dim3), -999., prec="single")

newnc = create.ncdf(fname,list(var1,var2,var3,var4,var5,var6))

cat("Filling Attributes ...\n")

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

att.put.ncdf( newnc,var1,"long_name","root-mean-squared error")
att.put.ncdf( newnc,var1,"comment0","Y = original prism data")
att.put.ncdf( newnc,var1,"comment1","X = splined prism")
att.put.ncdf( newnc,var1,"comment2","sqrt(mean((Y - X)^2))")

att.put.ncdf( newnc,var2,"long_name","root-mean-squared error with no bias included")
att.put.ncdf( newnc,var2,"comment0","Y = original prism data")
att.put.ncdf( newnc,var2,"comment1","X = (b + m*x^(1/3))^3")
att.put.ncdf( newnc,var2,"comment2","sqrt(mean((Y - X)^2))")

att.put.ncdf( newnc,var3,"long_name","root-mean-squared error after including bias")
att.put.ncdf( newnc,var3,"comment0","Y = original prism data")
att.put.ncdf( newnc,var3,"comment1","X = (b + m*x^(1/3))^3 + 3((b + m*x^(1/3))variance)")
att.put.ncdf( newnc,var3,"comment2","sqrt(mean((Y - X)^2))")

att.put.ncdf( newnc,var4,"long_name","coefficient of error variation")
att.put.ncdf( newnc,var4,"comment0","orgrms/mean(PRISM)")

att.put.ncdf( newnc,var5,"long_name","coefficient of error variation")
att.put.ncdf( newnc,var5,"comment0","nobiasrms/mean(PRISM)")

att.put.ncdf( newnc,var6,"long_name","coefficient of error variation")
att.put.ncdf( newnc,var6,"comment0","biasrms/mean(PRISM)")

cat("Filling Variables ...\n")

std1 = sqrt(vrbl$orgmse)
std2 = sqrt(vrbl$nobiasmse)
std3 = sqrt(vrbl$biasmse)
put.var.ncdf( newnc,var1 , std1 )
put.var.ncdf( newnc,var2 , std2 )
put.var.ncdf( newnc,var3 , std3 )
put.var.ncdf( newnc,var4 , 100.0*std1/vrbl$Rpptmean )
put.var.ncdf( newnc,var5 , 100.0*std2/vrbl$Rpptmean )
put.var.ncdf( newnc,var6 , 100.0*std3/vrbl$Rpptmean )

cat("Closing netCDF file.\n")

close.ncdf(newnc)

#---------------------------------------------------------------------
# Create netCDF file for the transformed distributions
#---------------------------------------------------------------------

fname = sprintf("%s/%s_%s_trfm_%d_%d.nc",ddir,fbase,vrbl$varname,vrbl$year1,vrbl$yearN)
cat(sprintf("Opening new file %s\n",fname))

cat("Setting up dimensions and variables ...\n")

dim1 = dim.def.ncdf("lon" , "degrees_east" , vrbl$lons)
dim2 = dim.def.ncdf("lat" , "degrees_north", vrbl$lats)
dim3 = dim.def.ncdf("time", vrbl$timeunits , vrbl$time)

var4  = var.def.ncdf("Rpptmean" ,"mm",      list(dim1,dim2,dim3), -999., prec="single")
var5  = var.def.ncdf("Rpptstd"  ,"mm",      list(dim1,dim2,dim3), -999., prec="single")
var6  = var.def.ncdf("Rpptskew" ,"",        list(dim1,dim2,dim3), -999., prec="single")
var7  = var.def.ncdf("RpptNzero","percent", list(dim1,dim2,dim3), -999., prec="single")
var8  = var.def.ncdf("Tpptmean" ,"mm",      list(dim1,dim2,dim3), -999., prec="single")
var9  = var.def.ncdf("Tpptstd"  ,"mm",      list(dim1,dim2,dim3), -999., prec="single")
var10 = var.def.ncdf("Tpptskew" ,"",        list(dim1,dim2,dim3), -999., prec="single")
var11 = var.def.ncdf("TpptNzero","percent", list(dim1,dim2,dim3), -999., prec="single")
newnc = create.ncdf(fname,list(var4,var5,var6,var7,var8,var9,var10,var11))

cat("Filling Attributes ...\n")

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

att.put.ncdf( newnc,var4 , "long_name", "RAW mean(total PRISM precip) for this location")
att.put.ncdf( newnc,var5 , "long_name", "RAW std(total PRISM precip) for this location")
att.put.ncdf( newnc,var6 , "long_name", "RAW skewness(total PRISM precip) for this location")
att.put.ncdf( newnc,var7 , "long_name", "RAW percent of zeros (total PRISM precip) for this location")

att.put.ncdf( newnc,var8 , "comment"  , "TFM indicates cuberoot transform")
att.put.ncdf( newnc,var8 , "long_name", "TFM mean(total PRISM precip) for this location")
att.put.ncdf( newnc,var9 , "comment"  , "TFM indicates cuberoot transform")
att.put.ncdf( newnc,var9 , "long_name", "TFM std(total PRISM precip) for this location")
att.put.ncdf( newnc,var10, "comment"  , "TFM indicates cuberoot transform")
att.put.ncdf( newnc,var10, "long_name", "TFM skewness(total PRISM precip) for this location")
att.put.ncdf( newnc,var11, "comment"  , "TFM indicates cuberoot transform")
att.put.ncdf( newnc,var11, "long_name", "TFM percent of zeros (total PRISM precip) for this location")

cat("Filling Variables ...\n")

put.var.ncdf( newnc,var4 , vrbl$Rpptmean )
put.var.ncdf( newnc,var5 , vrbl$Rpptstd )
put.var.ncdf( newnc,var6 , vrbl$Rpptskew )
put.var.ncdf( newnc,var7 , vrbl$RpptNzero )
put.var.ncdf( newnc,var8 , vrbl$Tpptmean )
put.var.ncdf( newnc,var9 , vrbl$Tpptstd )
put.var.ncdf( newnc,var10, vrbl$Tpptskew )
put.var.ncdf( newnc,var11, vrbl$TpptNzero )

cat("Closing netCDF file.\n")

close.ncdf(newnc)

#---------------------------------------------------------------------
# Create netCDF file for the transformed distributions
#---------------------------------------------------------------------

fname = sprintf("%s/%s_%s_AGG_SPL_%d_%d.nc",ddir,fbase,vrbl$varname,vrbl$year1,vrbl$yearN)
cat(sprintf("Opening new file %s\n",fname))

cat("Setting up dimensions and variables ...\n")

dim1 = dim.def.ncdf("lon" , "degrees_east" , vrbl$lons)
dim2 = dim.def.ncdf("lat" , "degrees_north", vrbl$lats)
dim3 = dim.def.ncdf("time", vrbl$timeunits , vrbl$time)

var4  = var.def.ncdf("Spptmean" ,"mm",      list(dim1,dim2,dim3), -999., prec="single")
var5  = var.def.ncdf("Spptstd"  ,"mm",      list(dim1,dim2,dim3), -999., prec="single")
var6  = var.def.ncdf("Spptskew" ,"",        list(dim1,dim2,dim3), -999., prec="single")
var7  = var.def.ncdf("SpptNzero","percent", list(dim1,dim2,dim3), -999., prec="single")
var8  = var.def.ncdf("Xpptmean" ,"mm",      list(dim1,dim2,dim3), -999., prec="single")
var9  = var.def.ncdf("Xpptstd"  ,"mm",      list(dim1,dim2,dim3), -999., prec="single")
var10 = var.def.ncdf("Xpptskew" ,"",        list(dim1,dim2,dim3), -999., prec="single")
var11 = var.def.ncdf("XpptNzero","percent", list(dim1,dim2,dim3), -999., prec="single")
newnc = create.ncdf(fname,list(var4,var5,var6,var7,var8,var9,var10,var11))

cat("Filling Attributes ...\n")

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

att.put.ncdf( newnc,var4 , "long_name", "AGG-SPL mean(total PRISM precip) for this location")
att.put.ncdf( newnc,var5 , "long_name", "AGG-SPL std(total PRISM precip) for this location")
att.put.ncdf( newnc,var6 , "long_name", "AGG-SPL skewness(total PRISM precip) for this location")
att.put.ncdf( newnc,var7 , "long_name", "AGG-SPL percent of zeros (total PRISM precip) for this location")

att.put.ncdf( newnc,var8 , "long_name", "AGG-SPL-TFM mean(total PRISM precip) for this location")
att.put.ncdf( newnc,var9 , "long_name", "AGG-SPL-TFM std(total PRISM precip) for this location")
att.put.ncdf( newnc,var10, "long_name", "AGG-SPL-TFM skewness(total PRISM precip) for this location")
att.put.ncdf( newnc,var11, "long_name", "AGG-SPL-TFM percent of zeros (total PRISM precip) for this location")

att.put.ncdf( newnc,var8 , "comment"  , "TFM indicates cuberoot transform")
att.put.ncdf( newnc,var9 , "comment"  , "TFM indicates cuberoot transform")
att.put.ncdf( newnc,var10, "comment"  , "TFM indicates cuberoot transform")
att.put.ncdf( newnc,var11, "comment"  , "TFM indicates cuberoot transform")

cat("Filling Variables ...\n")

put.var.ncdf( newnc,var4 , vrbl$Spptmean )
put.var.ncdf( newnc,var5 , vrbl$Spptstd )
put.var.ncdf( newnc,var6 , vrbl$Spptskew )
put.var.ncdf( newnc,var7 , vrbl$SpptNzero )
put.var.ncdf( newnc,var8 , vrbl$Xpptmean )
put.var.ncdf( newnc,var9 , vrbl$Xpptstd )
put.var.ncdf( newnc,var10, vrbl$Xpptskew )
put.var.ncdf( newnc,var11, vrbl$XpptNzero )

cat("Closing netCDF file.\n")

close.ncdf(newnc)

}
