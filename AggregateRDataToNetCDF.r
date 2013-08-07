"AggregateRDataToNetCDF" <- function ( vrbl, 
                             ddir="/project/gsp/thoar/downscaling",
                             file,
                             fbase="prismaggregated",hascalendar=TRUE,
                             unlim=TRUE)
{
#
#   $Id: AggregateRDataToNetCDF.r,v 1.4 2007/09/06 17:50:16 thoar Exp $
#
#   ddir  = "/fs/image/home/thoar/downscaling"
#   fbase = "prismaggregated"
#
#-------------------------------------------------------------------------------
# The named components of 'vrbl' are as follows:
# lons        [1405]  longitude array for downscaled data
# lats        [621]   latitude  array for downscaled data
# time       ~[120]   time/date array for downscaled data
# timeunits    [1]    time/date unit character string
# timecalendar [1]    character string describing calendar
# timestdname  [1]    character string for standard name
# timelongname [1]    character string for long name
# timeaxis     [1]    character string for netcdf axis attribute
# timeexample  [1]    character string of time conversion example
# year1       [1]     first year 
# yearN       [1]     last year 
# varname     [1]     variable character string
# varunits    [1]     character string describing units of variable
# data    [1405,621,120]  the downscaled data
# timebnds    [2,120] the first/last dates of temporal averaging 
# parentfname [1]     filename of 'parent' (GCM) data (that was downscaled)
# parentvar           variable name in 'parent' (GCM) data
# parentvarunits
# parentvarlongname
# parentlons  [41]    longitudes of parent data
# parentlats  [19]    latitudes  of parent data
# parentdata  [41,19,120]  the actual parent data.
# coeffname   [1]     filename of coefficients from the prism data
#-------------------------------------------------------------------------------
# might even be able to use is.null(bob$dodo) strategy ...
#-------------------------------------------------------------------------------

library(ncdf)

#---------------------------------------------------------------------
# Set up netCDF output 
#---------------------------------------------------------------------

cat("Setting up dimensions and variables ...\n")

dim1 = dim.def.ncdf("lon"      ,"degrees_east"       , vrbl$lons)
dim2 = dim.def.ncdf("lat"      ,"degrees_north"      , vrbl$lats)
dim3 = dim.def.ncdf("bnds"     ,"indexical"          ,    (1:2))
dim4 = dim.def.ncdf("time"     ,vrbl$timeunits       , vrbl$time, unlim)
dim5 = dim.def.ncdf("parentlons","degrees_east"      , vrbl$parentlons)
dim6 = dim.def.ncdf("parentlats","degrees_north"     , vrbl$parentlats)

var1 = var.def.ncdf("lon_bnds","degrees_east",  list(dim3,dim1), -999., prec="double")
var2 = var.def.ncdf("lat_bnds","degrees_north", list(dim3,dim2), -999., prec="double")
var3 = var.def.ncdf("time_bnds",vrbl$timeunits, list(dim3,dim4), -999., prec="double")
var5 = var.def.ncdf(vrbl$varname,vrbl$varunits, list(dim1,dim2,dim4), -999., prec="double")

fname = sprintf("%s/%s",ddir,file)
cat(sprintf("Opening new file %s\n",fname))

newnc = create.ncdf(fname,list(var1,var2,var3,var5))

cat("Filling Attributes ...\n")

att.put.ncdf( newnc,0,"PrismDataSource","PRISM Group, Oregon State University")
att.put.ncdf( newnc,0,"PrismDataURL","http://www.ocs.orst.edu/prism/")
att.put.ncdf( newnc,0,"PrismDataOrigin","ftp.ncdc.noaa.gov/pub/data/prism100")
att.put.ncdf( newnc,0,"PrismDataRetrievalDate1_1895_1997","30 Aug 2006")
att.put.ncdf( newnc,0,"PrismDataRetrievalDate2_1998_2005","20 Oct 2006")
att.put.ncdf( newnc,0,"ParentDataFileName",vrbl$parentfname)
att.put.ncdf( newnc,0,"netCDFCreatedBy","Tim Hoar - thoar@ucar.edu")
att.put.ncdf( newnc,0,"CreationDate",date())

att.put.ncdf( newnc,"lat","axis","Y")
att.put.ncdf( newnc,"lat","standard_name","latitude")
att.put.ncdf( newnc,"lat","bounds","lat_bnds")
att.put.ncdf( newnc,"lat","long_name","latitude")

att.put.ncdf( newnc,"lon","axis","X")
att.put.ncdf( newnc,"lon","standard_name","longitude")
att.put.ncdf( newnc,"lon","bounds","lon_bnds")
att.put.ncdf( newnc,"lon","long_name","longitude")

att.put.ncdf( newnc,"time",      "calendar",     vrbl$timecalendar)
att.put.ncdf( newnc,"time",      "standard_name",vrbl$timestdname)
att.put.ncdf( newnc,"time",      "axis",         vrbl$timeaxis)
att.put.ncdf( newnc,"time",      "long_name",    vrbl$timelongname)
att.put.ncdf( newnc,"time",      "example",      vrbl$timeexample)
att.put.ncdf( newnc,"time",      "bounds",       "time_bnds")

att.put.ncdf( newnc,"time_bnds", "calendar",     vrbl$timecalendar)
att.put.ncdf( newnc,"time_bnds", "standard_name",vrbl$timestdname)
att.put.ncdf( newnc,"time_bnds", "axis",         vrbl$timeaxis)
att.put.ncdf( newnc,"time_bnds", "long_name",    vrbl$timelongname)
att.put.ncdf( newnc,"time_bnds", "example",      vrbl$timeexample)

if ( vrbl$varname == 'ppt'){
   att.put.ncdf( newnc,  var5, "cell_method","time: total")
   att.put.ncdf( newnc,  var5, "cell_methods","time: total (interval: 1 month)")
   vrbl$standardname = 'precipitation'
   vrbl$longname = 'monthly total precipitation'
}else{
   att.put.ncdf( newnc,  var5, "cell_method","time: mean")
   att.put.ncdf( newnc,  var5, "cell_methods","time: mean (interval: 1 month)")
}

if ( ! is.null(vrbl$standardname)){
   att.put.ncdf( newnc,  var5, "standard_name",vrbl$standardname)
}
if ( ! is.null(vrbl$longname)){
   att.put.ncdf( newnc,  var5, "long_name",vrbl$longname)
}

cat("Filling Variables ...\n")

nlons = length(vrbl$lons)
nlats = length(vrbl$lats)
ntime = length(vrbl$time)

# the boundaries 

dlon = mean(diff(vrbl$lons))
dlat = mean(diff(vrbl$lats))

lonedges = matrix(NA,nrow=2,ncol=nlons)
latedges = matrix(NA,nrow=2,ncol=nlats)

lonedges[1,] = vrbl$lons - dlon/2.0
lonedges[2,] = vrbl$lons + dlon/2.0
latedges[1,] = vrbl$lats - dlat/2.0
latedges[2,] = vrbl$lats + dlat/2.0

# stow the variables
start   = c(                     1,                      1,     1)
hrcount = c(                 nlons,                  nlats, ntime)
lrcount = c(length(vrbl$parentlons), length(vrbl$parentlats), ntime)
put.var.ncdf(nc=newnc, varid=var1, vals=lonedges)
put.var.ncdf(nc=newnc, varid=var2, vals=latedges)
put.var.ncdf(nc=newnc, varid=var3, vals=vrbl$timebnds, start=c(1,1), count=c(2,ntime))

#print(newnc)
#print(var5)
#print(start)
#print(hrcount)

put.var.ncdf(nc=newnc, varid=var5, vals=vrbl$data, start=start, count=hrcount)

cat("Closing netCDF file.\n")

close.ncdf(newnc)

}
