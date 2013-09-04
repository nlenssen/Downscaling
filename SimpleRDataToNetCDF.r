"SimpleRDataToNetCDF" <- function ( vrbl, 
                             ddir="/project/gsp/thoar/downscaling",
                             file,
                             fbase="prismaggregated",hascalendar=TRUE,
                             unlim=TRUE,
			     RCP = "RCP 2.6",
			     timeperiod = "2006-2100")
{
#
#   $Id: SimpleRDataToNetCDF.r,v 1.14 2007/10/17 21:35:25 thoar Exp $
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

missval = -999.

###
# Sloppy hard coding to attach the land mask to the NetCDF product
###
load("/glade/p/work/lenssen/AR5/LandMask.RData")

cat("Setting up dimensions and variables ...\n")

dim1 = dim.def.ncdf("lon"      ,"degrees_east"       , vrbl$lons)
dim2 = dim.def.ncdf("lat"      ,"degrees_north"      , vrbl$lats)
dim3 = dim.def.ncdf("bnds"     ,"indexical"          ,    (1:2))
dim4 = dim.def.ncdf("time"     ,vrbl$timeunits       , vrbl$time, unlim=T)
dim5 = dim.def.ncdf("parentlons","degrees_east"      , vrbl$parentlons)
dim6 = dim.def.ncdf("parentlats","degrees_north"     , vrbl$parentlats)

var1 = var.def.ncdf("lon_bnds","degrees_east",  list(dim3,dim1), missval, prec="double")
var2 = var.def.ncdf("lat_bnds","degrees_north", list(dim3,dim2), missval, prec="double")
var3 = var.def.ncdf("time_bnds",vrbl$timeunits, list(dim3,dim4), missval, prec="double")

# Not all data structures have a parent variable
if ( ! is.null(vrbl$parentdata)){
var4 = var.def.ncdf("parentvar",vrbl$parentvarunits, list(dim5,dim6,dim4), missval, prec="single")
}

# express choice of precision
if ( ! is.null(vrbl$varprecision)){ 
var5 = var.def.ncdf(vrbl$varname,vrbl$varunits, list(dim1,dim2,dim4), missval, prec=vrbl$varprecision)
} else {
var5 = var.def.ncdf(vrbl$varname,vrbl$varunits, list(dim1,dim2,dim4), missval, prec="single")
}

var6 <- var.def.ncdf("land_mask","logical",list(dim1,dim2), missval, prec="single")

#fname = sprintf("%s/%s_%s_%d_%d.nc",ddir,fbase,vrbl$varname,vrbl$year1,vrbl$yearN)
fname = sprintf("%s/%s",ddir,file)
cat(sprintf("Opening new file %s\n",fname))
if ( is.null(vrbl$parentdata)){
   newnc = create.ncdf(fname,list(var1,var2,var3,var5,var6))
} else {
   newnc = create.ncdf(fname,list(var1,var2,var3,var4,var5,var6))
}

cat("Filling Attributes ...\n")

att.put.ncdf( newnc,0,"title","Statistially downscaled climate data")
if(ddata$varunits == "mm"){
   outvar <- "precipitation amount"
   keywords <- "Atmosphere > Precipitation > Precipitation Amount"
} else{
   outvar <- "surface air temperature"
   keywords <- "Atmosphere > Atmospheric Temperature > Air Temperature"
}

summarytxt <- sprintf("The data show monthly average %s for the period (%s) for %s.",
		      outvar,timeperiod,RCP)
att.put.ncdf( newnc,0,"summary",summarytxt)
att.put.ncdf( newnc,0,"keywords",keywords)
att.put.ncdf( newnc,0,"keywords_vocabulary","NASA/Global Change Master Directory (GCMD) Earth Science Keywords.  Version 8.0.0.0.0")
att.put.ncdf( newnc,0,"source","Statistical downscaled product from CCSM3.0, version beta 19 (2004): atmosphere:  CAM3.0, T85.  The downscaled method was produced by Tim Hoar and Dough Nychka at NCAR/IMAGE.")
att.put.ncdf( newnc,0,"date_created",date())
att.put.ncdf( newnc,0,"institution","National Center for Atmospheric Research")
att.put.ncdf( newnc,0,"project","GIS Program, GIS Climate Change Scenarios Portal")
att.put.ncdf( newnc,0,"date_modified",date())
att.put.ncdf( newnc,0,"date_issued",date())
att.put.ncdf( newnc,0,"acknowledgment","http://www2.cesm.ucar.edu/")
att.put.ncdf( newnc,0,"license","The data and information contained in this report is intended for research purposes only.  It is provided \"as is\" and without representations or warranties of any kind")
att.put.ncdf( newnc,0,"publisher_name","NCAR GIS Program")
att.put.ncdf( newnc,0,"publisher_email","GISSupport@ucar.edu")
att.put.ncdf( newnc,0,"publisher_url","https://gis.ucar.edu/")
att.put.ncdf( newnc,0,"program_info","The Geographic Information Systems (GIS) Program at the National Center for Atmospheric Research (NCAR) is an interdisciplinary effort to foster collaborative science, spatial data interoperability, and knowledge sharing with GIS. The main goal of the GIS Program is to promote and support the use of GIS as both an analysis, and an infrastructure tool in atmospheric research and to address broader issues of spatial data management, interoperability, and geoinformatics within the geosciences.  Working in collaboration with other NCAR strategic initiatives, divisions, and UCAR programs (such as Unidata, CDP, IMAGe, CGD, ISSE and RAL), we support variety of science projects at NCAR, make atmospheric data sets compatible with GIS tools, create bridges between atmospheric, geo- and social sciences, enable users to access, integrate and manage spatial information, and communicate science outcomes to the community of GIS users and the stakeholders.")
att.put.ncdf( newnc,0,"data_disclaimer","The data and information contained in this report is intended for research purposes only. It is provided \"as is\" and without representations or warranties of any kind, either expressed or implied. All representations and warranties are disclaimed, including, but not limited to, the implied warranties of merchantability and fitness for a particular purpose.  Experience has proven that the timeliness, resolution and manner in which data from these types of reports/models is used does not wholly support the effective or reliable use of the data in making decisions of an immediate or short term nature that involve the safety of people or property. The user assumes all risk as to the use of the data and/or information. In no event will UCAR, or any other party who has been involved in the creation, production or display of this data/information, be liable for damages, whether direct, special, indirect, incidental, or consequential, including loss of profits.")
att.put.ncdf( newnc,0,"PrismDataSource","PRISM Group, Oregon State University")
att.put.ncdf( newnc,0,"PrismDataURL","http://www.ocs.orst.edu/prism/")
att.put.ncdf( newnc,0,"PrismDataOrigin","ftp.ncdc.noaa.gov/pub/data/prism100")
att.put.ncdf( newnc,0,"PrismDataRetrievalDate1_1895_1997","30 Aug 2006")
att.put.ncdf( newnc,0,"PrismDataRetrievalDate2_1998_2005","20 Oct 2006")
att.put.ncdf( newnc,0,"ParentDataFileName",vrbl$parentfname)
att.put.ncdf( newnc,0,"creator_name","Doug Nychka -NCAR IMAGe")
att.put.ncdf( newnc,0,"creator_url","http://www2.image.ucar.edu/")
att.put.ncdf( newnc,0,"creator_email","nychka@ucar.edu")
if (! is.null(vrbl$coeffname)){
   att.put.ncdf( newnc,0, "CoefficientFileName",vrbl$coeffname)
}
att.put.ncdf( newnc,"lat","axis","Y")
att.put.ncdf( newnc,"lat","standard_name","latitude")
att.put.ncdf( newnc,"lat","bounds","lat_bnds")
att.put.ncdf( newnc,"lat","long_name","latitude")

att.put.ncdf( newnc,"lon","axis","X")
att.put.ncdf( newnc,"lon","standard_name","longitude")
att.put.ncdf( newnc,"lon","bounds","lon_bnds")
att.put.ncdf( newnc,"lon","long_name","longitude")

att.put.ncdf( newnc,"time","bounds","time_bnds")

if (is.null(vrbl$timecalendar)){
   att.put.ncdf( newnc,"time",      "calendar","noleap")
}else{
   att.put.ncdf( newnc,"time",      "calendar",vrbl$timecalendar)
   att.put.ncdf( newnc,"time_bnds", "calendar",vrbl$timecalendar)
}

if (is.null(vrbl$timestdname)){
   att.put.ncdf( newnc,"time",      "standard_name","time")
}else{
   att.put.ncdf( newnc,"time",      "standard_name",vrbl$timestdname)
   att.put.ncdf( newnc,"time_bnds", "standard_name",vrbl$timestdname)
}

if (is.null(vrbl$timeaxis)){
   att.put.ncdf( newnc,"time",      "axis","T")
}else{
   att.put.ncdf( newnc,"time",      "axis",vrbl$timeaxis)
   att.put.ncdf( newnc,"time_bnds", "axis",vrbl$timeaxis)
}

if (is.null(vrbl$timelongname)){
   att.put.ncdf( newnc,"time",      "long_name","time")
}else{
   att.put.ncdf( newnc,"time",      "long_name",vrbl$timelongname)
   att.put.ncdf( newnc,"time_bnds", "long_name",vrbl$timelongname)
}

if ( ! is.null(vrbl$timeexample)){
   att.put.ncdf( newnc,"time",      "example",vrbl$timeexample)
}

if ( ! is.null(vrbl$parentdata)){
   att.put.ncdf( newnc,  var4, "original_varname",vrbl$parentvar)
   att.put.ncdf( newnc,  var4, "standard_name",vrbl$parentstandardname)
   att.put.ncdf( newnc,  var4, "long_name",vrbl$parentlongname)
}

if ( vrbl$varunits == 'mm'){
   att.put.ncdf( newnc,  var5, "cell_method","time: total")
   att.put.ncdf( newnc,  var5, "cell_methods","time: total (interval: 1 month)")
}else{
   att.put.ncdf( newnc,  var5, "cell_method","time: mean")
   att.put.ncdf( newnc,  var5, "cell_methods","time: mean (interval: 1 month)")
}
att.put.ncdf( newnc,  var5, "cell_step1","thin-plate spline to dense grid")
att.put.ncdf( newnc,  var5, "cell_step2","apply linear model based on PRISM")
att.put.ncdf( newnc,  var5, "_FillValue",missval)
if ( ! is.null(vrbl$standardname)){
   att.put.ncdf( newnc,  var5, "standard_name",vrbl$standardname)
}
if ( ! is.null(vrbl$varlongname)){
   att.put.ncdf( newnc,  var5, "long_name",vrbl$varlongname)
}

# fill in the land mask attributes
att.put.ncdf( newnc, var6, "standard_name", "PRISM Land Mask")
att.put.ncdf( newnc, var6, "long_name", "PRISM Land Mask")

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
put.var.ncdf( newnc, var1, lonedges )
put.var.ncdf( newnc, var2, latedges )
put.var.ncdf( newnc, var3, vrbl$timebnds,   start=c(1,1), count=c(2,ntime) )
if ( ! is.null(vrbl$parentdata)){
put.var.ncdf( newnc, var4, vrbl$parentdata, start=start,  count=lrcount    )
}
put.var.ncdf( newnc, var5, vrbl$data,       start=start,  count=hrcount    )
put.var.ncdf( newnc, var6, prismmask, start=c(1,1), count=c(nlons,nlats))
cat("Closing netCDF file.\n")

close.ncdf(newnc)

}
