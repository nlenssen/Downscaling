"ArcInfoToNetCDF" <- function(ddir='/Users/thoar/downscaling/prism100', 
                              metadatafile='temp_metadata.txt',
                              NCvar='tmin', 
                              NCunits='celsius',
                              NCstdname='minimum daily temperature',
                              NClongname='minimum daily temperature',
                              NCmethods='time: mean (interval: 1 month)',
                              NCmethod='time: mean',
                              scaleby=100.0,
                              year1=1895, yearN=2005)
{
# ArcInfoToNetCDF
#
# $Id: ArcInfoToNetCDF.r,v 1.11 2008/04/24 03:57:34 thoar Exp $
#
# Reads multiple ArcInfoAsciiGrid format text files and converts them into a 
# single netCDF file. Some metadata from xxx_metadata.txt is needed,
# and just for grins, we stuff the elevation dataset into the netCDF file as well. 
# Both us_25m.dem and xxx_metadata.txt must be in the run-time (local) directory.
#
# The data text files must be organized into ddir/[TMAX,TMIN,PPT] directories.
# The NCvar is converted to uppercase and used to build the final portion of the
# directory name.
#
# USAGE:
#
# ddir='/Users/thoar/downscaling/prism100'
# ddir='/project/gsp/thoar/prism100'
# NCvar='tmin'
# NCunits='celsius'
# NCstdname='minimum daily temperature'
# NClongname='minimum daily temperature'
# NCmethods='time: mean (interval: 1 month)'
# NCmethod='time: mean'
# year1=1895
# yearN=2005
#
# bob = ArcInfoToNetCDF(ddir,NCvar,NCunits,NClongname,year1,yearN)

library(ncdf)
library(chron)

# ---------------------------------------------------------------------
# I need to read one to get some metadata for the netCDF file
# ---------------------------------------------------------------------

# Changed the first NCvar from toupper(NCvar)
fname   = sprintf("%s/%s/us_%s_%d.%02d.gz",ddir,(NCvar),NCvar,year1,1)
prism   = ReadArcInfoAsciiGrid(fname=fname, gzipped=TRUE, scaleby=scaleby)

# ---------------------------------------------------------------------
# Create the time, time_bnds arrays.
# The GCM data have the time as the middle of the averaging period
# ---------------------------------------------------------------------

datearr = (1:((yearN-year1+1)*12))
nfiles  = length(datearr)
filenames = array(NA,dim=nfiles)
time_bnds = array(NA,dim=c(nfiles,2))
i       = 1
for (iyear in year1:yearN){
for (imon  in     1:12   ){
     datearr[i] = julian(d=1, x=imon, y=iyear, origin=prism$chron.origin)
   # Again, changed the first NCvar from toupper(NCvar)
   filenames[i] = sprintf("%s/%s/us_%s_%d.%02d.gz", ddir,(NCvar), NCvar, iyear, imon)
   time_bnds[i,1] = datearr[i]
   i = i + 1
}
}
time_bnds[1:(nfiles-1),2] = time_bnds[2:nfiles,1]  
time_bnds[nfiles,2] = julian(d=1, x=1, y=yearN+1, origin=prism$chron.origin)
timearray = rowSums(time_bnds)/2.0

cat(sprintf('Processing %d files from %d to %d\n',nfiles,year1,yearN))

# ---------------------------------------------------------------------
# Define the netCDF coordinate variables -- these have values! 
# ---------------------------------------------------------------------

dim1 = dim.def.ncdf("lon"     ,"degrees_east"       , 360 + prism$xcenters)
dim2 = dim.def.ncdf("lat"     ,"degrees_north"      ,       prism$ycenters)
dim3 = dim.def.ncdf("bnds"    ,"bounds"             , (1:2))
dim4 = dim.def.ncdf("time"    ,"days since 1601-1-1", timearray,unlim=TRUE)

# ---------------------------------------------------------------------
# NClongname Define the netCDF variables -- these are empty! 
# ---------------------------------------------------------------------

elev   = var.def.ncdf("elev","meters_above_sea_level", list(dim1,dim2), -999., prec="integer")
var1   = var.def.ncdf("lon_bnds", "", list(dim3,dim1), -999., prec="double")
var2   = var.def.ncdf("lat_bnds", "", list(dim3,dim2), -999., prec="double")
var3   = var.def.ncdf("time_bnds","", list(dim3,dim4), -999., prec="double")
var4   = var.def.ncdf(NCvar, NCunits, list(dim1,dim2,dim4), 1.0E20, prec="single")

# ---------------------------------------------------------------------
# Open a netCDF file containing desired variables.
# ---------------------------------------------------------------------

cat("Opening the netCDF file ...\n")

ncfname = paste(NCvar,"_",year1,"_",yearN,".nc",sep='')
newnc = create.ncdf(ncfname,list(elev,var1,var2,var3,var4))

cat("Filling Attributes ...\n")

att.put.ncdf(newnc,0,"PrismDataSource","PRISM Group, Oregon State University")
att.put.ncdf(newnc,0,"PrismDataURL","http://www.ocs.orst.edu/prism/")
att.put.ncdf(newnc,0,"PrismDataOrigin","ftp.ncdc.noaa.gov/pub/data/prism100")
att.put.ncdf(newnc,0,"PrismDataRetrievalDate1_1895_1997","30 Aug 2006")
att.put.ncdf(newnc,0,"PrismDataRetrievalDate2_1998_2005","20 Oct 2006")
att.put.ncdf(newnc,0,"netCDFCreatedBy","Tim Hoar - thoar@ucar.edu")
att.put.ncdf(newnc,0,"CreationDate",date())
att.put.ncdf(newnc,0,"year_first",as.integer(year1))
att.put.ncdf(newnc,0,"year_last",as.integer(yearN))

att.put.ncdf(newnc,"lat","axis","Y") 
att.put.ncdf(newnc,"lat","standard_name","latitude")
att.put.ncdf(newnc,"lat","bounds","lat_bnds")
att.put.ncdf(newnc,"lat","long_name","latitude")

att.put.ncdf(newnc,"lon","axis","X")
att.put.ncdf(newnc,"lon","standard_name","longitude")
att.put.ncdf(newnc,"lon","bounds","lon_bnds")
att.put.ncdf(newnc,"lon","long_name","east_longitude")

t1example = julian(d=1, x=1,y=year1,origin=c(1,1,1601)) + 0.50
t1string  = sprintf("12Z  1 Jan %d == %f",year1,t1example)
t2example = julian(d=23,x=7,y=yearN,origin=c(1,1,1601)) + 0.75
t2string  = sprintf("18Z 23 Jul %d == %f",yearN,t2example)

att.put.ncdf(newnc,"time","calendar","Gregorian")
att.put.ncdf(newnc,"time","standard_name","time")
att.put.ncdf(newnc,"time","axis","T")
att.put.ncdf(newnc,"time","bounds","time_bnds")
att.put.ncdf(newnc,"time","long_name","time")
att.put.ncdf(newnc,"time","origin"," month=1, day=1, year=1601 ") 
att.put.ncdf(newnc,"time","example1",t1string) 
att.put.ncdf(newnc,"time","example2",t2string) 

att.put.ncdf(newnc,var4,"_FillValue",1.0E20) 
att.put.ncdf(newnc,var4,"cell_methods" ,NCmethods )
att.put.ncdf(newnc,var4,"cell_method"  ,NCmethod  )
att.put.ncdf(newnc,var4,"standard_name",NCstdname) 
att.put.ncdf(newnc,var4,"long_name",NClongname) 

cat("Filling Variables ...\n")

put.var.ncdf(newnc,var1, prism$lon_bnds )
put.var.ncdf(newnc,var2, prism$lat_bnds )
put.var.ncdf(newnc,var3, t(time_bnds)   )

# ---------------------------------------------------------------------
# Get the corresponding elevation data
# ---------------------------------------------------------------------

elevs = ReadArcInfoAsciiGrid(fname="us_25m.dem",gzipped=FALSE)
put.var.ncdf(newnc, elev, elevs$datmat)

# ---------------------------------------------------------------------
# Get some metadata from the metadata file,
# add it to the global attributes.
# ---------------------------------------------------------------------

metadata = GetArcInfoMetadata(ddir='.',fname=metadatafile)
att.put.ncdf(newnc, 0, metadata$weststring , metadata$westmost )
att.put.ncdf(newnc, 0, metadata$eaststring , metadata$eastmost )
att.put.ncdf(newnc, 0, metadata$southstring, metadata$southern )
att.put.ncdf(newnc, 0, metadata$northstring, metadata$northern )
att.put.ncdf(newnc, 0, metadata$dlatstring , metadata$dlat     )
att.put.ncdf(newnc, 0, metadata$dlonstring , metadata$dlon     )
att.put.ncdf(newnc, 0, metadata$unitstring , metadata$units    )

# ---------------------------------------------------------------------
# Loop over the source data files and stuff into proper slot.
# We are reading 2D slices and inserting into a 3D structure.
# ---------------------------------------------------------------------

for (i in 1:nfiles){

   prism = ReadArcInfoAsciiGrid(fname=filenames[i], gzipped=TRUE, scaleby=scaleby)

   put.var.ncdf(newnc, var4, prism$datmat, start=c(1,1,i), count=c(-1,-1,1))

} # end of loop over files

close.ncdf(newnc)

}
