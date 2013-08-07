"GenerateCoeffsByLon" <- function ( ddir="/ptmp/thoar/downscaling",
     varstring="tave", year1=1895, yearN=2005, latind1=1, latindN=100 )
{
#
# $Id: GenerateCoeffsByLon.r,v 1.17 2008/05/02 15:45:37 thoar Exp $
#
# Generate downscaling coefficients for a latitude.
# Use the entire prism record to detemine the relationship.
# 
# The overall strategy for the processing is something like:
#
# 1) extract the data for each month for all longitudes for a latitude
# 2) extract the prism data for the same
# 3) determine the regression relation
#
#---------------------------------------------------------------------
# The output file is going to be a netcdf file with latitude as the
# unlimited dimension. That way, I can 'ncrcat' them together.
#---------------------------------------------------------------------

prismfname = sprintf('%s/prism100/PRISM_%s_%d_%d.nc',      ddir,varstring,year1,yearN)
datafname  = sprintf('%s/PRISM_splined2_%s_%d_%d.nc',      ddir,varstring,year1,yearN)
outfname   = sprintf('%s/coeffs2_%s_latinds_%03d_%03d.nc', ddir,varstring,latind1,latindN)

cat(sprintf("truth  file is %s\n",prismfname))
cat(sprintf("data   file is %s\n",datafname))
cat(sprintf("output file is %s\n",outfname))
cat("$Id: GenerateCoeffsByLon.r,v 1.17 2008/05/02 15:45:37 thoar Exp $ \n")

#---------------------------------------------------------------------
# Get prism data and metadata. Will leave netCDF file open because
# we will be continually extracting subsets of the data from the file.
#---------------------------------------------------------------------

prismnc    = open.ncdf(prismfname) 
prismlons  = get.var.ncdf(prismnc, "lon") 
prismlats  = get.var.ncdf(prismnc, "lat")
prismtime  = get.var.ncdf(prismnc, "time")
prismunits = att.get.ncdf(prismnc, varstring, "units")$value

#---------------------------------------------------------------------
# Get the data (the 'gcm-equivalent' dataset)
#---------------------------------------------------------------------

datanc     = open.ncdf(datafname)
datalons   = get.var.ncdf(datanc, "lon") 
datalats   = get.var.ncdf(datanc, "lat")
datatime   = get.var.ncdf(datanc, "time")
dataunits  = att.get.ncdf(datanc, varstring, "units")$value

#---------------------------------------------------------------------
# Make sure all is 'equal'
#---------------------------------------------------------------------

if ( all(prismlats == datalats) ){
   lats  = prismlats[latind1:latindN]
   nlats = length(lats)
   rm(prismlats,datalats)
}else{
   stop('Latitudes not equal')
}

if ( all(prismlons == datalons) ){
   lons  = prismlons
   nlons = length(lons)
   rm(prismlons,datalons)
}else{
   stop('Longitudes not equal')
}

if ( all(prismtime == datatime) ){
   times  = prismtime
   ntimes = length(times)
   rm(prismtime,datatime)
}else{
   stop('Times not equal')
}

#---------------------------------------------------------------------
# GCM temperature data are in degrees Kelvin,
# the prism temperature data is degrees celsius.
#---------------------------------------------------------------------

if ( dataunits == prismunits ){
   pscale      = 1.0
   poffset     = 0.0
   dscale      = 1.0
   doffset     = 0.0
}else{
   bob        = ConvertUnits(from=prismunits,to='K')
   pscale     = bob$scale
   poffset    = bob$offset
   prismunits = bob$units
   rm(bob)
   bob        = ConvertUnits(from=dataunits,to='K')
   dscale     = bob$scale
   doffset    = bob$offset
   dataunits  = bob$units
   rm(bob)
}

#---------------------------------------------------------------------
# Create a data structure to hold results
#---------------------------------------------------------------------

prism = list(latind1   = latind1,
             latindN   = latindN,
             lats      = lats,
             lons      = lons,
             month     = 1:12,
             varname   = varstring,
             units     = prismunits,
             m         = array(NA,c(nlons,nlats,12)), # slope
             b         = array(NA,c(nlons,nlats,12)), # intercept
             sse       = array(NA,c(nlons,nlats,12)), # sum(resid^2)
             Ndata     = array( 0,c(nlons,nlats,12))) # count

if (dataunits == 'mm'){
   prism$nobiasmse = array(NA,c(nlons,nlats,12))
   prism$biasmse   = array(NA,c(nlons,nlats,12))
   prism$orgmse    = array(NA,c(nlons,nlats,12))
   prism$Rpptmean  = array(NA,c(nlons,nlats,12))
   prism$Rpptstd   = array(NA,c(nlons,nlats,12))
   prism$Rpptskew  = array(NA,c(nlons,nlats,12))
   prism$RpptNzero = array(NA,c(nlons,nlats,12))
   prism$Spptmean  = array(NA,c(nlons,nlats,12))
   prism$Spptstd   = array(NA,c(nlons,nlats,12))
   prism$Spptskew  = array(NA,c(nlons,nlats,12))
   prism$SpptNzero = array(NA,c(nlons,nlats,12))
   prism$Tpptmean  = array(NA,c(nlons,nlats,12))
   prism$Tpptstd   = array(NA,c(nlons,nlats,12))
   prism$Tpptskew  = array(NA,c(nlons,nlats,12))
   prism$TpptNzero = array(NA,c(nlons,nlats,12))
   prism$Xpptmean  = array(NA,c(nlons,nlats,12))
   prism$Xpptstd   = array(NA,c(nlons,nlats,12))
   prism$Xpptskew  = array(NA,c(nlons,nlats,12))
   prism$XpptNzero = array(NA,c(nlons,nlats,12))
}

onethird = 1.0/3.0

# implicitly relies on times(1) == january
nyears    = length(times)/12
timeindex = rep(1:12,nyears)

#---------------------------------------------------------------------
# Loop over all possible prediction longitudes
#---------------------------------------------------------------------

for (j in latind1:latindN){

   jth = j - latind1 + 1

   start     = c(    1, j,      1)
   count     = c(nlons, 1, ntimes)
   prismdata = get.var.ncdf(prismnc, varstring, start, count)*pscale + poffset
   datadata  = get.var.ncdf( datanc, varstring, start, count)*dscale + doffset

 # image.plot(1:nlons,1:ntimes,prismdata - datadata) to pick valid lons

   str1 = sprintf("latitude %3d of %3d",j,latindN)
   cat(sprintf("Working on %s \n",str1))

   for (i in 1:nlons){
   
   #  str2 = sprintf("longitude %4d of %4d",i,nlons)
   #  cat(sprintf("Working on %s %s\n",str1,str2))
   
      for (imon in 1:12){
   
         #---------------------------------------------------------------
         # subset by time
         # x     is [111-by-1]   (all times for one location)
         # y     is [111-by-1]
         #---------------------------------------------------------------
   
         y = matrix(prismdata[i,timeindex==imon], ncol=1)
         x = matrix( datadata[i,timeindex==imon], ncol=1)

         #---------------------------------------------------------------
         # ensure there is data to regress
         #---------------------------------------------------------------
   
         ngood = sum(is.finite(y))
   
         if ( ngood < nyears ) next

         #---------------------------------------------------------------
         # Here's where we transform the data. Since these are precipitation
         # amounts, we know they are all zero or positive, so the cube-root
         # transform becomes a bit easier. 
         #---------------------------------------------------------------
         if (dataunits == 'mm'){
            x[x < 0] = 0.0
            y[y < 0] = 0.0
            y = y^onethird
            x = x^onethird
            prism$units = 'cuberoot(mm)'

            # record the transform of the original data
            prism$Tpptmean[ i,jth,imon] = mean(y)
            prism$Tpptstd[  i,jth,imon] = sqrt(var(y))
            prism$Tpptskew[ i,jth,imon] = skewness(y)
            prism$TpptNzero[i,jth,imon] = sum(y == 0.0)

            prism$Xpptmean[ i,jth,imon] = mean(x)
            prism$Xpptstd[  i,jth,imon] = sqrt(var(x))
            prism$Xpptskew[ i,jth,imon] = skewness(x)
            prism$XpptNzero[i,jth,imon] = sum(x == 0.0)
         }
        
         bob          = lm(y ~ x) 
         modelsummary = summary(bob)
         resids       = modelsummary$residuals
   
         prism$m[     i,jth,imon] = bob$coefficients["x"          ]
         prism$b[     i,jth,imon] = bob$coefficients["(Intercept)"]
         prism$sse[   i,jth,imon] = sum(resids**2)
         prism$Ndata[ i,jth,imon] = prism$Ndata[i,jth,imon] + ngood
   
         #----------------------------------------------------------------
         # This is a debug block to ensure I can reconstruct the data.
         #----------------------------------------------------------------
         if ( 1 == 2 ) { 
            frog = prism$m[i,jth,imon]*x + prism$b[i,jth,imon] + resids 
            plot(y - frog) 
         } 

         #----------------------------------------------------------------
         # This part is for the reconstruction of precipitation. 
         # The cuberoot transform introduces bias.
         #----------------------------------------------------------------
         if (dataunits == 'mm'){

            # recover untransformed ppt ... in mm/month
            y = matrix(prismdata[i,timeindex==imon], ncol=1)
            x = matrix( datadata[i,timeindex==imon], ncol=1)
            x[x < 0] = 0.0
            y[y < 0] = 0.0

            prism$Rpptmean[ i,jth,imon] = mean(y)
            prism$Rpptstd[  i,jth,imon] = sqrt(var(y))
            prism$Rpptskew[ i,jth,imon] = skewness(y)
            prism$RpptNzero[i,jth,imon] = sum(y == 0)

            prism$Spptmean[ i,jth,imon] = mean(x)
            prism$Spptstd[  i,jth,imon] = sqrt(var(x))
            prism$Spptskew[ i,jth,imon] = skewness(x)
            prism$SpptNzero[i,jth,imon] = sum(x == 0)

            # transform, apply model
            x13    = x^onethird
            frog   = prism$m[i,jth,imon]*x13 + prism$b[i,jth,imon]
            frog[frog < 0.0] = 0.0

            sigma2 = prism$sse[i,jth,imon]/(prism$Ndata[i,jth,imon]-2)
            bias   = 3*frog*sigma2
            yhat   = frog^3 + bias

            # record the fit
            nobiasmse = mean((y-frog^3)^2)
            biasmse   = mean((y-  yhat)^2)
            orgmse    = mean((y-     x)^2)

            prism$nobiasmse[i,jth,imon] = nobiasmse
            prism$biasmse[  i,jth,imon] = biasmse
            prism$orgmse[   i,jth,imon] = orgmse

            #cat(sprintf('mean square    error = %f\n',orgmse))
            #cat(sprintf('no bias        error = %f\n',nobiasmse))
            #cat(sprintf('bias-corrected error = %f\n',biasmse))

         }
      }
   }
}

close.ncdf(prismnc)
close.ncdf( datanc)

return(prism)

}
