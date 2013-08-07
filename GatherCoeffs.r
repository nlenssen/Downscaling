"GatherCoeffs" <- function ( ddir="/ptmp/thoar/downscaling",
                           varstring="tave", latind1arr, latindNarr )
{
#
# $Id: GatherCoeffs.r,v 1.5 2007/09/14 21:05:49 thoar Exp $
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

cat("$Id: GatherCoeffs.r,v 1.5 2007/09/14 21:05:49 thoar Exp $ \n")

nfiles  = length(latind1arr)

#---------------------------------------------------------------------
# Create a data structure to hold results
#---------------------------------------------------------------------

nlons = 1405
nlats = max(latindNarr) - min(latind1arr) + 1

coeff = list(lats         = rep(NA,nlats),
             lons         = rep(NA,nlons),
             varname      = varstring,
             year1        = 1895,
             yearN        = 2005,
             nlons        = nlons,
             nlats        = nlats,
             ntimes       = 12,
             time         = 1:12,
             timeunits    = 'months',
             timecalendar = 'none',
             timestdname  = 'month',
             timelongname = 'month',
             timeexample  = '1==Jan, 2==Feb ...',
             timeaxis     = 'M', 
             m            = array(NA,c(nlons,nlats,12)), # slope
             b            = array(NA,c(nlons,nlats,12)), # intercept
             sse          = array(NA,c(nlons,nlats,12)), # sum(resid^2)
             Ndata        = array( 0,c(nlons,nlats,12))) # count

if (varstring == 'ppt'){
   coeff$orgmse    = array(NA,c(nlons,nlats,12))
   coeff$biasmse   = array(NA,c(nlons,nlats,12))
   coeff$nobiasmse = array(NA,c(nlons,nlats,12))

   # original prism data
   coeff$Rpptmean  = array(NA,c(nlons,nlats,12))
   coeff$Rpptstd   = array(NA,c(nlons,nlats,12))
   coeff$Rpptskew  = array(NA,c(nlons,nlats,12))
   coeff$RpptNzero = array(NA,c(nlons,nlats,12))

   # aggregated-then-splined prism data
   coeff$Spptmean  = array(NA,c(nlons,nlats,12))
   coeff$Spptstd   = array(NA,c(nlons,nlats,12))
   coeff$Spptskew  = array(NA,c(nlons,nlats,12))
   coeff$SpptNzero = array(NA,c(nlons,nlats,12))

   # cuberoot-transform of prism data
   coeff$Tpptmean  = array(NA,c(nlons,nlats,12))
   coeff$Tpptstd   = array(NA,c(nlons,nlats,12))
   coeff$Tpptskew  = array(NA,c(nlons,nlats,12))
   coeff$TpptNzero = array(NA,c(nlons,nlats,12))

   # cuberoot-transform of agg-then-splined prism data
   coeff$Xpptmean  = array(NA,c(nlons,nlats,12))
   coeff$Xpptstd   = array(NA,c(nlons,nlats,12))
   coeff$Xpptskew  = array(NA,c(nlons,nlats,12))
   coeff$XpptNzero = array(NA,c(nlons,nlats,12))
}

#---------------------------------------------------------------------
# Loop over all possible prediction longitudes
#---------------------------------------------------------------------

for (ifile in 1:nfiles){

   rm(bob)

   latind1 = latind1arr[ifile]
   latindN = latindNarr[ifile]
   ifname  = sprintf('%s/coeffs_%s_%03d_%03d.RData', ddir,varstring,latind1,latindN)
   cat(sprintf("Concatenating on %s \n",ifname))

   load(ifname)

   coeff$m[    ,bob$latind1:bob$latindN,] = bob$m
   coeff$b[    ,bob$latind1:bob$latindN,] = bob$b
   coeff$sse[  ,bob$latind1:bob$latindN,] = bob$sse
   coeff$Ndata[,bob$latind1:bob$latindN,] = bob$Ndata
   coeff$lats[  bob$latind1:bob$latindN ] = bob$lats

if (varstring == 'ppt'){
   # different mean square error measures
   coeff$orgmse[    ,bob$latind1:bob$latindN,] = bob$orgmse
   coeff$biasmse[   ,bob$latind1:bob$latindN,] = bob$biasmse
   coeff$nobiasmse[ ,bob$latind1:bob$latindN,] = bob$nobiasmse

   # distribution of the (R)aw PRISM ppt data
   coeff$Rpptmean[  ,bob$latind1:bob$latindN,] = bob$Rpptmean
   coeff$Rpptstd[   ,bob$latind1:bob$latindN,] = bob$Rpptstd
   coeff$Rpptskew[  ,bob$latind1:bob$latindN,] = bob$Rpptskew
   coeff$RpptNzero[ ,bob$latind1:bob$latindN,] = bob$RpptNzero

   coeff$Spptmean[  ,bob$latind1:bob$latindN,] = bob$Spptmean
   coeff$Spptstd[   ,bob$latind1:bob$latindN,] = bob$Spptstd
   coeff$Spptskew[  ,bob$latind1:bob$latindN,] = bob$Spptskew
   coeff$SpptNzero[ ,bob$latind1:bob$latindN,] = bob$SpptNzero

   # distribution of the _cuberoot_ (T)ransformed PRISM ppt data
   coeff$Tpptmean[  ,bob$latind1:bob$latindN,] = bob$Tpptmean
   coeff$Tpptstd[   ,bob$latind1:bob$latindN,] = bob$Tpptstd
   coeff$Tpptskew[  ,bob$latind1:bob$latindN,] = bob$Tpptskew
   coeff$TpptNzero[ ,bob$latind1:bob$latindN,] = bob$TpptNzero
   
   coeff$Xpptmean[  ,bob$latind1:bob$latindN,] = bob$Xpptmean
   coeff$Xpptstd[   ,bob$latind1:bob$latindN,] = bob$Xpptstd
   coeff$Xpptskew[  ,bob$latind1:bob$latindN,] = bob$Xpptskew
   coeff$XpptNzero[ ,bob$latind1:bob$latindN,] = bob$XpptNzero
}

}

coeff$lons  = bob$lons
coeff$units = bob$units

return(coeff)

}
