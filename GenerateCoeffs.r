"GenerateCoeffs" <- function ( fname="prism100/tmin_1895_2005.nc", 
     prismvar="tmin", ddir="/fs/image/home/thoar/downscaling/gcm",
     locator=FALSE )
{
#
# $Id: GenerateCoeffs.r,v 1.22 2008/04/24 03:57:35 thoar Exp $
#
# Generate downscaling coefficients.
# Use the entire prism record to detemine the relationship
# between the prism locations and the GCM location.
#
# The outline is something like:
#
# partition up the domain so that each prism location can relate to
# only one GCM location.
# For EACH GCM location:
#---------------------------------------------------------------------
# 1) aggregate the prism locations to an equivalent GCM resolution. 
# 2) determine the regression relation between the 
#    prism locations and the mean of those locations. 
#

# We add a debugging feature. This will generate key plots along 
# the way to help determine what is happening

debug = FALSE

# Get prism data and metadata. Will leave netCDF file open
# because we will be continually extracting subsets of the data
# from the file.

prismnc    = open.ncdf(fname) 
prismlons  = get.var.ncdf(prismnc, "lon") 
prismlats  = get.var.ncdf(prismnc, "lat")
prismtime  = get.var.ncdf(prismnc, "time")
prismunits = att.get.ncdf(prismnc, prismvar, "units")$value

topmost   = att.get.ncdf(prismnc,0,"North_Bounding_Coordinate")$value
botmost   = att.get.ncdf(prismnc,0,"South_Bounding_Coordinate")$value
lftmost   = att.get.ncdf(prismnc,0, "West_Bounding_Coordinate")$value
rhtmost   = att.get.ncdf(prismnc,0, "East_Bounding_Coordinate")$value

nlons     = length(prismlons)
nlats     = length(prismlats)

nlonsindarray = (1:nlons)
nlatsindarray = (1:nlats)

#---------------------------------------------------------------------
# Get the GCM grid from the netCDF file, subset to conUS
# retain the original GCM numbering [256,128]
#---------------------------------------------------------------------

gcmfname = "ts_A1.SRESA1B_1.CCSM.atmm.2000-01_cat_2099-12.nc"
gcmnc    = open.ncdf(paste(ddir,gcmfname,sep="/"))
gcmlons  = get.var.ncdf(gcmnc, "lon") 
gcmlats  = get.var.ncdf(gcmnc, "lat")
gcmtimes = get.var.ncdf(gcmnc, "time")
gcmunits = att.get.ncdf(gcmnc, "ts", "units")$value
close.ncdf(gcmnc)

ngcmlons = length(gcmlons)
ngcmlats = length(gcmlats)

loninds = (1:ngcmlons)[(gcmlons >= lftmost) & (gcmlons <= rhtmost)]
latinds = (1:ngcmlats)[(gcmlats >= botmost) & (gcmlats <= topmost)]

ngcmlonconus = length(loninds)
ngcmlatconus = length(latinds)

gcmgrid = expand.grid(lon = gcmlons[loninds], lat = gcmlats[latinds])
gcminds = expand.grid(lonind = loninds,             latind = latinds)
gcmgrid = cbind(gcmgrid,gcminds)
gcminds = expand.grid(gcmxi = (1:ngcmlonconus),gcmyj = (1:ngcmlatconus))
gcmgrid = cbind(gcmgrid,gcminds)
rm(gcminds)

Ngridcells = nrow(gcmgrid)
gcmdx      = mean(diff(gcmlons[loninds]))
gcmdy      =      diff(gcmlats[latinds])

cat(paste("There are",Ngridcells,"GCM grid cells over the continental US"),"\n")

#---------------------------------------------------------------------
# GCM temperature data are in degrees Kelvin PRISM is degrees celsius.
# GCM precipitation is flux ... PRISM is mm*100 (accumulation).
#---------------------------------------------------------------------

bob        = ConvertUnits(from=prismunits,to=gcmunits)
scale      = bob$scale
offset     = bob$offset
prismunits = bob$units
rm(bob)

#---------------------------------------------------------------------
# Loop over all possible GCM locations and create
# the prism equivalent for each GCM location. 
#---------------------------------------------------------------------

prism = list(means = array(NA, c(ngcmlonconus,
                                ngcmlatconus,
                                length(prismtime))),
           gcmlons    = gcmlons[loninds],
           gcmlats    = gcmlats[latinds],
           gcmloninds = loninds,
           gcmlatinds = latinds,
           lons       = prismlons,
           lats       = prismlats,
           times      = prismtime,
           varname    = prismvar,
           units      = prismunits,
           m          = array(NA,c(nlons,nlats,12)), # slope
           b          = array(NA,c(nlons,nlats,12)), # intercept
           sse        = array(NA,c(nlons,nlats,12)), # sum(resid^2)
           gcmN       = array(NA,c(nlons,nlats,12)), # count
           gcmxi      = array(NA,c(nlons,nlats)),    # applicable gcmlon index
           gcmyj      = array(NA,c(nlons,nlats)),    # applicable gcmlat index
           gcmlon     = array(NA,c(nlons,nlats)),    # applicable gcmlon
           gcmlat     = array(NA,c(nlons,nlats)))    # applicable gcmlat

# implicitly relies on prismtime(1) == january
timeindex = rep(1:12,length(prismtime)/12)

cellcount = 0

#---------------------------------------------------------------------
# Create a little locator figure 
#---------------------------------------------------------------------

if ( locator ){
   ndevices = length(dev.list())
   if (ndevices < 5){ for (idev in (ndevices+1):5) x11() }
}

# i =   1 is full of nothing.
# i =  30 is in the Florida Keys (n=8).
# i = 190 is on the Mississippi coast.
# i = 430 is right in the middle of Kansas.
# i = 739 is about Vancouver Island ... has ONE prism cell

#for (i in c(1, 30, 190, 430, 739)){
for (i in 1:Ngridcells){

   cellcount = cellcount + 1
   cat(paste("Working on cell ",cellcount,"of",Ngridcells),"\n")

   #------------------------------------------------------------------
   # Given the GCM centroid, find the grid edges.
   #------------------------------------------------------------------

   targetlon = gcmgrid$lon[i]
   targetlat = gcmgrid$lat[i]
   gcmxi     = gcmgrid$gcmxi[i]
   gcmyj     = gcmgrid$gcmyj[i]

   x1 = targetlon - gcmdx/2.0
   x2 = targetlon + gcmdx/2.0
   y1 = targetlat - max(gcmdy)/2.0
   y2 = targetlat + min(gcmdy)/2.0

   #------------------------------------------------------------------
   # Find the prism neighbor locations corresponding to the 
   # particular GCM (prediction) locations.
   #------------------------------------------------------------------

   loninds = nlonsindarray[(prismlons < x2) & (prismlons >= x1)]
   latinds = nlatsindarray[(prismlats < y2) & (prismlats >= y1)]
   pgrid   = expand.grid(loninds=loninds, latinds=latinds)

   # Get the prism data corresponding to this prediction location.

   start   = c(      loninds[1],      latinds[1],                 1 )
   count   = c( length(loninds), length(latinds), length(prismtime) )
   datmat  = get.var.ncdf(prismnc, prismvar, start, count)*scale + offset

   # sometimes, datmat is NA ... need to skip these
   # help(Control)   to find info on 'for'
   # use 'next' to halt the processing of the current iteration and 
   # advance the looping index.

   if ( ! any(is.finite(datmat)) ) next

   #------------------------------------------------------------------
   # Form the spatial mean(as a function of time) -- this is the
   # prism estimate of the value for the gcm grid cell. 
   #------------------------------------------------------------------

   prismmeans = apply (datmat, 3, mean, na.rm=TRUE)
   prism$means[gcmxi,gcmyj,] = prismmeans

   if ( locator ){
      dev.set(2)
      plot(targetlon,targetlat,xlim=c(235,293),ylim=c(24,50))
      lines(x=c(x1,x2,x2,x1,x1),y=c(y1,y1,y2,y2,y1),type='l',lty=1,col='red')
      US(add=TRUE,shift=TRUE)
      title('Region being estimated')

      dev.set(3)
      image.plot(prismlons[loninds], prismlats[latinds], datmat[,,1])
      US(add=TRUE,shift=TRUE)
      title('Data subsetted through start,count')

      dev.set(4)
      plot(prismmeans,xlab="month")
      lines(prismmeans)
      title(sprintf('Mean %s as a function of time',prismvar))
   }

   #------------------------------------------------------------------
   # Record the location information for these prism cells.
   # Given any prism cell, we will know exactly which gcm cell to 
   # extract to perform the prediction.
   #------------------------------------------------------------------

   prism$gcmlon[loninds,latinds] = targetlon
   prism$gcmlat[loninds,latinds] = targetlat
   prism$gcmxi[ loninds,latinds] = gcmxi
   prism$gcmyj[ loninds,latinds] = gcmyj

   #------------------------------------------------------------------
   # Perform the regression to determine coefficients for each month.
   #------------------------------------------------------------------
   # Walpole & Myers 1989 4th ed.
   # "Probability and Statistics for Engineers and Scientists"
   # pp 366-376     ISBN 0-02-424210-1 
   #
   # save pieces needed to quantify prediction uncertainty ... pp 373

   slopes = rep(NA,length(loninds)*length(latinds))
   intrcp = rep(NA,length(loninds)*length(latinds))
   sse    = rep(NA,length(loninds)*length(latinds))

   for (imon in 1:12){

      #---------------------------------------------------------------
      # subset by time
      # xtemp is [111-by-1]          (all time)
      # y     is [111-by-Nlocations] 
      #---------------------------------------------------------------

      xtemp = matrix(prism$means[gcmxi,gcmyj,timeindex==imon], ncol=1)
      y     = matrix(datmat[,,timeindex==imon], nrow=nrow(xtemp), byrow=TRUE)

      #---------------------------------------------------------------
      # pick out rows (i.e. locations) that have data to regress
      #---------------------------------------------------------------

      goodlocations = (colSums(is.finite(y)) > 0)
      ngood = sum(goodlocations)

      if ( all(goodlocations==FALSE) ) next
      if ( ngood < 20 ) next

      slopes[] = NA
      intrcp[] = NA
      sse[]    = NA
      bob      = lsfit(xtemp, y[,goodlocations] ) 

      #----------------------------------------------------------------
      # R is a little inconsistent on this subject.
      #----------------------------------------------------------------

      if ( ngood == 1 ) {
         slopes[goodlocations] = bob$coefficients["X"        ]
         intrcp[goodlocations] = bob$coefficients["Intercept"]
      } else {
         slopes[goodlocations] = bob$coefficients["X"        ,]
         intrcp[goodlocations] = bob$coefficients["Intercept",]
      }

      slopemat = matrix(slopes,nrow=length(loninds),byrow=FALSE)
      xtrcpmat = matrix(intrcp,nrow=length(loninds),byrow=FALSE)

      #----------------------------------------------------------------
      # This is a debug block to ensure I convert back and forth from
      # three dimensions to two and vice-versa.
      #----------------------------------------------------------------

      if ( locator ) {

         # plot one month of data 
         dev.set(2)
         # png(file=sprintf('loc%d_month%d_org.png',i,imon))
         image.plot(prismlons[loninds], prismlats[latinds], datmat[,,imon+48], 
                    col=tim.colors())
         US(add=TRUE,shift=TRUE)
         title(sprintf('month %d -- original data.',imon))
         dev.off()

         dev.set(3)
         # png(file=sprintf('loc%d_month%d_reg.png',i,imon))
         xax = (1:dim(y)[1])
         yax = (1:dim(y)[2])
         image.plot(xax,yax,y,xlab="time",ylab="space", col=tim.colors())
         title(sprintf('Regression data for month %d in state-space form.',imon))
         dev.off()

         dev.set(4)
         # png(file=sprintf('loc%d_month%d_slopes.png',i,imon))
         image.plot(prismlons[loninds], prismlats[latinds], slopemat, 
                    col=tim.colors())
         US(add=TRUE,shift=TRUE)
         title(sprintf('Slopes for month %d in original shape.',imon))
         dev.off()

         # reconstruct original data
         dev.set(5)
         # png(file=sprintf('loc%d_month%d_recon.png',i,imon))
         sse[goodlocations] = bob$residuals[(imon-1)*12+imon,]
         sse[goodlocations] = bob$residuals[5,]
         residmat = matrix(sse,nrow=length(loninds),byrow=FALSE)

         frog = slopemat*prism$means[gcmxi,gcmyj,imon+48] + xtrcpmat + residmat
         image.plot(prismlons[loninds], prismlats[latinds], frog, col=tim.colors())
         US(add=TRUE,shift=TRUE)
         title(sprintf('Month %d reconstituted - should match original.',imon))
         dev.off()

         # plot difference
         dev.set(6)
         # png(file=sprintf('loc%d_month%d_diff.png',i,imon))
         image.plot(prismlons[loninds], prismlats[latinds], 
                    frog-datmat[,,imon+48], col=tim.colors())
         US(add=TRUE,shift=TRUE)
         title(sprintf('Month %d Reconstituted data - truth',imon))
         dev.off()

         rm(xax,yax,frog)
         sse[] = NA
      } 

      #---------------------------------------------------------------
      # After the fit, stuff those results into the proper locations.
      # Now we take the regression coefficients etc. and stuff
      # them into the prismGCM structure that is sized for the
      # entire prism domain ... for [jan...dec] only.
      #---------------------------------------------------------------

      if ( ngood == 1) {
         sse[goodlocations] =     sum(bob$residuals**2)
      } else {
         sse[goodlocations] = colSums(bob$residuals**2)
      }
      residmat = matrix(   sse,nrow=length(loninds),byrow=FALSE)

      prism$m[     loninds,latinds,imon] = slopemat
      prism$b[     loninds,latinds,imon] = xtrcpmat
      prism$sse[   loninds,latinds,imon] = residmat
      prism$gcmN[  loninds,latinds,imon] = prism$gcmN[ loninds,latinds,imon] + ngood

      rm(slopemat,residmat,xtrcpmat)
   }

}

if(debug) {
dev.new()
image.plot(prismlons[loninds],prismlats[latinds],prism$m[,,1], main = 'Slopes')
US(add=TRUE,shift=TRUE)

dev.new()
image.plot(prismlons[loninds],prismlats[latinds],
           prism$b[,,1], main = 'Intercepts')
US(add=TRUE,shift=TRUE)

dev.new()
image.plot(prismlons[loninds],prismlats[latinds],
           prism$means[,,1], main = 'Aggregates')
}

close.ncdf(prismnc)

return(prism)

}
