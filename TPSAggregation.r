TPSAggregation <- function(prismlons,prismlats,prismtime,gcmgrid,Ngridcells,
                           prismnc,prismvar,locator,gcmdx,gcmdy,scale,offset,
			   nlons,nlats,gcmloninds,gcmlatinds,gcmlons,gcmlats){
# A function that takes in Prism data, aggregates to the gcm grid, and splines
# back down to the prism grid. It is written in a manner than makes it easily
# replaced in GenerateCoeffsV2.r

   # A switch to only perform the aggregations and collect stats
   aggOnly <- TRUE

   curTime <- 0         

   # Create the array that will contain the modified prism data   
   prismAdj <- array(NA, dim=c(nlons,nlats,length(prismtime)))

   nlonsindarray = (1:nlons)
   nlatsindarray = (1:nlats)

   # Read in one timepoint of prism to get indicator for NAs
   tempPrism <- get.var.ncdf(prismnc, prismvar, c(1,1,1), c(-1,-1,1))
   prismLandIndicator <- !is.na(tempPrism)
   prismLocLand <- as.matrix(expand.grid(
				prismlons,prismlats))[prismLandIndicator,]



   # Initialize the aggStats object
   xdim <- length(gcmloninds)
   ydim <- length(gcmlatinds)
   stats <- list(mmeans = array(NA, dim=c(xdim,ydim,12)),
		 msds   = array(NA, dim=c(xdim,ydim,12)),
                 mmin   = array(NA, dim=c(xdim,ydim,12)),
		 m25    = array(NA, dim=c(xdim,ydim,12)),
		 mmed   = array(NA, dim=c(xdim,ydim,12)),
		 m75    = array(NA, dim=c(xdim,ydim,12)),
		 mmax   = array(NA, dim=c(xdim,ydim,12)))	


   tempArray <- array(NA, dim = c(xdim,ydim,length(prismtime)))
   for(t in 1:length(prismtime)){

      curTime = curTime + 1
      cat(paste("Working on time ",curTime,"of",length(prismtime)),"\n")

      # Matrix where aggregates will be stored

      for(i in 1:Ngridcells){
      
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

         start   = c(      loninds[1],      latinds[1], t )
         count   = c( length(loninds), length(latinds), 1 )
         datmat  = get.var.ncdf(prismnc, prismvar, start, count)*scale + offset

         # sometimes, datmat is NA ... need to skip these
         # help(Control)   to find info on 'for'
         # use 'next' to halt the processing of the current iteration and 
         # advance the looping index.

         if ( ! any(is.finite(datmat)) ) next

         tempArray[gcmxi,gcmyj,t] <- mean(datmat,na.rm=TRUE)

      }

      # the first time through we build a gcmland grid and perform the Tps
      if(t == 1){
         gcmLandIndicator <- !is.na(tempArray[,,1])
         gcmLocLand       <- as.matrix(
			     expand.grid(gcmlons[gcmloninds],
					 gcmlats[gcmlatinds]))[gcmLandIndicator,]
         subTempMat <- c(tempArray[,,1])[gcmLandIndicator]
 	 tpsObject <- Tps(x=gcmgrid[gcmLandIndicator,1:2],Y=subTempMat,lambda=0)
      }

      # spline and interpolate the aggregates back to the prism grid
      if( !aggOnly){
         prismAdjTemp <- predict(tpsObject,x=prismLocLand,
			      yM=c(tempArray[,,t])[gcmLandIndicator],lambda=0)
         tempMat2 <- matrix(NA,1405,621)
         tempMat2[prismLandIndicator] <- prismAdjTemp
         prismAdj[,,t] <- tempMat2
      }
   }

   # Preform Stats on Aggregates
   timeindex <- rep(1:12,length(prismtime)/12)
   for(t in 1:12){
      for(i in 1:xdim){
	 for(j in 1:ydim){
	    stats$mmeans[i,j,t] <- mean(tempArray[i,j,timeindex==t],na.rm=T)
            stats$msds  [i,j,t] <-   sd(tempArray[i,j,timeindex==t],na.rm=T)
	    
	    quant <- quantile(tempArray[i,j,timeindex==t],na.rm=T)
            stats$mmin[i,j,t] <- quant[1]
	    stats$m25 [i,j,t] <- quant[2]
	    stats$mmed[i,j,t] <- quant[3]
	    stats$m75 [i,j,t] <- quant[4]
	    stats$mmax[i,j,t] <- quant[5]
	 }
      }
   }
   return(list(prismAdj=prismAdj,stats=stats))
}
