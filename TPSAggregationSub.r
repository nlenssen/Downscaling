TPSAggregationSub<-function(prismlons,prismlats,prismtime,gcmgrid,Ngridcells,
                           prismnc,prismvar,locator,gcmdx,gcmdy,scale,offset,
                           nlons,nlats){

   curTime <- 0

   prismAdj <- array(NA, dim= c(length(prismlons), length(prismlats),
                             length(prismtime)))

   nlonsindarray = (1:nlons)
   nlatsindarray = (1:nlats)

   sublons <- prismlons[(prismlons<242) & (prismlons>240)]
   sublats <- prismlats[(prismlats< 37) & (prismlats> 35)]
   sublonsinds <- nlonsindarray[(prismlons<242) & (prismlons>240)]
   sublatsinds <- nlatsindarray[(prismlats< 37) & (prismlats> 35)]

   for(t in 1:length(prismtime)){

      curTime = curTime + 1
      cat(paste("Working on time ",curTime,"of",length(prismtime)),"\n")


      tempMat <- matrix(NA,nrow=max(gcmgrid$gcmxi),ncol=max(gcmgrid$gcmyj))

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

         tempMat[gcmxi,gcmyj] <- mean(datmat,na.rm=TRUE)

      }

      splinefit <- Tps(x=gcmgrid[,1:2], Y=c(tempMat),lambda=0)
      prismAdj[sublonsinds,sublatsinds,t] <- predict.surface(
					        splinefit,
                                                grid=list(sublons,sublats),
                                                extrap=TRUE, lambda=0)$z


   }
   return(prismAdj)
}

