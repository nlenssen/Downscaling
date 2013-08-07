TimAggregationSub<-function(prismlons,prismlats,prismtime,gcmgrid,Ngridcells,
		            prismnc,prismvar,locator,gcmdx,gcmdy,scale,offset,
			    nlons,nlats){

   cellcount <- 0  
   
   prismAdj <- array(NA, dim= c(length(prismlons), length(prismlats),
                             length(prismtime)))

   nlonsindarray = (1:nlons)
   nlatsindarray = (1:nlats)

   sublons <- prismlons[(prismlons<242) & (prismlons>240)]
   sublats <- prismlats[(prismlats< 37) & (prismlats> 35)]
   sublonsinds <- nlonsindarray[(prismlons<242) & (prismlons>240)]
   sublatsinds <- nlatsindarray[(prismlats< 37) & (prismlats> 35)]

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

      prismAdj[loninds, latinds,] <- prismmeans


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
   }

return(prismAdj)
}

