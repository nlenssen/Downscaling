#A script used to create a land mask for prism wrt the parent grid

ddir <- "/glade/p/work/lenssen/AR5"
fname <- "tave_1895_2005.nc"
prismvar <- "tave"

library(fields)
library(ncdf)

prismnc    = open.ncdf(fname)
prismlons  = get.var.ncdf(prismnc, "lon")
prismlats  = get.var.ncdf(prismnc, "lat")
topmost   = att.get.ncdf(prismnc,0,"North_Bounding_Coordinate")$value
botmost   = att.get.ncdf(prismnc,0,"South_Bounding_Coordinate")$value
lftmost   = att.get.ncdf(prismnc,0, "West_Bounding_Coordinate")$value
rhtmost   = att.get.ncdf(prismnc,0, "East_Bounding_Coordinate")$value
nlons     = length(prismlons)
nlats     = length(prismlats)

nlonsindarray = (1:nlons)
nlatsindarray = (1:nlats)


gcmfname = "TAS/tas_Amon_CCSM4_rcp26_r1i1p1_200601-210012.nc"
gcmnc    = open.ncdf(paste(ddir,gcmfname,sep="/"))
gcmlons  = get.var.ncdf(gcmnc, "lon")
gcmlats  = get.var.ncdf(gcmnc, "lat")
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

# Get a prism Indicator
tempPrism <- get.var.ncdf(prismnc, prismvar, c(1,1,1), c(-1,-1,1))
prismLandIndicator <- !is.na(tempPrism)
prismLocLand <- as.matrix(expand.grid(
                                prismlons,prismlats))[prismLandIndicator,]

# Build a mask at the parent grid size
parentmask<- matrix(NA,nrow=47,ncol=27)
for(i in 1:Ngridcells){
   targetlon = gcmgrid$lon[i]
   targetlat = gcmgrid$lat[i]
   gcmxi     = gcmgrid$gcmxi[i]
   gcmyj     = gcmgrid$gcmyj[i]

   x1 = targetlon - gcmdx/2.0
   x2 = targetlon + gcmdx/2.0
   y1 = targetlat - max(gcmdy)/2.0
   y2 = targetlat + min(gcmdy)/2.0

   loninds = nlonsindarray[(prismlons < x2) & (prismlons >= x1)]
   latinds = nlatsindarray[(prismlats < y2) & (prismlats >= y1)]
   pgrid   = expand.grid(loninds=loninds, latinds=latinds)

   start   = c(      loninds[1],      latinds[1], 1 )
   count   = c( length(loninds), length(latinds), 1 )
   datmat  = get.var.ncdf(prismnc, prismvar, start, count)

   if(all(is.na(datmat))){
       val <- NA 
   } else if(any(is.na(datmat))){
       val <- FALSE
   } else val <- TRUE

   parentmask[gcmxi,gcmyj] <- val
}

# Map back to the prism grid
prismgrid <- expand.grid(lon = prismlons, lat = prismlats)
nprismlonconus <- length(prismlons)
nprismlatconus <- length(prismlats)
prisminds <- expand.grid(prismxi= (1:nprismlonconus), prismyj = (1:nprismlatconus))
prismgrid <- cbind(prismgrid,prisminds)

prismmask <- matrix(NA,nrow=nprismlonconus,ncol=nprismlatconus)

for(i in 1:(dim(prismgrid)[1])){
   x <- prismgrid[i,1]
   y <- prismgrid[i,2]
   prismxi <- prismgrid$prismxi[i]
   prismyj <- prismgrid$prismyj[i]


   loninds <- which(min(abs(gcmgrid[,1]-x))==(abs(gcmgrid[,1]-x)))
   latinds <- which(min(abs(gcmgrid[,2]-y))==(abs(gcmgrid[,2]-y)))

   gcmval <- c(parentmask)[intersect(loninds,latinds)]

   prismmask[prismxi,prismyj] <- gcmval

   
}

