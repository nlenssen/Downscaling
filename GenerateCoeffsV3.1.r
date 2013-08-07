#------------------------------------------------------------------
# Perform the regression to determine coefficients for each month.
#------------------------------------------------------------------
# Walpole & Myers 1989 4th ed.
# "Probability and Statistics for Engineers and Scientists"
# pp 366-376     ISBN 0-02-424210-1 
#
# save pieces needed to quantify prediction uncertainty ... pp 373

slopes = rep(NA,length(prismlons)*length(prismlats))
intrcp = rep(NA,length(prismlons)*length(prismlats))
sse    = rep(NA,length(prismlons)*length(prismlats))

prismMonth <- get.var.ncdf(prismnc, prismvar)


bob <- ConvertUnits(from=prismunits,to=gcmunits)
scale      = bob$scale
offset     = bob$offset
rm(bob)

prismMonth <- prismMonth*scale + offset

m     <- array(NA, dim=c(length(prismlons),length(prismlats),12))
b     <- array(NA, dim=c(length(prismlons),length(prismlats),12))
sse   <- array(NA, dim=c(length(prismlons),length(prismlats),12))
ngood <- array(0 , dim=c(length(prismlons),length(prismlats),12))

for (imon in 1:12){
   for(i in 1:length(prismlons)){
      for(j in 1:length(prismlats)){
         if(sum(is.na(prismAdj[i,j,timeindex==imon]))==0 &&
            sum(is.na(prismMonth[i,j,timeindex==imon]))==0){
            reg <- lsfit(prismAdj[i,j,timeindex==imon],
                         prismMonth[i,j,timeindex==imon])
            m[i,j,imon] <- reg$coefficients["X"]
            b[i,j,imon] <- reg$coefficients["Intercept"]
            sse[i,j,imon] <- sum(reg$residuals**2)
            ngood[i,j,imon] <-  1
         }
      }
   }
}

prism$m <- m
prism$b <- b
prism$sse <- sse
prism$gcmN <- ngood

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

