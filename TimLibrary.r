"find" <- function ( x )
{
#
# $Id: TimLibrary.r,v 1.1 2007/09/13 17:31:41 thoar Exp $
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

xmax = max(x,na.rm=T)

j = row(x)[x == xmax & !is.na(x)]
i = col(x)[x == xmax & !is.na(x)]

return(list(max=xmax,row=j,col=i,nrow=nrow(x),ncol=ncol(x)))

}


"skewness" <- function (x, na.rm = FALSE)
{
   if (is.matrix(x))
       apply(x, 2, skewness, na.rm = na.rm)
   else if (is.vector(x)) {
       if (na.rm)
           x <- x[!is.na(x)]
       n <- length(x)
       (sum((x - mean(x))^3)/n)/(sum((x - mean(x))^2)/n)^(3/2)
   }
   else if (is.data.frame(x))
       sapply(x, skewness, na.rm = na.rm)
   else skewness(as.vector(x), na.rm = na.rm)
}

"mskew" <- function(x) {
m3 <- sum((x - mean(x))^3)/length(x)
s3 <- sqrt(var(x))^3
m3/s3
}

"kurtosis" <- function (x, na.rm = FALSE)
{
   if (is.matrix(x))
       apply(x, 2, kurtosis, na.rm = na.rm)
   else if (is.vector(x)) {
       if (na.rm)
           x <- x[!is.na(x)]
       n <- length(x)
       n * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2)
   }
   else if (is.data.frame(x))
       sapply(x, kurtosis, na.rm = na.rm)
   else kurtosis(as.vector(x), na.rm = na.rm)
}

"mkurtosis" <- function(x) {
m4 <- sum((x - mean(x))^4)/length(x)
s4 <- var(x)^2
m4/s4 - 3
}

