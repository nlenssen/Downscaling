"GetArcInfoMetadata" <- function(ddir='.', fname="temp_metadata.txt")
{
#
# $Id: GetArcInfoMetadata.r,v 1.5 2007/05/29 21:43:19 thoar Exp $
#
# USAGE:
#
# ddir='/project/gsp/thoar/prism100'
# fname='temp_metadata.txt'
#
# bob = GetArcInfoMetadata(ddir,fname)

# ---------------------------------------------------------------------
# Get some metadata from the temperature metadata file,
# add it to the global attributes.
# ---------------------------------------------------------------------

wholefname = paste(ddir,fname,sep="/")

wbstring = system(paste('grep    "West_Bounding_Coordinate:"',
                  wholefname), intern=TRUE)
ebstring = system(paste('grep    "East_Bounding_Coordinate:"',
                  wholefname), intern=TRUE)
nbstring = system(paste('grep   "North_Bounding_Coordinate:"',
                  wholefname), intern=TRUE)
sbstring = system(paste('grep   "South_Bounding_Coordinate:"',
                  wholefname), intern=TRUE)
latres   = system(paste('grep         "Latitude_Resolution:"',
                  wholefname), intern=TRUE)
lonres   = system(paste('grep        "Longitude_Resolution:"',
                  wholefname), intern=TRUE)
resunits = system(paste('grep "Geographic_Coordinate_Units:"',
                  wholefname), intern=TRUE)

bob         = unlist(strsplit(wbstring,":"))
weststring  = sub('^[[:space:]]+', '', bob[1])
westmost    = 360 + as.numeric(bob[2])

bob         = unlist(strsplit(ebstring,":"))
eaststring  = sub('^[[:space:]]+', '', bob[1])
eastmost    = 360 + as.numeric(bob[2])

bob         = unlist(strsplit(nbstring,":"))
northstring = sub('^[[:space:]]+', '', bob[1])
northern    = as.numeric(bob[2])

bob         = unlist(strsplit(sbstring,":"))
southstring = sub('^[[:space:]]+', '', bob[1])
southern    = as.numeric(bob[2])

bob         = unlist(strsplit(latres,":"))
dlatstring  = sub('^[[:space:]]+', '', bob[1])
dlat        = as.numeric(bob[2])

bob         = unlist(strsplit(lonres,":"))
dlonstring  = sub('^[[:space:]]+', '', bob[1])
dlon        = as.numeric(bob[2])

bob         = unlist(strsplit(resunits,":"))
str         = sub('^[[:space:]]+', '', bob[1])
val         = sub('^[[:space:]]+', '', bob[2])

#cat(bob,"\n")
#cat(str,"\n")
#cat(val,"\n")

bob = list(fname      = wholefname,
           westmost   = westmost,
           weststring = weststring,
           eastmost   = eastmost,
           eaststring = eaststring,
           southern   = southern,
           southstring= southstring,
           northern   = northern,
           northstring= northstring,
           dlat       = dlat,
           dlatstring = dlatstring,
           dlon       = dlon,
           dlonstring = dlonstring,
           unitstring = str,
           units      = val)

return(bob)
}
