"ConvertUnits" <- function ( from, to )
{
#---------------------------------------------------------------------
#
# $Id: ConvertUnits.r,v 1.2 2007/02/19 23:52:25 thoar Exp $
#
# GCM T data are in degrees Kelvin, the prism data is degrees celsius
# We convert temperatures to Kelvin.
#---------------------------------------------------------------------

if ( from == to ) {
   scale  = 1
   offset = 0
} else if ( (from == "celsius") & (to == "K") ) {
   scale  =   1.00
   offset = 273.15
   from   = to
} else if ( (from == "K") & (to == "celsius") ) {
   scale  =    1.00
   offset = -273.15
   from   = to
} else {
   cat('WARNING - units mismatch - doing nothing.\n')
   cat(sprintf('from  variable has units ... %s \n',from))
   cat(sprintf('to    variable has units ... %s \n',to))
   scale  = 1
   offset = 0
}

return(list(units=from,scale=scale,offset=offset))

}
