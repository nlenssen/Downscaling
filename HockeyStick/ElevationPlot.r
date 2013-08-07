require(fields)
require(ncdf)

# Load in the 1895-1899 data to begin investigating the data
data <- open.ncdf("/glade/p/work/lenssen/output/tmax_1895_1899.nc")

# Extract the data of lat, lon, and elevation for a test plot
y <- get.var.ncdf(data, "lat")
x <- get.var.ncdf(data, "lon")
z <- get.var.ncdf(data, "elev")

# Make a elevation plot
image.plot((x-360),y,z,col=terrain.colors(32))
world(add=T)
US(add=T)
