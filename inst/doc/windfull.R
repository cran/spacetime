library(gstat)
data(wind)
wind.loc$y = as.numeric(char2dms(as.character(wind.loc[["Latitude"]])))
wind.loc$x = as.numeric(char2dms(as.character(wind.loc[["Longitude"]])))
coordinates(wind.loc) = ~x+y
proj4string(wind.loc) = "+proj=longlat +datum=WGS84"

library(mapdata)
wind$time = ISOdate(wind$year+1900, wind$month, wind$day)
N = nrow(wind)
wind$jday = as.numeric(format(wind$time, '%j'))
stations = 4:15
windsqrt = sqrt(0.5148 * wind[stations]) # knots -> m/s
Jday = 1:366
daymeans = apply(sapply(split(windsqrt - mean(windsqrt), wind$jday), mean), 2,  mean)
meanwind = lowess(daymeans ~ Jday, f = 0.1)$y[wind$jday]
velocities = apply(windsqrt, 2, function(x) { x - meanwind })

# match order of columns in wind to Code in wind.loc;
# convert to utm zone 29, to be able to do interpolation in
# proper Euclidian (projected) space:
pts = coordinates(wind.loc[match(names(wind[4:15]), wind.loc$Code),])
pts = SpatialPoints(pts)
proj4string(pts) = "+proj=longlat +datum=WGS84"
library(rgdal)
utm29 = CRS("+proj=utm +zone=29 +datum=WGS84")
pts = spTransform(pts, utm29)
t = xts(1:nrow(wind), wind$time)
# note the t() in:
w = STFDF(pts, t, data.frame(values = as.vector(t(velocities))))

library(maptools)
m = map2SpatialLines(
	map("worldHires", xlim = c(-11,-5.4), ylim = c(51,55.5), plot=F))
proj4string(m) = "+proj=longlat +datum=WGS84"
m = spTransform(m, utm29)

# setup grid
grd = SpatialPixels(SpatialPoints(makegrid(m, n = 300)),
	proj4string = proj4string(m))
# grd$t = rep(1, nrow(grd))
#coordinates(grd) = ~x1+x2
#gridded(grd)=TRUE

# select april 1961:
#w = w[, "1961-04"]

covfn.ST = function(x, y = x, seperate = missing(y)) { 
	du = spDists(coordinates(x@sp), coordinates(y@sp))
	t1 = as.numeric(index(x@time)) # time in seconds
	t2 = as.numeric(index(y@time)) # time in seconds
	dt = abs(outer(t1, t2, "-"))
	# separable, product covariance model:
	S = 0.6 * exp(-du/750000) 
	T = exp(-dt / (1.5 * 3600 * 24))
	if (separate) 
		list(S = S, T = T)
	else
		T %x% S
}

# for each day:
tgrd = xts(1:N, seq(min(index(w)), max(index(w)), length=N))

sys.time(pred <- krigeST(sqrt(values)~1, w, STF(grd, tgrd), covfn.ST))
wind.ST = STFDF(grd, tgrd, data.frame(pred = pred))
object.size(wind.ST)

#stplot(wind.ST, col.regions=bpy.colors(),
#	par.strip.text = list(cex=.5), sp.layout = layout)
