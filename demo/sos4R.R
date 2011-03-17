library(gstat)

# move to an appropriate CRS:
RD = CRS(paste("+init=epsg:28992",
 "+towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812"))

library(rgdal)
no2.spdf = spTransform(no2.spdf, RD)
map.lines = spTransform(map.lines, RD)

no2.T1 = no2.spdf[no2.spdf$SamplingTime == min(no2.spdf$SamplingTime),]

grd = SpatialPixels(SpatialPoints(makegrid(bbox(map.lines), n = 1000)),
	proj4string = proj4string(map.lines))

plot(grd)
plot(map.lines, add=T)

names(no2.T1)[3] = "NO2"
NO2.idw =idw(NO2~1, no2.T1, grd)
lt = list(list("sp.lines", map.lines),
	list("sp.points", no2.T1, col = grey(.5)))
spplot(NO2.idw[1], col.regions = bpy.colors(), sp.layout=lt)
