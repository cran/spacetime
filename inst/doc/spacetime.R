###################################################
### chunk number 1: 
###################################################
#line 143 "spacetime.Rnw"
library(spacetime)
s = 1:3
t = c(1, 1.5, 3, 4.5)
g = data.frame(rep(t, each=3), rep(s,4))
plot(g, xaxt = 'n', yaxt = 'n', xlab = "Time points", 
	ylab = "Space locations", xlim = c(.5,5), ylim = c(.5,3.5))
abline(h=s, col = grey(.8))
abline(v=t, col = grey(.8))
axis(1, at = t, labels = c("1st", "2nd", "3rd", "4th"))
axis(2, at = s, labels = c("1st", "2nd", "3rd"))
text(g, labels = 1:12, pos=4)
title("Layout for STFDF")


###################################################
### chunk number 2: 
###################################################
#line 174 "spacetime.Rnw"
s = 1:3
t = c(1, 2.2, 3, 4.5)
g = data.frame(rep(t, each=3), rep(s,4))
plot(g, xaxt = 'n', yaxt = 'n', xlab = "Time points",                
    ylab = "Space locations", xlim = c(.5,5), ylim = c(.5,3.5))
abline(h=s, col = grey(.8))
abline(v=t, col = grey(.8))
axis(1, at = t, labels = c("1st", "2nd", "3rd", "4th"))
axis(2, at = s, labels = c("1st", "2nd", "3rd"))
sel = c(1,2,3,5,6,7,11)
text(g[sel,], labels = paste(1:length(sel), "[",c(1,2,3,2,3,1,2),",",c(1,1,1,2,2,3,4),"]", sep=""), pos=4)
title("Layout for STPDF")


###################################################
### chunk number 3: 
###################################################
#line 209 "spacetime.Rnw"
s = c(1,2,3,1,4)
t = c(1, 2.2, 3, 3, 4.5)
g = data.frame(t,s)
plot(g, xaxt = 'n', yaxt = 'n', xlab = "Time points",                
    ylab = "Space locations", xlim = c(.5,5), ylim = c(.5,4.5))
abline(h=s, col = grey(.8))
abline(v=t, col = grey(.8))
axis(1, at = sort(unique(t)), labels = c("1st", "2nd", "3rd,4th", "5th"))
axis(2, at = sort(unique(s)), labels = c("1st,4th", "2nd", "3rd", "5th"))
text(g, labels = 1:5, pos=4)
title("Layout for STSDF")


###################################################
### chunk number 4: 
###################################################
#line 238 "spacetime.Rnw"
library(spacetime)
showClass("ST")
showClass("STFDF")
sp = cbind(x = c(0,0,1), y = c(0,1,1))
row.names(sp) = paste("point", 1:nrow(sp), sep="")
sp = SpatialPoints(sp)
time = xts(1:4, as.POSIXct("2010-08-05")+3600*(10:13))
m = c(10,20,30) # means for each of the 3 point locations
mydata = rnorm(length(sp)*length(time),mean=rep(m, 4))
IDs = paste("ID",1:length(mydata), sep = "_")
mydata = data.frame(values = signif(mydata,3), ID=IDs)
stfdf = STFDF(sp, time, mydata)
str(stfdf)


###################################################
### chunk number 5: 
###################################################
#line 258 "spacetime.Rnw"
as.data.frame(stfdf, row.names = IDs)
as(stfdf, "data.frame")[1:4,]


###################################################
### chunk number 6: 
###################################################
#line 271 "spacetime.Rnw"
unstack(stfdf)
t(unstack(stfdf))
unstack(stfdf, which = 2)


###################################################
### chunk number 7: 
###################################################
#line 280 "spacetime.Rnw"
as(stfdf, "xts")


###################################################
### chunk number 8: 
###################################################
#line 289 "spacetime.Rnw"
stfdf[[1]]
stfdf[["values"]]
stfdf[["newVal"]] = rnorm(12)
stfdf$ID
stfdf$ID = paste("OldIDs", 1:12, sep="")
stfdf$NewID = paste("NewIDs", 12:1, sep="")
stfdf


###################################################
### chunk number 9: 
###################################################
#line 315 "spacetime.Rnw"
stfdf[,1] # SpatialPointsDataFrame:
stfdf[,,1]
stfdf[1,,1] # xts
stfdf[,,"ID"]
stfdf[1,,"values", drop=FALSE] # stays STFDF:
stfdf[,1, drop=FALSE] #stays STFDF


###################################################
### chunk number 10: 
###################################################
#line 335 "spacetime.Rnw"
showClass("STPDF")


###################################################
### chunk number 11: 
###################################################
#line 349 "spacetime.Rnw"
showClass("STSDF")
sp = expand.grid(x = 1:3, y = 1:3)
row.names(sp) = paste("point", 1:nrow(sp), sep="")
sp = SpatialPoints(sp)
time = xts(1:9, as.POSIXct("2010-08-05")+3600*(11:19))
m = 1:9 * 10 # means for each of the 9 point locations
mydata = rnorm(length(sp), mean=m)
IDs = paste("ID",1:length(mydata))
mydata = data.frame(values = signif(mydata,3),ID=IDs)
stsdf = STSDF(sp, time, mydata)
stsdf


###################################################
### chunk number 12: 
###################################################
#line 365 "spacetime.Rnw"
stsdf[1:2,]


###################################################
### chunk number 13: 
###################################################
#line 374 "spacetime.Rnw"
stfdf[,time[3]]


###################################################
### chunk number 14: 
###################################################
#line 380 "spacetime.Rnw"
class(stfdf[,time[3],drop=FALSE])


###################################################
### chunk number 15: 
###################################################
#line 387 "spacetime.Rnw"
stfdf[1, , "values"]


###################################################
### chunk number 16: 
###################################################
#line 392 "spacetime.Rnw"
class(stfdf[1,drop=FALSE])


###################################################
### chunk number 17: 
###################################################
#line 404 "spacetime.Rnw"
class(stfdf)
class(as(stfdf, "STPDF"))
class(as(as(stfdf, "STPDF"), "STSDF"))
class(as(stfdf, "STSDF"))


###################################################
### chunk number 18: 
###################################################
#line 412 "spacetime.Rnw"
x = as(stfdf, "STSDF")
class(as(x, "STPDF"))
class(as(as(x, "STPDF"), "STFDF"))
class(as(x, "STFDF"))
xx = as(x, "STFDF")
identical(stfdf, xx)


###################################################
### chunk number 19: 
###################################################
#line 440 "spacetime.Rnw"
.parseISO8601('2010-05')


###################################################
### chunk number 20: 
###################################################
#line 447 "spacetime.Rnw"
.parseISO8601('2010-05-01T13:30/2010-05-01T13:39')


###################################################
### chunk number 21: 
###################################################
#line 461 "spacetime.Rnw"
library(maptools)
fname = system.file("shapes/sids.shp", package="maptools")[1]
nc = readShapePoly(fname, proj4string=CRS("+proj=longlat +datum=NAD27"))
data = data.frame(
	BIR = c(nc$BIR74, nc$BIR79),
	NWBIR = c(nc$NWBIR74, nc$NWBIR79),
	SID = c(nc$SID74, nc$SID79))
time = xts(1:2, 
	as.POSIXct(strptime(c("1974-01-01", "1979-01-01"), "%Y-%m-%d")))
nct = STFDF(
	sp = as(nc, "SpatialPolygons"),
	time = time,
	data = data)
stplot(nct[,,"SID"],as.table=TRUE)


###################################################
### chunk number 22: 
###################################################
#line 480 "spacetime.Rnw"
print(stplot(nct[,,"SID"],as.table=TRUE))


###################################################
### chunk number 23: 
###################################################
#line 517 "spacetime.Rnw"
library(gstat)
data(wind)
wind.loc$y = as.numeric(char2dms(as.character(wind.loc[["Latitude"]])))
wind.loc$x = as.numeric(char2dms(as.character(wind.loc[["Longitude"]])))
coordinates(wind.loc) = ~x+y
proj4string(wind.loc) = "+proj=longlat +datum=WGS84"


###################################################
### chunk number 24: 
###################################################
#line 528 "spacetime.Rnw"
library(mapdata)
plot(wind.loc, xlim = c(-11,-5.4), ylim = c(51,55.5), axes=T, col="red")
map("worldHires", add=T, col = grey(.5))
text(coordinates(wind.loc), pos=1, label=wind.loc$Station, cex=.7)


###################################################
### chunk number 25: 
###################################################
#line 544 "spacetime.Rnw"
wind[1:3,]


###################################################
### chunk number 26: 
###################################################
#line 549 "spacetime.Rnw"
wind$time = ISOdate(wind$year+1900, wind$month, wind$day)
wind$jday = as.numeric(format(wind$time, '%j'))
stations = 4:15
windsqrt = sqrt(0.5148 * wind[stations]) # knots -> m/s
Jday = 1:366
daymeans = apply(sapply(split(windsqrt - mean(windsqrt), wind$jday), mean), 2,  mean)
meanwind = lowess(daymeans ~ Jday, f = 0.1)$y[wind$jday]
velocities = apply(windsqrt, 2, function(x) { x - meanwind })


###################################################
### chunk number 27: 
###################################################
#line 563 "spacetime.Rnw"
# order locations to order of columns in wind;
# connect station names to location coordinates
wind.loc = wind.loc[match(names(wind[4:15]), wind.loc$Code),]
pts = coordinates(wind.loc[match(names(wind[4:15]), wind.loc$Code),])
rownames(pts) = wind.loc$Station
pts = SpatialPoints(pts)
# convert to utm zone 29, to be able to do interpolation in
# proper Euclidian (projected) space:
proj4string(pts) = "+proj=longlat +datum=WGS84"
library(rgdal)
utm29 = CRS("+proj=utm +zone=29 +datum=WGS84")
t = xts(1:nrow(wind), wind$time)
pts = spTransform(pts, utm29)
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
# select april 1961:
w = w[, "1961-04"]
# 10 prediction time points, evenly spread over this month:
n = 10
tgrd = xts(1:n, seq(min(index(w)), max(index(w)), length=n))

# separable covariance model, exponential with ranges 750 km and 1.5 day:
v = list(space = vgm(0.6, "Exp", 750000), time = vgm(1, "Exp", 1.5 * 3600 * 24))
pred = krigeST(sqrt(values)~1, w, STF(grd, tgrd), v)
wind.ST = STFDF(grd, tgrd, data.frame(sqrt_speed = pred))


###################################################
### chunk number 28: 
###################################################
#line 605 "spacetime.Rnw"
layout = list(list("sp.lines", m, col='grey'),
	list("sp.points", pts, first=F, cex=.5))
print(stplot(wind.ST, col.regions=bpy.colors(),
	par.strip.text = list(cex=.5), sp.layout = layout))


###################################################
### chunk number 29: 
###################################################
#line 621 "spacetime.Rnw"
library(lattice)
library(RColorBrewer)
b = brewer.pal(12,"Set3")
par.settings = list(superpose.symbol = list(col = b, fill = b), 
	superpose.line = list(col = b),
	fontsize = list(text=9)) 
print(xyplot(values~time, groups=sp.ID, as.data.frame(w), 
	type='l', auto.key=list(space="right"),
	xlab = "1961", ylab = expression(sqrt(speed)),
	par.settings = par.settings))


###################################################
### chunk number 30: 
###################################################
#line 649 "spacetime.Rnw"
library(diveMove)
library(trip)

locs = readLocs(system.file(file.path("data", "sealLocs.csv"),
	package="diveMove"), idCol=1, dateCol=2,
	dtformat="%Y-%m-%d %H:%M:%S", classCol=3,
	lonCol=4, latCol=5, sep=";")

ringy = subset(locs, id == "ringy" & !is.na(lon) & !is.na(lat))
coordinates(ringy) = ringy[c("lon", "lat")]
tr = trip(ringy, c("time", "id"))
# convert to SPSDFtraj, and plot:
setAs("trip", "STSDFtraj",
	function(from) {
		from$burst = from[[from@TOR.columns[2]]]
		time = from[[from@TOR.columns[1]]]
		new("STSDFtraj", STSDF(as(from, "SpatialPoints"), time, from@data))
	}
)
x = as(tr, "STSDFtraj")
m = map2SpatialLines(map("world", 
	xlim = c(-100,-50), ylim = c(40,77), plot=F))
proj4string(m) = "+proj=longlat +datum=WGS84"
plot(m, axes=TRUE)
plot(x, add=TRUE, line.col = "red")

# convert back, compare:
setAs("STSDFtraj", "trip", function(from) {
		from$time = index(from@time)
		trip(SpatialPointsDataFrame(from@sp, from@data), c("time", "burst"))
	}
)
y = as(x, "trip")
y$burst = NULL
all.equal(y, tr, check.attributes = FALSE)


###################################################
### chunk number 31: 
###################################################
#line 696 "spacetime.Rnw"
library(adehabitat)
# from: adehabitat/demo/managltraj.r
# demo(managltraj)
data(puechabon)
# locations:
locs = puechabon$locs
xy = locs[,c("X","Y")]
### Conversion of the date to the format POSIX
da = as.character(locs$Date)
da = as.POSIXct(strptime(as.character(locs$Date),"%y%m%d")) 
## object of class "ltraj"
ltr = as.ltraj(xy, da, id = locs$Name)
foo = function(dt) dt > 100*3600*24
## The function foo returns TRUE if dt is longer than 100 days
## We use it to cut ltr:
l2 = cutltraj(ltr, "foo(dt)", nextr = TRUE)
ltr.stsdf = as(l2, "STSDFtraj")
# ltr.stsdf[1:10,]
ltr0 = as(ltr.stsdf, "ltraj")
all.equal(l2, ltr0, check.attributes = FALSE)
plot(ltr.stsdf, line.col = c("red", "green", "blue", "darkgreen", "black"),
	axes=TRUE)


