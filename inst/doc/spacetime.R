###################################################
### chunk number 1: 
###################################################
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
as.data.frame(stfdf, row.names = IDs)
as(stfdf, "data.frame")[1:4,]


###################################################
### chunk number 6: 
###################################################
unstack(stfdf)
t(unstack(stfdf))
unstack(stfdf, which = 2)


###################################################
### chunk number 7: 
###################################################
as(stfdf, "xts")


###################################################
### chunk number 8: 
###################################################
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
stfdf[,1] # SpatialPointsDataFrame:
stfdf[,,1]
stfdf[1,,1] # xts
stfdf[,,"ID"]
stfdf[1,,"values", drop=FALSE] # stays STFDF:
stfdf[,1, drop=FALSE] #stays STFDF


###################################################
### chunk number 10: 
###################################################
showClass("STPDF")


###################################################
### chunk number 11: 
###################################################
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
stsdf[1:2,]


###################################################
### chunk number 13: 
###################################################
stfdf[,time[3]]


###################################################
### chunk number 14: 
###################################################
class(stfdf[,time[3],drop=FALSE])


###################################################
### chunk number 15: 
###################################################
stfdf[1, , "values"]


###################################################
### chunk number 16: 
###################################################
class(stfdf[1,drop=FALSE])


###################################################
### chunk number 17: 
###################################################
class(stfdf)
class(as(stfdf, "STPDF"))
class(as(as(stfdf, "STPDF"), "STSDF"))
class(as(stfdf, "STSDF"))


###################################################
### chunk number 18: 
###################################################
x = as(stfdf, "STSDF")
class(as(x, "STPDF"))
class(as(as(x, "STPDF"), "STFDF"))
class(as(x, "STFDF"))
xx = as(x, "STFDF")
identical(stfdf, xx)


###################################################
### chunk number 19: 
###################################################
.parseISO8601('2010-05')


###################################################
### chunk number 20: 
###################################################
.parseISO8601('2010-05-01T13:30/2010-05-01T13:39')


###################################################
### chunk number 21: 
###################################################
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
print(stplot(nct[,,"SID"],as.table=TRUE))


###################################################
### chunk number 23: 
###################################################
library(gstat)
data(wind)
wind.loc$y = as.numeric(char2dms(as.character(wind.loc[["Latitude"]])))
wind.loc$x = as.numeric(char2dms(as.character(wind.loc[["Longitude"]])))
coordinates(wind.loc) = ~x+y
proj4string(wind.loc) = "+proj=longlat +datum=WGS84"


###################################################
### chunk number 24: 
###################################################
library(mapdata)
plot(wind.loc, xlim = c(-11,-5.4), ylim = c(51,55.5), axes=T, col="red")
map("worldHires", add=T, col = grey(.5))
text(coordinates(wind.loc), pos=1, label=wind.loc$Station, cex=.7)


###################################################
### chunk number 25: 
###################################################
wind[1:3,]


###################################################
### chunk number 26: 
###################################################
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
layout = list(list("sp.lines", m, col='grey'),
	list("sp.points", pts, first=F, cex=.5))
print(stplot(wind.ST, col.regions=bpy.colors(),
	par.strip.text = list(cex=.5), sp.layout = layout))


###################################################
### chunk number 29: 
###################################################
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
library(diveMove)
library(trip)

locs = readLocs(system.file(file.path("data", "sealLocs.csv"),
	package="diveMove"), idCol=1, dateCol=2,
	dtformat="%Y-%m-%d %H:%M:%S", classCol=3,
	lonCol=4, latCol=5, sep=";")

ringy = subset(locs, id == "ringy" & !is.na(lon) & !is.na(lat))
coordinates(ringy) = ringy[c("lon", "lat")]
tr = trip(ringy, c("time", "id"))

x = as(tr, "STSDFtraj")
y = as(x, "trip")
all.equal(y, tr)


###################################################
### chunk number 31: 
###################################################
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

foo = function(dt) {
    return(dt > 100*3600*24)
}
## The function foo returns TRUE if dt is longer than 100 days
## We use it to cut ltr:
l2 = cutltraj(ltr, "foo(dt)", nextr = TRUE)
ltr.stsdf = as(l2, "STSDFltraj")
ltr.stsdf[1:10,]
ltr0 = as(ltr.stsdf, "ltraj")
all.equal(l2, ltr0, check.attributes = FALSE)
plot(ltr.stsdf, line.col = c("red", "green", "blue", "darkgreen", "black"),
	axes=T)


