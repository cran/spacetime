###################################################
### chunk number 1: 
###################################################
#line 128 "spacetime.Rnw"
library(foreign)
read.dbf(system.file("shapes/sids.dbf", package="maptools"))[1:5,c(5,9:14)]


###################################################
### chunk number 2: 
###################################################
#line 137 "spacetime.Rnw"
data(wind, package = "gstat")
wind[1:6,]


###################################################
### chunk number 3: 
###################################################
#line 151 "spacetime.Rnw"
data("Produc", package = "plm")
Produc[1:5,]


###################################################
### chunk number 4: 
###################################################
#line 198 "spacetime.Rnw"
s = 1:3
t = c(1, 1.5, 3, 4.5)
g = data.frame(rep(t, each=3), rep(s,4))
plot(g, xaxt = 'n', yaxt = 'n', xlab = "Time points", 
	ylab = "Space locations", xlim = c(.5,5), ylim = c(.5,3.5))
abline(h=s, col = grey(.8))
abline(v=t, col = grey(.8))
points(g)
axis(1, at = t, labels = c("1st", "2nd", "3rd", "4th"))
axis(2, at = s, labels = c("1st", "2nd", "3rd"))
text(g, labels = 1:12, pos=4)
title("STFDF (Space-time full data.frame) layout")


###################################################
### chunk number 5: 
###################################################
#line 228 "spacetime.Rnw"
s = 1:3
t = c(1, 2.2, 3, 4.5)
g = data.frame(rep(t, each=3), rep(s,4))
sel = c(1,2,3,5,6,7,11)
plot(g[sel,], xaxt = 'n', yaxt = 'n', xlab = "Time points",                
    ylab = "Space locations", xlim = c(.5,5), ylim = c(.5,3.5))
abline(h=s, col = grey(.8))
abline(v=t, col = grey(.8))
points(g[sel,])
axis(1, at = t, labels = c("1st", "2nd", "3rd", "4th"))
axis(2, at = s, labels = c("1st", "2nd", "3rd"))
text(g[sel,], labels = paste(1:length(sel), "[",c(1,2,3,2,3,1,2),",",c(1,1,1,2,2,3,4),"]", sep=""), pos=4)
title("STSDF (Space-time sparse data.frame) layout")


###################################################
### chunk number 6: 
###################################################
#line 265 "spacetime.Rnw"
s = c(1,2,3,1,4)
t = c(1, 2.2, 2.5, 4, 4.5)
g = data.frame(t,s)
plot(g, xaxt = 'n', yaxt = 'n', xlab = "Time points",                
    ylab = "Space locations", xlim = c(.5,5), ylim = c(.5,4.5))
#abline(h=s, col = grey(.8))
#abline(v=t, col = grey(.8))
arrows(t,s,0.5,s,.1,col='red')
arrows(t,s,t,0.5,.1,col='red')
points(g)
axis(1, at = sort(unique(t)), labels = c("1st", "2nd", "3rd", "4th", "5th"))
axis(2, at = sort(unique(s)), labels = c("1st,4th", "2nd", "3rd", "5th"))
text(g, labels = 1:5, pos=4)
title("STIDF (Space-time irregular data.frame) layout")


###################################################
### chunk number 7: 
###################################################
#line 297 "spacetime.Rnw"
library(spacetime)
showClass("ST")
showClass("STFDF")
sp = cbind(x = c(0,0,1), y = c(0,1,1))
row.names(sp) = paste("point", 1:nrow(sp), sep="")
sp = SpatialPoints(sp)
time = xts(1:4, as.POSIXct("2010-08-05", tz = "GMT")+3600*(10:13))
m = c(10,20,30) # means for each of the 3 point locations
mydata = rnorm(length(sp)*length(time),mean=rep(m, 4))
IDs = paste("ID",1:length(mydata), sep = "_")
mydata = data.frame(values = signif(mydata,3), ID=IDs)
stfdf = STFDF(sp, time, mydata)
str(stfdf)


###################################################
### chunk number 8: 
###################################################
#line 317 "spacetime.Rnw"
as.data.frame(stfdf, row.names = IDs)
as(stfdf, "data.frame")[1:4,]


###################################################
### chunk number 9: 
###################################################
#line 330 "spacetime.Rnw"
unstack(stfdf)
t(unstack(stfdf))
unstack(stfdf, which = 2)


###################################################
### chunk number 10: 
###################################################
#line 339 "spacetime.Rnw"
as(stfdf[,,"values"], "xts")


###################################################
### chunk number 11: 
###################################################
#line 348 "spacetime.Rnw"
stfdf[[1]]
stfdf[["values"]]
stfdf[["newVal"]] = rnorm(12)
stfdf$ID
stfdf$ID = paste("OldIDs", 1:12, sep="")
stfdf$NewID = paste("NewIDs", 12:1, sep="")
stfdf


###################################################
### chunk number 12: 
###################################################
#line 374 "spacetime.Rnw"
stfdf[,1] # SpatialPointsDataFrame:
stfdf[,,1]
stfdf[1,,1] # xts
stfdf[,,"ID"]
stfdf[1,,"values", drop=FALSE] # stays STFDF:
stfdf[,1, drop=FALSE] #stays STFDF


###################################################
### chunk number 13: 
###################################################
#line 394 "spacetime.Rnw"
showClass("STSDF")


###################################################
### chunk number 14: 
###################################################
#line 408 "spacetime.Rnw"
showClass("STIDF")
sp = expand.grid(x = 1:3, y = 1:3)
row.names(sp) = paste("point", 1:nrow(sp), sep="")
sp = SpatialPoints(sp)
time = xts(1:9, as.POSIXct("2010-08-05", tz = "GMT")+3600*(11:19))
m = 1:9 * 10 # means for each of the 9 point locations
mydata = rnorm(length(sp), mean=m)
IDs = paste("ID",1:length(mydata))
mydata = data.frame(values = signif(mydata,3),ID=IDs)
stidf = STIDF(sp, time, mydata)
stidf


###################################################
### chunk number 15: 
###################################################
#line 424 "spacetime.Rnw"
stidf[1:2,]


###################################################
### chunk number 16: 
###################################################
#line 434 "spacetime.Rnw"
stfdf[,time[3]]


###################################################
### chunk number 17: 
###################################################
#line 440 "spacetime.Rnw"
class(stfdf[,time[3]])
class(stfdf[,time[3],drop=FALSE])


###################################################
### chunk number 18: 
###################################################
#line 448 "spacetime.Rnw"
stfdf[1, , "values"]


###################################################
### chunk number 19: 
###################################################
#line 453 "spacetime.Rnw"
class(stfdf[1,])
class(stfdf[1,drop=FALSE])


###################################################
### chunk number 20: 
###################################################
#line 463 "spacetime.Rnw"
class(stfdf)
class(as(stfdf, "STSDF"))
class(as(as(stfdf, "STSDF"), "STIDF"))
class(as(stfdf, "STIDF"))


###################################################
### chunk number 21: 
###################################################
#line 471 "spacetime.Rnw"
x = as(stfdf, "STIDF")
class(as(x, "STSDF"))
class(as(as(x, "STSDF"), "STFDF"))
class(as(x, "STFDF"))
xx = as(x, "STFDF")
identical(stfdf, xx)


###################################################
### chunk number 22: 
###################################################
#line 499 "spacetime.Rnw"
.parseISO8601('2010-05')


###################################################
### chunk number 23: 
###################################################
#line 506 "spacetime.Rnw"
.parseISO8601('2010-05-01T13:30/2010-05-01T13:39')


###################################################
### chunk number 24: 
###################################################
#line 543 "spacetime.Rnw"
library(maptools)
fname = system.file("shapes/sids.shp", package="maptools")[1]
nc = readShapePoly(fname, proj4string=CRS("+proj=longlat +datum=NAD27"))
data = data.frame(
	BIR = c(nc$BIR74, nc$BIR79),
	NWBIR = c(nc$NWBIR74, nc$NWBIR79),
	SID = c(nc$SID74, nc$SID79))
time = xts(1:2, 
	as.POSIXct(strptime(c("1974-01-01", "1979-01-01"), "%Y-%m-%d"), tz = "GMT"))
nct = STFDF(
	sp = as(nc, "SpatialPolygons"),
	time = time,
	data = data)
stplot(nct[,,"SID"], c("1974-1978", "1979-1984"))


###################################################
### chunk number 25: 
###################################################
#line 562 "spacetime.Rnw"
print(stplot(nct[,,"SID"], c("1974-1978", "1979-1984"), par.strip.text = list(cex=.5)))


###################################################
### chunk number 26: 
###################################################
#line 577 "spacetime.Rnw"
library(maps)
states.m = map('state', plot=FALSE, fill=TRUE)
IDs <- sapply(strsplit(states.m$names, ":"), function(x) x[1])
    
library(maptools)
states = map2SpatialPolygons(states.m, IDs=IDs)

library(plm)
data(Produc)

yrs = 1970:1986
time = xts(1:17, as.POSIXct(paste(yrs, "-01-01", sep=""), tz = "GMT"))
# deselect District of Columbia, polygon 8, which is not present in Produc:
Produc.st = STFDF(states[-8], time, Produc[(order(Produc[2], Produc[1])),])
stplot(Produc.st[,,"unemp"], yrs)


###################################################
### chunk number 27: 
###################################################
#line 601 "spacetime.Rnw"
zz <- plm(log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, 
	data = as.data.frame(Produc.st), index = c("state","year"))
summary(zz)


###################################################
### chunk number 28: 
###################################################
#line 638 "spacetime.Rnw"
library(gstat)
data(wind)
wind.loc$y = as.numeric(char2dms(as.character(wind.loc[["Latitude"]])))
wind.loc$x = as.numeric(char2dms(as.character(wind.loc[["Longitude"]])))
coordinates(wind.loc) = ~x+y
proj4string(wind.loc) = "+proj=longlat +datum=WGS84"


###################################################
### chunk number 29: 
###################################################
#line 649 "spacetime.Rnw"
library(mapdata)
plot(wind.loc, xlim = c(-11,-5.4), ylim = c(51,55.5), axes=T, col="red",
	cex.axis =.7)
map("worldHires", add=T, col = grey(.5))
text(coordinates(wind.loc), pos=1, label=wind.loc$Station, cex=.7)


###################################################
### chunk number 30: 
###################################################
#line 666 "spacetime.Rnw"
wind[1:3,]


###################################################
### chunk number 31: 
###################################################
#line 671 "spacetime.Rnw"
wind$time = ISOdate(wind$year+1900, wind$month, wind$day)
wind$jday = as.numeric(format(wind$time, '%j'))
stations = 4:15
windsqrt = sqrt(0.5148 * wind[stations]) # knots -> m/s
Jday = 1:366
daymeans = apply(sapply(split(windsqrt - mean(windsqrt), wind$jday), mean), 2,  mean)
meanwind = lowess(daymeans ~ Jday, f = 0.1)$y[wind$jday]
velocities = apply(windsqrt, 2, function(x) { x - meanwind })


###################################################
### chunk number 32: 
###################################################
#line 685 "spacetime.Rnw"
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
### chunk number 33: 
###################################################
#line 727 "spacetime.Rnw"
layout = list(list("sp.lines", m, col='grey'),
	list("sp.points", pts, first=F, cex=.5))
print(stplot(wind.ST, col.regions=bpy.colors(),
	par.strip.text = list(cex=.5), sp.layout = layout))


###################################################
### chunk number 34: 
###################################################
#line 743 "spacetime.Rnw"
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
### chunk number 35: 
###################################################
#line 771 "spacetime.Rnw"
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
setAs("trip", "STIDFtraj",
	function(from) {
		from$burst = from[[from@TOR.columns[2]]]
		time = from[[from@TOR.columns[1]]]
		new("STIDFtraj", STIDF(as(from, "SpatialPoints"), time, from@data))
	}
)
x = as(tr, "STIDFtraj")
m = map2SpatialLines(map("world", 
	xlim = c(-100,-50), ylim = c(40,77), plot=F))
proj4string(m) = "+proj=longlat +datum=WGS84"
plot(m, axes=TRUE, cex.axis =.7)
plot(x, add=TRUE, col = "red")

# convert back, compare:
setAs("STIDFtraj", "trip", function(from) {
		from$time = index(from@time)
		trip(SpatialPointsDataFrame(from@sp, from@data), c("time", "burst"))
	}
)
y = as(x, "trip")
y$burst = NULL
all.equal(y, tr, check.attributes = FALSE)


###################################################
### chunk number 36: 
###################################################
#line 818 "spacetime.Rnw"
library(adehabitatLT)
# from: adehabitat/demo/managltraj.r
# demo(managltraj)
data(puechabonsp)
# locations:
locs = puechabonsp$relocs
xy = coordinates(locs)
### Conversion of the date to the format POSIX
da = as.character(locs$Date)
da = as.POSIXct(strptime(as.character(locs$Date),"%y%m%d"), tz = "GMT") 
## object of class "ltraj"
ltr = as.ltraj(xy, da, id = locs$Name)
foo = function(dt) dt > 100*3600*24
## The function foo returns TRUE if dt is longer than 100 days
## We use it to cut ltr:
l2 = cutltraj(ltr, "foo(dt)", nextr = TRUE)
stidfTrj = as(l2, "STIDFtraj")
ltr0 = as(stidfTrj, "ltraj")
all.equal(l2, ltr0, check.attributes = FALSE)
plot(stidfTrj, col = c("red", "green", "blue", "darkgreen", "black"),
	axes=TRUE)


###################################################
### chunk number 37:  eval=FALSE
###################################################
## #line 844 "spacetime.Rnw"
## stplot(stidfTrj,by="time*id")


###################################################
### chunk number 38: 
###################################################
#line 851 "spacetime.Rnw"
print(stplot(stidfTrj, by="time*id"))


###################################################
### chunk number 39: 
###################################################
#line 865 "spacetime.Rnw"
library(cshapes)
cs = cshp()
names(cs)


###################################################
### chunk number 40: 
###################################################
#line 877 "spacetime.Rnw"
cshp.2002 <- cshp(date=as.Date("2002-6-30"), useGW=TRUE)


###################################################
### chunk number 41: 
###################################################
#line 884 "spacetime.Rnw"
t = as.POSIXct(strptime(paste(cs$COWSYEAR, 
	cs$COWSMONTH,cs$COWSDAY, sep="-"), "%Y-%m-%d"), tz = "GMT")
st = STIDF(geometry(cs), t, as.data.frame(cs))
pt = SpatialPoints(cbind(7, 52), CRS(proj4string(cs)))
as.data.frame(st[pt,,1:5])


