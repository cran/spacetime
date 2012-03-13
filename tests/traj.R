library(spacetime)
library(maps)
library(mapdata)
library(maptools)
###################################################
### code chunk number 48: spacetime.Rnw:1039-1049
###################################################
library("diveMove")
library("trip")
locs = readLocs(gzfile(system.file(file.path("data", "sealLocs.csv.gz"),
	package="diveMove")), idCol=1, dateCol=2,
	dtformat="%Y-%m-%d %H:%M:%S", classCol=3,
	lonCol=4, latCol=5, sep=";")

ringy = subset(locs, id == "ringy" & !is.na(lon) & !is.na(lat))
coordinates(ringy) = ringy[c("lon", "lat")]
tr = trip(ringy, c("time", "id"))


###################################################
### code chunk number 49: spacetime.Rnw:1052-1068
###################################################
setAs("trip", "STTDF",
	function(from) {
		from$burst = from[[from@TOR.columns[2]]]
		time = from[[from@TOR.columns[1]]]
		#from = from[order(time),]
        STIbox = STI(SpatialPoints(t(bbox(from))), range(time))
		STT = new("STT", STIbox, traj = list(STI(geometry(from), time)))
		new("STTDF", STT, data = from@data)
	}
)
x = as(tr, "STTDF")
m = map2SpatialLines(map("world", 
	xlim = c(-100,-50), ylim = c(40,77), plot=F))
proj4string(m) = "+proj=longlat +datum=WGS84"
plot(m, axes=TRUE, cex.axis =.7)
plot(x, add=TRUE, col = "red")


###################################################
### code chunk number 50: spacetime.Rnw:1073-1075
###################################################
plot(m, axes=TRUE, cex.axis =.7)
plot(x, add=TRUE, col = "red")


###################################################
### code chunk number 51: spacetime.Rnw:1089-1098
###################################################
library("adehabitatLT")
data("puechabonsp")
locs = puechabonsp$relocs
xy = coordinates(locs)
da = as.character(locs$Date)
da = as.POSIXct(strptime(as.character(locs$Date),"%y%m%d"), tz = "GMT") 
ltr = as.ltraj(xy, da, id = locs$Name)
foo = function(dt) dt > 100*3600*24
l2 = cutltraj(ltr, "foo(dt)", nextr = TRUE)

###################################################
### code chunk number 52: spacetime.Rnw:1102-1104 (eval = FALSE)
###################################################
sttdf = as(l2, "STTDF")
print(stplot(sttdf, by="time*id"))
