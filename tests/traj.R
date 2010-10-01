###################################################
### chunk number 25: 
###################################################
library(spacetime)
library(diveMove)
library(trip)

locs <- readLocs(system.file(file.path("data", "sealLocs.csv"),
	package="diveMove"), idCol=1, dateCol=2,
	dtformat="%Y-%m-%d %H:%M:%S", classCol=3,
	lonCol=4, latCol=5, sep=";")

ringy <- subset(locs, id == "ringy" & !is.na(lon) & !is.na(lat))
coordinates(ringy) <- ringy[c("lon", "lat")]
tr <- trip(ringy, c("time", "id"))

setClass("STSDFtrip", representation("STSDF", TOR.columns = "character"))
setAs("trip", "STSDFtrip",
	function(from) {
		new("STSDFtrip", STSDF(as(from, "SpatialPoints"), 
				from[[from@TOR.columns[1]]], from@data), 
			TOR.columns = from@TOR.columns)
	}
)
setAs("STSDFtrip", "trip", function(from) 
	trip(SpatialPointsDataFrame(from@sp, from@data), from@TOR.columns)
)
x = as(tr, "STSDFtrip")
y = as(x, "trip")
all.equal(y, tr)


###################################################
### chunk number 26: 
###################################################
library(adehabitat)
# from: adehabitat/demo/managltraj.r
# demo(managltraj)
data(puechabon)
# locations:
locs <- puechabon$locs
xy <- locs[,c("X","Y")]
### Conversion of the date to the format POSIX
da <- as.character(locs$Date)
da <- as.POSIXct(strptime(as.character(locs$Date),"%y%m%d")) 

## object of class "ltraj"
ltr <- as.ltraj(xy, da, id = locs$Name)

foo <- function(dt) {
    return(dt> (100*3600*24))
}
## The function foo returns TRUE if dt is longer than 100 days
## We use it to cut ltr:
l2 <- cutltraj(ltr, "foo(dt)", nextr = TRUE)

setClass("ltraj", representation("list"))
setClass("STSDFltraj", representation("STSDF"))
setAs("ltraj", "STSDFltraj", 
	function(from) {
		d = do.call(rbind, from)
		n = unlist(lapply(from, nrow))
		d$id = rep(unlist(t(sapply(from, attributes))[,4]), times = n)
		d$burst = rep(unlist(t(sapply(from, attributes))[,5]), times = n)
		new("STSDFltraj", STSDF(SpatialPoints(d[c("x","y")]), d$date, d))
	}
)
setAs("STSDFltraj", "ltraj", 
	function(from) {
		xy = coordinates(from@sp)
		da = index(from@time)
		as.ltraj(xy, da, id = from@data[,"id"], burst = from@data[,"burst"])
	}
)

ltr.stsdf = as(l2, "STSDFltraj")
ltr.stsdf[1:10,]
ltr0 = as(ltr.stsdf, "ltraj")
all.equal(l2, ltr0, check.attributes = FALSE)

