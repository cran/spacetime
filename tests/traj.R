###################################################
### chunk number 25: 
###################################################
library(spacetime)
library(diveMove)
library(trip)

locs <- readLocs(gzfile(system.file(file.path("data", "sealLocs.csv.gz"),
	package="diveMove")), idCol=1, dateCol=2,
	dtformat="%Y-%m-%d %H:%M:%S", classCol=3,
	lonCol=4, latCol=5, sep=";")

ringy <- subset(locs, id == "ringy" & !is.na(lon) & !is.na(lat))
coordinates(ringy) <- ringy[c("lon", "lat")]
tr <- trip(ringy, c("time", "id"))

setClass("STIDFtrip", representation("STIDF", TOR.columns = "character"))
setAs("trip", "STIDFtrip",
	function(from) {
		new("STIDFtrip", STIDF(as(from, "SpatialPoints"), 
				from[[from@TOR.columns[1]]], from@data), 
			TOR.columns = from@TOR.columns)
	}
)
setAs("STIDFtrip", "trip", function(from) 
	trip(SpatialPointsDataFrame(from@sp, from@data), from@TOR.columns)
)
x = as(tr, "STIDFtrip")
y = as(x, "trip")
all.equal(y, tr)

###################################################
### chunk number 26: 
###################################################
library(adehabitatLT)
# from: adehabitat/demo/managltraj.r
# demo(managltraj)
data(puechabonsp)
# locations:
locs <- puechabonsp$relocs
xy <- coordinates(locs)
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
setClass("STIDFltraj", representation("STIDF"))
setAs("ltraj", "STIDFltraj", 
	function(from) {
		d = do.call(rbind, from)
		n = unlist(lapply(from, nrow))
		d$id = rep(unlist(t(sapply(from, attributes))[,4]), times = n)
		d$burst = rep(unlist(t(sapply(from, attributes))[,5]), times = n)
		new("STIDFltraj", STIDF(SpatialPoints(d[c("x","y")]), d$date, d))
	}
)
setAs("STIDFltraj", "ltraj", 
	function(from) {
		xy = coordinates(from@sp)
		da = index(from@time)
		as.ltraj(xy, da, id = from@data[,"id"], burst = from@data[,"burst"])
	}
)

ltr.stsdf = as(l2, "STIDFltraj")
ltr.stsdf[1:10,]
ltr0 = as(ltr.stsdf, "ltraj")
all.equal(l2, ltr0, check.attributes = FALSE)

