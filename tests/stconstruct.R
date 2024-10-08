suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(spacetime))
suppressPackageStartupMessages(library(xts))

# example 0: construction with STFDF:

## IGNORE_RDIFF_BEGIN
if (require(maps, quietly = TRUE)) {
states.m = map('state', plot=FALSE, fill=TRUE)
IDs <- sapply(strsplit(states.m$names, ":"), function(x) x[1])
    
if (require(sf, quietly = TRUE)) {
sta = st_as_sf(states.m)
row.names(sta) = unique(IDs)
states = geometry(as(sta, "Spatial"))

if (require(plm, quietly = TRUE)) {
data(Produc)

yrs = 1970:1986
time = xts(1:17, as.POSIXct(paste(yrs, "-01-01", sep=""), tz = "GMT"))
# deselect District of Columbia, polygon 8, which is not present in Produc:
Produc.st = STFDF(states[-8], time, Produc[(order(Produc[,2], Produc[,1])),])
#stplot(Produc.st[,,"unemp"], yrs)

# example 1: st from long table, with states as Spatial object:
# use Date format for time:
Produc$time = as.Date(paste(yrs, "01", "01", sep = "-"))
xy = coordinates(states[-8])
Produc$x = xy[,1]
Produc$y = xy[,2]
#using stConstruct, use polygon centroids for location:
x = stConstruct(Produc, c("x", "y"), "time")
class(x)
stplot(x[,,"unemp"])

# alternatively, pass states:
Produc$state = gsub("TENNESSE", "TENNESSEE", Produc$state)
Produc$State = gsub("_", " ", tolower(Produc$state))
x = stConstruct(Produc, "State", "time", states[-8])
class(x)
#stplot(x[,,"unemp"], yrs)

if (require(sf, quietly = TRUE)) {
# stConstruct multivariable, time-wide
fname = system.file("shape/nc.shp", package="sf")[1]
nc = as(read_sf(fname), "Spatial")
timesList = list(
	BIR=c("BIR74", "BIR79"), 
	NWBIR=c("NWBIR74", "NWBIR79"), 
	SID=c("SID74", "SID79")
)
t = xts(1:2, as.Date(c("1974-01-01","1979-01-01")))
nc.st = stConstruct(as(nc, "data.frame"), geometry(nc), timesList,
	TimeObj = t)
}
}}}
## IGNORE_RDIFF_END
