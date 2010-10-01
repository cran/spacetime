###################################################
### chunk number 4: 
###################################################
library(spacetime)
set.seed(13579) # to make outcome a bit predictable!
sp = cbind(x = c(0,0,1), y = c(0,1,1))
row.names(sp) = paste("point", 1:nrow(sp), sep="")
sp = SpatialPoints(sp)
time = xts(1:4, as.POSIXct("2010-08-05")+3600*(10:13))
m = c(10,20,30) # means for each of the 3 point locations
mydata = rnorm(length(sp)*length(time),mean=rep(m, 4))
IDs = paste("ID", 1:length(mydata), sep = "_")
mydata = data.frame(values = signif(mydata,3), ID=IDs)
stfdf = STFDF(sp, time, mydata)


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
stfdf[["newVal"]] <- rnorm(12)
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

stpdf = as(stfdf, "STPDF")
stpdf[[1]]
stpdf[["values"]]
stpdf[["newVal"]] <- rnorm(12)
stpdf$ID
stpdf$ID = paste("OldIDs", 1:12, sep="")
stpdf$NewID = paste("NewIDs", 12:1, sep="")
stpdf
stpdf[,1] # SpatialPointsDataFrame:
stpdf[,,1]
stpdf[1,,1] # xts
stpdf[,,"ID"]
stpdf[1,,"values", drop=FALSE] # stays STSDF:
stpdf[,1, drop=FALSE] #stays STSDF
as.data.frame(stpdf)
as(stpdf, "data.frame")

stsdf = as(stfdf, "STSDF")
stsdf[[1]]
stsdf[["values"]]
stsdf[["newVal"]] <- rnorm(12)
stsdf$ID
stsdf$ID = paste("OldIDs", 1:12, sep="")
stsdf$NewID = paste("NewIDs", 12:1, sep="")
stsdf
stsdf[,1] # SpatialPointsDataFrame:
stsdf[,,1]
stsdf[1,,1] # xts
stsdf[,,"ID"]
stsdf[1,,"values", drop=FALSE] # stays STSDF:
stsdf[,1, drop=FALSE] #stays STSDF
as.data.frame(stsdf)
as(stsdf, "data.frame")
