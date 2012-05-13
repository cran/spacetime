ST = function(sp, time) {
	if (!is(time, "xts")) {
		stopifnot(is(time, 
			c("Date", "POSIXct", "timeDate", "yearmon", "yearqtr")))
		t = 1:length(time)
		stopifnot(order(time, t) == t)
		tm = xts(1:length(time), time)
		timeIsInterval(tm) = timeIsInterval(time)
		time = tm
	}
	if (is(sp, "SpatialGrid")) {
		sp = as(sp, "SpatialPixels")
		warning("converted SpatialGrid to SpatialPixels")
	}
	new("ST", sp = sp, time = time)
}

setMethod("[[", c("ST", "ANY", "missing"), 
	function(x, i, j, ...) {
		if (!("data" %in% slotNames(x)))
			stop("no [[ method for object without attributes")
		x@data[[i]]
	}
)
setReplaceMethod("[[", c("ST", "ANY", "missing", "ANY"), 
	function(x, i, j, value) {
		if (!("data" %in% slotNames(x)))
			stop("no [[ method for object without attributes")
		if (is.character(i)) {
			if (any(!is.na(match(i, dimnames(coordinates(x@sp))[[2]]))))
				stop(paste(i, "is already present as a coordinate name!"))
			if (i == "time")
				stop("cannot set time")
		}
		x@data[[i]] <- value
		x
	}
)

setMethod("$", "ST", 
	function(x, name) {
		if (!("data" %in% slotNames(x)))
			stop("no $ method for object without attributes")
		x@data[[name]]
	}
)

setReplaceMethod("$", "ST", 
	function(x, name, value) { 
		if (!("data" %in% slotNames(x)))
			stop("no $<- method for object without attributes")
		x@data[[name]] = value 
		x 
	}
)

dim.ST = function(x) {
	c(length(x@sp), nrow(x@time))
}
dim.STDF = function(x) {
	c(length(x@sp), nrow(x@time), ncol(x@data))
}
dim.STF = dim.STS = dim.STI = dim.ST
dim.STFDF = dim.STSDF = dim.STIDF = dim.STDF

if (!isGeneric("proj4string"))
	setGeneric("proj4string", function(obj)
		standardGeneric("proj4string"))
setMethod("proj4string", "ST", function(obj) proj4string(obj@sp))
if (!isGeneric("proj4string<-"))
	setGeneric("proj4string<-", function(obj, value)
		standardGeneric("proj4string<-"))
setReplaceMethod("proj4string", signature(obj = "ST", value = "ANY"), 
	function(obj,value) { proj4string(obj@sp) = value; obj })
if (!isGeneric("is.projected"))
	setGeneric("is.projected", function(obj)
		standardGeneric("is.projected"))
setMethod("is.projected", "ST", function(obj) is.projected(obj@sp))

if (!isGeneric("spTransform"))
	setGeneric("spTransform", function(x, CRSobj, ...)
		standardGeneric("spTransform"))
spTransform.ST = function(x, CRSobj, ...) {
	x@sp = spTransform(x@sp, CRSobj)
	x
}
setMethod("spTransform", signature("ST", "CRS"), spTransform.ST)

# setMethod("geometry", "ST", function(obj) geometry(obj@sp))

summary.ST = function(object, ...) {
    obj = list()
    obj[["class"]] = class(object)
    obj[["dim"]] = dim(object)
	obj[["sp"]] = summary(object@sp)
	obj[["time"]] = summary(object@time)
    if ("data" %in% slotNames(object))
        if (ncol(object@data) > 1)
                obj[["data"]] = summary(object@data)
            else obj[["data"]] = summary(object@data[[1]])
    class(obj) = "summary.ST"
    obj
}
setMethod("summary", "ST", summary.ST)

print.summary.ST = function(x, ...) {
    cat(paste("Object of class ", x[["class"]], "\n", sep = ""))
	cat(" with Dimensions (s, t, attr): (")
	cat(paste(x[["dim"]], collapse = ", "))
	cat(")\n")
	cat("[[Spatial:]]\n")
    print(x[["sp"]])
	cat("[[Temporal:]]\n")
    print(x[["time"]])
    if (!is.null(x$data)) {
        cat("[[Data attributes:]]\n")
        print(x$data)
    }
    invisible(x)
}

#asSpatialDataFrame = function(x) { # convert to lower
#	stopifnot(length(x@sp) == nrow(x@data))
#	if (is(x@sp, "SpatialPoints"))
#		return(SpatialPointsDataFrame(x@sp, x@data))
#	if (is(x@sp, "SpatialLines"))
#		return(SpatialLinesDataFrame(x@sp, x@data))
#	if (is(x@sp, "SpatialPixels"))
#		return(SpatialPixelsDataFrame(x@sp, x@data))
#	if (is(x@sp, "SpatialGrid"))
#		return(SpatialGridDataFrame(x@sp, x@data))
#	if (is(x@sp, "SpatialPolygons"))
#		return(SpatialPolygonsDataFrame(x@sp, x@data))
#	#if (is(x@sp, "SpatialRings"))
#	#	return(SpatialRingsDataFrame(x@sp, x@data))
#	stop("unknown Spatial class")
#}

if (!isGeneric("aggregate"))
	setGeneric("aggregate", function(x, ...)
		standardGeneric("aggregate"))
if (!isGeneric("aggregateBy"))
	setGeneric("aggregateBy", function(x, by, ...)
		standardGeneric("aggregateBy"))
