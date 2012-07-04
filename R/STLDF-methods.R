STL = function(sp, time, endTime) {
	if (is.na(timeIsInterval(time))) {
		#warning("STI(): setting timeIsInterval to default value FALSE\n")
		timeIsInterval(time) = FALSE
	}
	if (is.na(timeIsInterval(endTime))) {
		#warning("STI(): setting timeIsInterval to default value FALSE\n")
		timeIsInterval(endTime) = FALSE
	}
	stopifnot(is(endTime, "POSIXt"))
	new("STL", ST(sp, time), endTime = endTime)
}

STLDF = function(sp, time, endTime, data) {
	if (!is(time, "xts")) {
		time0 = time
        time = xts(1:length(time), time)
		timeIsInterval(time) = timeIsInterval(time0)
		# rearrange sp and data in new time order:
        o = as.vector(time[,1])
		sp = sp[o,]
		endTime = endTime[o]
		data = data[o,,drop=FALSE]
	}
	new("STLDF", STL(sp, time, endTime), data = data)
}

setMethod("coordinates", "STL", function(obj) {
		myCoordinates(obj@sp)
	}
)
index.STL = function(x, ...) {
	data.frame(time = index(x@time), endTime = index(x@endTime))
}
index.STLDF = index.STL

as.data.frame.STL = function(x, row.names = NULL, ...) {
	timedata = x@time
	if (is.null(row.names(x@sp)))
		row.names(x@sp) = 1:nrow(x@sp)
  	ret = data.frame(as.data.frame(coordinates(x)), 
		sp.ID = row.names(x@sp),
		time = index(x)[,1],
		endTime = index(x)[,2],
		timedata,
		row.names = row.names, ...)
	if ("data" %in% slotNames(x@sp))
		ret = data.frame(ret, x@sp@data)
	ret
}
setAs("STL", "data.frame", function(from) as.data.frame.STL(from))

as.data.frame.STLDF = function(x, row.names = NULL, ...) {
  	f = as.data.frame(as(x, "STL"))
  	data.frame(f, x@data, row.names = row.names, ...)
}
setAs("STLDF", "data.frame", function(from) as.data.frame.STLDF(from))

as.xts.STLDF = function(x, ...) xts(x@data, index(x@time))

setAs("STLDF", "xts", function(from) as.xts.STLDF(from))

subs.STLDF <- function(x, i, j, ... , drop = FALSE) {
	missing.i = missing(i)
	missing.j = missing(j)
	missing.k = k = TRUE
	dots = list(...)
    if (length(dots) > 0) {
        missing.k = FALSE
        k = dots[[1]]
    }

	if (missing.i && missing.j && missing.k)
		return(x)

	# space
	if (missing.i)
		i = TRUE
	if (is(i, "Spatial"))
		i = !is.na(over(x@sp, geometry(i)))
	if (is.logical(i)) {
		i = rep(i, length.out = length(x@sp))
		i = which(i)
	} else if (is.character(i)) { # suggested by BG:
		i = match(i, row.names(x@sp), nomatch = FALSE)
	}

	# time
	if (missing.j)
		j = rep(TRUE, length=nrow(x@time))
	else {
		if (is.logical(j))
			j = which(j)
		t = xts(1:nrow(x@time), index(x@time))[j]
		j = as.vector(t[,1])
	}
	
	if(is.numeric(i))
		i = 1:nrow(x@time) %in% i
	if(is.numeric(j))
		j = 1:nrow(x@time) %in% j

	i = i & j

	x@sp = x@sp[i,]
	x@time = x@time[i]
	x@endTime = x@endTime[i]
	x@data = x@data[i, k, drop = FALSE]
	if (drop && length(unique(index(x@time))) == 1)
		x = addAttrToGeom(x@sp, x@data, match.ID = FALSE)
	x
}
setMethod("[", "STLDF", subs.STLDF)

setMethod("addAttrToGeom", signature(x = "STL", y = "data.frame"),
    function(x, y, match.ID, ...)
		new("STLDF", x, data = y)
)

length.STL = function(x) { length(x@sp) }

length.STLDF = function(x) { length(x@sp) }

setMethod("geometry", "STLDF", function(obj) as(obj, "STL"))
