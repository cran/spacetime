STI = function(sp, time) {
	new("STI", ST(sp, time))
}

STIDF = function(sp, time, data) {
	if (!is(time, "xts")) {
		if (is(sp, "SpatialGrid"))
			sp = as(sp, "SpatialPixels")
        time = xts(1:length(time), time)
		# rearrange sp and data in new time order:
        o = as.vector(time[,1])
		sp = sp[o] # won't work for SpatialGrid?
		data = data[o,,drop=FALSE]
	}
	new("STIDF", STI(sp, time), data = data)
}

setMethod("coordinates", "STI", function(obj) {
		myCoordinates(obj@sp)
	}
)
index.STI = function(x, ...) {
	index(x@time)
}
index.STIDF = index.STI

as.data.frame.STI = function(x, row.names = NULL, ...) {
  	data.frame(coordinates(x), 
		sp.ID = row.names(x@sp),
		time = index(x),
		row.names = row.names, ...)
}
setAs("STI", "data.frame", function(from) as.data.frame.STI(from))

as.data.frame.STIDF = function(x, row.names = NULL, ...) {
  	f = as.data.frame(as(x, "STI"))
  	data.frame(f, x@data, row.names = row.names, ...)
}
setAs("STIDF", "data.frame", function(from) as.data.frame.STIDF(from))

as.xts.STIDF = function(x, ...) xts(x@data, index(x@sp))

subs.STIDF <- function(x, i, j, ... , drop = FALSE) {
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

	if (missing.i)
		i = TRUE
	else if (is(i, "Spatial"))
		i = !is.na(over(x@sp,i))

	if (missing.j)
		j = rep(TRUE, length=nrow(x@time))
	else {
		t = xts(1:nrow(x@time), index(x@time))[j]
		j = as.vector(t[,1])
	}
	
	if(is.numeric(i))
		i = 1:nrow(x@time) %in% i
	if(is.numeric(j))
		j = 1:nrow(x@time) %in% j

	i = i & j

	x@sp = x@sp[i]
	x@time = x@time[i]
	x@data = x@data[i, k, drop = FALSE]
	if (drop && length(unique(index(x@time))) == 1)
		x = addAttrToGeom(x@sp, x@data, match.ID = FALSE)
	x
}
setMethod("[", "STIDF", subs.STIDF)
