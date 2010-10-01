STS = function(sp, time) {
	new("STS", ST(sp, time))
}

STSDF = function(sp, time, data) {
	if (!is(time, "xts")) {
		if (is(sp, "SpatialGrid"))
			sp = as(sp, "SpatialPixels")
        time = xts(1:length(time), time)
		# rearrange sp and data in new time order:
        o = as.vector(time[,1])
		sp = sp[o] # won't work for SpatialGrid?
		data = data[o,,drop=FALSE]
	}
	new("STSDF", STS(sp, time), data = data)
}

setMethod("coordinates", "STS", function(obj) {
		myCoordinates(obj@sp)
	}
)
index.STS = function(x, ...) {
	index(x@time)
}
index.STSDF = index.STS

as.data.frame.STS = function(x, row.names = NULL, ...) {
  	data.frame(coordinates(x), 
		sp.ID = row.names(x@sp),
		time = index(x),
		row.names = row.names, ...)
}
setAs("STS", "data.frame", function(from) as.data.frame.STS(from))

as.data.frame.STSDF = function(x, row.names = NULL, ...) {
  	f = as.data.frame(as(x, "STS"))
  	data.frame(f, x@data, row.names = row.names, ...)
}
setAs("STSDF", "data.frame", function(from) as.data.frame.STSDF(from))

subs.STSDF <- function(x, i, j, ... , drop = FALSE) {
	missing.i = missing(i)
	missing.j = missing(j)

	if (missing.i && missing.j)
		return(x)

	if (missing.j)
		j = TRUE

	if (missing.i)
		i = TRUE
	
	if (is.character(i)) { # defer to [.xts:
		t = xts(1:nrow(x@time), index(x@time))[i]
		i = as.vector(t[,1])
	}

	x@sp = x@sp[i]
	x@time = x@time[i]
	x@data = x@data[i, j, drop = FALSE]
	if (drop && length(unique(index(x@time))) == 1)
		x = asSpatialDataFrame(x)
	x
}
setMethod("[", "STSDF", subs.STSDF)
