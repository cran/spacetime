STI = function(sp, time) {
	new("STI", ST(sp, time))
}

STIDF = function(sp, time, data) {
	if (!is(time, "xts")) {
        time = xts(1:length(time), time)
		# rearrange sp and data in new time order:
        o = as.vector(time[,1])
		sp = sp[o,]
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
	timedata = x@time
	if (is.null(row.names(x@sp)))
		row.names(x@sp) = 1:nrow(x@sp)
  	ret = data.frame(as.data.frame(coordinates(x)), 
		sp.ID = row.names(x@sp),
		time = index(x),
		timedata,
		row.names = row.names, ...)
	if ("data" %in% slotNames(x@sp))
		ret = data.frame(ret, x@sp@data)
	ret
}
setAs("STI", "data.frame", function(from) as.data.frame.STI(from))

as.data.frame.STIDF = function(x, row.names = NULL, ...) {
  	f = as.data.frame(as(x, "STI"))
  	data.frame(f, x@data, row.names = row.names, ...)
}
setAs("STIDF", "data.frame", function(from) as.data.frame.STIDF(from))

as.xts.STIDF = function(x, ...) xts(x@data, index(x@time))

setAs("STIDF", "xts", function(from) as.xts.STIDF(from))

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
	x@data = x@data[i, k, drop = FALSE]
	if (drop && length(unique(index(x@time))) == 1)
		x = addAttrToGeom(x@sp, x@data, match.ID = FALSE)
	x
}
setMethod("[", "STIDF", subs.STIDF)

setMethod("addAttrToGeom", signature(x = "STI", y = "data.frame"),
    function(x, y, match.ID, ...)
		new("STIDF", x, data = y)
)

length.STI = function(x) { length(x@sp) }

length.STIDF = function(x) { length(x@sp) }

setMethod("geometry", "STIDF", function(obj) as(obj, "STI"))
