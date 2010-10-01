STP = function(sp, time, index) {
	time[,1] = 1:nrow(time) # reset any original order
	new("STP", sp = sp, time = time, index = index)
}

STPDF = function(sp, time, data, index) {
	new("STPDF", STP(sp, time, index), data = data)
}

setMethod("coordinates", "STP", function(obj) {
		myCoordinates(obj@sp)[obj@index[,1],]
	}
)

index.STP = function(x, ...) {
	index(x@time)[x@index[,2]]
}
index.STPDF = index.STP

as.data.frame.STP = function(x, row.names = NULL, ...) {
  	data.frame(coordinates(x), 
		sp.ID = row.names(x@sp)[x@index[,1]],
		time = index(x),
		row.names = row.names, ...)
}
setAs("STP", "data.frame", function(from) as.data.frame.STP(from))

as.data.frame.STPDF = function(x, row.names = NULL, ...) {
	f = as.data.frame(as(x, "STP"))
  	data.frame(f, x@data, row.names = row.names, ...)
}
setAs("STPDF", "data.frame", function(from) as.data.frame.STPDF(from))

subs.STPDF <- function(x, i, j, ... , drop = TRUE) {
	n.args = nargs()
	dots = list(...)
	missing.i = missing(i)
	missing.j = missing(j)
	if (length(dots) > 0) {
		missing.k = FALSE
		k = dots[[1]]
	} else
		missing.k = TRUE
	if (missing.i && missing.j && missing.k)
		return(x)

	if (missing.k) {
		k = TRUE
	} else if (missing.j && n.args == 2) {
		x@data = x@data[ , k, drop = FALSE]
		return(x)
	} 
	if (missing.i)
		s = 1:length(x@sp)
	else
		s = i
	if (missing.j)
		t = 1:nrow(x@time)
	else
		#t = j -- will not work for character j
		t = x@time[j,1]
	si = rep(1:length(x@sp), nrow(x@time))
	ti = rep(1:nrow(x@time), each = length(x@sp))
	#x@sp = x@sp[s] -- time and space topology not touched
	#x@time = x@time[t]
	sel = si %in% s & ti %in% t
	x@data = x@data[sel, k, drop = FALSE]
	x@index = x@index[sel,] # -- so index number remain valid
	if (drop) {
		if (length(s) == 1) { # space index has only 1 item:
			if (length(t) == 1)
				x = x@data[1,1,drop=TRUE]
			else
				x = xts(x@data, index(x@time))
		} else if (length(t) == 1) # only one time item
			x = asSpatialDataFrame(x)
	}
	x
}
setMethod("[", "STPDF", subs.STPDF)
