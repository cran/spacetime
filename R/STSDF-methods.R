STS = function(sp, time, index) {
	time[,1] = 1:nrow(time) # reset any original order
	new("STS", sp = sp, time = time, index = index)
}

STSDF = function(sp, time, data, index) {
	new("STSDF", STS(sp, time, index), data = data)
}

setMethod("coordinates", "STS", function(obj) {
		myCoordinates(obj@sp)[obj@index[,1],]
	}
)

index.STS = function(x, ...) {
	index(x@time)[x@index[,2]]
}
index.STSDF = index.STS

as.data.frame.STS = function(x, row.names = NULL, ...) {
  	data.frame(coordinates(x), 
		sp.ID = row.names(x@sp)[x@index[,1]],
		time = index(x),
		row.names = row.names, ...)
}
setAs("STS", "data.frame", function(from) as.data.frame.STS(from))

as.data.frame.STSDF = function(x, row.names = NULL, ...) {
	f = as.data.frame(as(x, "STS"))
  	data.frame(f, x@data, row.names = row.names, ...)
}
setAs("STSDF", "data.frame", function(from) as.data.frame.STSDF(from))

subs.STSDF <- function(x, i, j, ... , drop = TRUE) {
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
	si = x@index[,1] 
	  # instead of: si = rep(1:length(x@sp), nrow(x@time)) # BG
	ti = x@index[,2] 
	  # instead of: ti = rep(1:nrow(x@time), each = length(x@sp)) # BG
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
			x = addAttrToGeom(x@sp, x@data, match.ID = FALSE)
	}
	x
}
setMethod("[", "STSDF", subs.STSDF)
