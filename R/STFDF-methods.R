STF = function(sp, time) {
	time[,1] = 1:nrow(time) # removes any old indexing
	new("STF", ST(sp, time))
}

STFDF = function(sp, time, data) {
	new("STFDF", STF(sp, time), data = data)
}

myCoordinates = function(x) {
	stopifnot(is(x, "Spatial"))
	if (is(x, "SpatialLines"))
		do.call(rbind, lapply(coordinates(x), function(x) x[[1]][1,]))
	else
		coordinates(x)
}

setMethod("coordinates", "STF", function(obj) {
		mc = myCoordinates(obj@sp)
		matrix(apply(mc, 2, rep, length(obj@time)), ncol = ncol(mc))
	}
)
index.STF = function(x, ...) {
	rep(index(x@time), each = length(x@sp))
}
index.STFDF = index.STF

as.data.frame.STF = function(x, row.names = NULL, ...) {
  	data.frame(coordinates(x), 
		sp.ID = rep(row.names(x@sp), length(x@time)),
		time = index(x),
		row.names = row.names, ...)
}
setAs("STF", "data.frame", function(from) as.data.frame.STF(from))

as.data.frame.STFDF = function(x, row.names = NULL, ...) {
	f = as.data.frame(as(x, "STF"))
	data.frame(f, x@data, row.names = row.names, ...)
}
setAs("STFDF", "data.frame", function(from) as.data.frame.STFDF(from))

unstack.STFDF = function(x, form, which = 1,...) {
  if(missing(form))
#    form = as.formula(paste(names(x@data)[which], 
#      paste(coordnames(x@sp),collapse="+"), sep = "~"))
    form = as.formula(paste(names(x@data)[which], "sp.ID", sep = "~"))
  ret = unstack(as(x, "data.frame"), form, ...)
  rownames(ret) = as.character(index(x@time))
  ret
}

setAs("STFDF", "xts", function(from) xts(unstack(from),index(from@time)))

subs.STFDF <- function(x, i, j, ... , drop = TRUE) {
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
	else {
		if (is(i, "Spatial"))
			s = !is.na(over(x@sp, geometry(i)))
		else 
			s = i
	}
	if (missing.j)
		t = 1:nrow(x@time)
	else
		t = j
	si = rep(1:length(x@sp), nrow(x@time))
	ti = rep(1:nrow(x@time), each = length(x@sp))
	x@sp = x@sp[s]
	x@time = x@time[t] # uses [.xts, deals with character
	t = x@time[,1] # gets the indices
	x@time[,1] = 1:nrow(x@time) # resets indices
	x@data = x@data[si %in% s & ti %in% t, k, drop = FALSE]
	if (drop) {
		if (length(s) == 1) { # space index has only 1 item:
			if (length(t) == 1)
				x = x@data[1,1,drop=TRUE]
			else
				x = xts(x@data, index(x@time))
		} else if (length(t) == 1) # only one data item
			x = addAttrToGeom(x@sp, x@data, match.ID = FALSE)
	}
	x
}
setMethod("[", "STFDF", subs.STFDF)
