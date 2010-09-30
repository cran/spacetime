STS = function(sp, time) {
	new("STS", ST(sp, time))
}

STSDF = function(sp, time, data) {
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

as.data.frame.STS = function(x, row.names, ...) {
  	data.frame(coordinates(x), 
		sp.ID = row.names(x@sp),
		time = index(x),
		row.names = row.names, ...)
}
setAs("STS", "data.frame", function(from) as.data.frame.STPDF(from))

as.data.frame.STSDF = function(x, row.names = 1:nrow(x@data), ...) {
  	data.frame(x@data, as.data.frame(as(x, "STS")),
		row.names = row.names, ...)
}
setAs("STSDF", "data.frame", function(from) as.data.frame.STPDF(from))

subs.STSDF <- function(x, i, j, ... , drop = FALSE) {
	missing.i = missing(i)
	missing.j = missing(j)

	if (missing.i && missing.j)
		return(x)

	if (missing.j)
		j = TRUE

	if (missing.i)
		i = TRUE
	
	if (is.character(i)) {
		t = xts(1:nrow(x@time), index(x@time))[i]
		i = as.vector(t[,1])
	}

	x@sp = x@sp[i]
	x@time = initTime(x@time[i])
	x@data = x@data[i, j, drop = FALSE]
	x
}
setMethod("[", "STSDF", subs.STSDF)

subs000.STSDF <- function(x, i, j, ... , drop = FALSE) {
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

	if (missing.j)
		#j = 1:nrow(x@time)
		t = TRUE
	else {
		if (is.character(j)) {
			t = xts(1:nrow(x@time), index(x@time))
			t = t[j]
			t = as.vector(t[,1])
		} else
			t = j
	}

	sel = rep(FALSE, nrow(x@time))
	sel[t] = TRUE

	if (missing.i)
		s = TRUE
	else
		s = i

	s = rep(TRUE, nrow(x@time))[s]

	sel = s & t
	x@sp = x@sp[sel]
	x@time = initTime(x@time[sel])
	x@data = x@data[sel, k, drop = FALSE]
	x
}
