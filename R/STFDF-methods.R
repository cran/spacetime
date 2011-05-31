STF = function(sp, time) {
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
	if (is.null(row.names(x@sp)))
		row.names(x@sp) = 1:nrow(x@sp)
	timedata = apply(x@time, 2, rep, each = length(x@sp))
  	ret = data.frame(as.data.frame(coordinates(x)), 
		sp.ID = rep(row.names(x@sp), length(x@time)),
		time = index(x),
		timedata,
		row.names = row.names, ...)
	if ("data" %in% slotNames(x@sp)) {
		df = data.frame(apply(x@sp@data, 2, rep, length(x@time)))
		ret = data.frame(ret, df)
	}
	ret
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

	si = rep(1:length(x@sp), nrow(x@time))
	ti = rep(1:nrow(x@time), each = length(x@sp))

	if (missing.i)
		s = 1:length(x@sp)
	else {
		if (is(i, "Spatial")) {
			s = which(!is.na(over(x@sp, geometry(i))))
		} else if (is.logical(i)) {
			i = rep(i, length.out = length(x@sp))
			s = which(i)
		} else
			s = i
	}
	x@sp = x@sp[s,]

	if (missing.j)
		t = 1:nrow(x@time)
	else {
		if (is.logical(j))
			j = which(j)
		nc = ncol(x@time)
		x@time = cbind(x@time, 1:nrow(x@time))
		# uses [.xts, deals with character/iso8601;
		# takes care of negative indices:
		x@time = x@time[j] 
		# get back the corresponding index vector t, to use for @data:
		t = x@time[, nc+1]
		x@time = x@time[,-(nc+1)]
	}

	if (all(s < 0))
		ssel = !(si %in% abs(s))
	else
		ssel = si %in% s

	#if (all(t < 0))
	#	tsel = !(ti %in% abs(t))
	#else
	tsel = ti %in% t

	x@data = x@data[ssel & tsel, k, drop = FALSE]
	if (drop) {
		if (length(s) == 1 && all(s > 0)) { # space index has only 1 item:
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

# provide a na.omit-method for STFDF objects
# removes rows and columns from the space-time grid
# containing NAs in the data
# Tom Gottfried
na.omit.STFDF <- function(object, drop=TRUE, ...){
  data <- na.omit(object@data)
  omit <- attr(data, "na.action")
  n <- length(object@sp)
  s <- unique((omit-1) %% n + 1)
  t <- unique((omit-1) %/% n + 1)
  if (drop && (length(s)==n || length(t)==nrow(object@time)))
    return(NA)
  else
#    return(object[-s,-t, drop=drop])
              # <= negative indices are partly not handled by [-method
#    return(object[(1:n)[!(1:n) %in% s],
#                  (1:nrow(object@time))[!1:nrow(object@time) %in% t],
#                  drop=drop])
              # <= logical indices partly not handled by [-method
    return(object[(1:n)[!(1:n) %in% s],
                  (1:nrow(object@time))[!1:nrow(object@time) %in% t],
                  drop=drop])
}

setMethod("addAttrToGeom", signature(x = "STF", y = "data.frame"),
    function(x, y, match.ID, ...)
		new("STFDF", x, data = y)
)

length.STF = function(x) { prod(dim(x)[1:2]) }

length.STFDF = function(x) { prod(dim(x)[1:2]) }

setMethod("geometry", "STFDF", function(obj) as(obj, "STF"))
