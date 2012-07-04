# from sp:
.index2list = function(x, returnList) {
	if (returnList) {
		l = lapply(1:length(x), function(x) { integer(0) })
		notNA = !is.na(x)
		l[notNA] = x[notNA]
		l
	} else
		x
}

augment.with.one = function(x) {
	ux = unique(x)
	l = length(ux)
	if (l <= 1)
		stop("cannot derive time interval from length 1 or constant sequence")
	last = ux[l]
	dt = last - ux[l-1]
	c(x, last + dt)
}

timeMatch = function(x, y, returnList = FALSE) {
	ti.x = timeIsInterval(x)
	ti.y = timeIsInterval(y)
	stopifnot(!(is.na(ti.x) || is.na(ti.y)))
	if (is(x, "xts"))
		x = index(x)
	if (is(y, "xts"))
		y = index(y)
	if (!identical(class(x), class(y)))
		warning("timeMatch: time indices of x and y are of different class")
	# now go through T/F for { returnList, ti.x, ti.y }:
	if (returnList) { # get all matches:
		if (ti.x == FALSE && ti.y == FALSE)
			# DOUBLE LOOP:
			return(lapply(x, function(X) which(X == y)))
		if (ti.x == TRUE && ti.y == FALSE) {
			x1 = augment.with.one(x) 
			# DOUBLE LOOP:
			#lst = lapply(y, function(Y) which(!(Y >= x1[-1] | Y < x)))
			#return(sp:::.invert(lst, length(lst), length(x)))
			FI1 = function(n, i) { 
				ret = lapply(1:n, function(x) integer(0))
				for (j in 1:length(i)) {
					ix = i[j]
					if (ix > 0 && ix <= n)
						ret[[ ix ]] = c(ret[[ix]], j)
				}
				ret
			}
			return(FI1(length(x), findInterval(y, x1)))
		}
		if (ti.x == FALSE && ti.y == TRUE) {
			# when more than one time step is available
			# add a time stamp to close the last (open) interval
			y1 = augment.with.one(y) 
			# DOUBLE LOOP:
			#return(lapply(x, function(X) which(!(X >= y1[-1] | X < y))))
			FI2 = function(n, i) {
				lapply(i, function(x) { 
					if (x == 0 || x > n) 
						numeric(0)
					else
						x # as x is NOT interval, there can only be one y element matching
				})
			}
			return(FI2(length(y), findInterval(x, y1)))
		}
		if (ti.x == TRUE && ti.y == TRUE) {
			x1 = augment.with.one(x) 
			y1 = augment.with.one(y) 
			####
			# DOUBLE LOOP:
			ret = vector("list", length(x))
			for (i in 1:length(x))
				ret[[i]] = which(x1[i] < y1[-1] & y < x1[i+1])
				#ret[[i]] = which(!(x1[i] >= y1[-1] | x1[i+1] <= y))
			return(ret)
			#FI3 = function(n, i) {
			#	ret = lapply(1:n, function(x) integer(0))
    		#	#for (j in (length(i)-1):1) {
    		#	for (j in 1:(length(i)-1)) {
			#		i1 = i[j]
			#		i2 = i[j+1]
			#		if (i2 > 0 && i1 <= n) {
			#			r = max(1, i1) : min(n, i2)
			#			for (k in r)
			#				ret[[ k ]] = c(ret[[ k ]], j)
			#		}
			#	}
			#	ret
			#}
			#return(FI3(length(x), findInterval(y1, x1, rightmost.closed = FALSE)))
		}
	} else { # get first match:
		if (ti.x == FALSE && ti.y == FALSE)
			return(match(x, y))
		if (ti.x == TRUE && ti.y == FALSE) {
			x1 = augment.with.one(x) 
			ret = findInterval(y, x1) # before first: 0; after last: l
			l = length(x1)
			ret[ret == 0 | ret == l] = NA
			return(sapply(1:length(x), function(P) which(ret == P)[1]))
		}
		if (ti.x == FALSE && ti.y == TRUE) {
			# when more than one time step is available
			# add a time stamp to close the last (open) interval
			y = augment.with.one(y) 
			ret = findInterval(x, y) # before first: 0; after last: l
			l = length(y)
			ret[ret == 0 | ret == l] = NA
			return(ret)
		}
		if (ti.x == TRUE && ti.y == TRUE) {
			x1 = augment.with.one(x) 
			y1 = augment.with.one(y) 
			ret = rep(as.integer(NA), length(x))
			# DOUBLE LOOP:
			#for (i in 1:length(x))
			#	ret[i] = which(!(x1[i] >= y1[-1] | x1[i+1] <= y))[1]
			for (i in 1:length(x))
				ret[i] = which(x1[i] < y1[-1] & y < x1[i+1])[1]
			return(ret)
		}
		stop("impossible to reach this point")
	}
}

over.xts = function(x, y, returnList = FALSE, fn = NULL, ...) {
	ix = index(x)
	iy = index(y)
	timeIsInterval(ix) = timeIsInterval(x)
	timeIsInterval(iy) = timeIsInterval(y)
	if (returnList) { # get all matches:
		tm = timeMatch(ix, iy, returnList = TRUE)
		stopifnot(is.null(fn))
		lapply(tm, function(P) { y[P, drop=FALSE] })
	} else { 
		tm = timeMatch(ix, iy, returnList = FALSE)
		if (is.null(fn)) # get first match:
			y[tm,]
		else {
			l = lapply(tm, function(P) { apply(y[P, drop=FALSE], 2, fn, ...) })
			ret = do.call(rbind, l)
			xts(ret, ix)
		}
	}
}
setMethod("over", signature(x = "xts", y = "xts"), over.xts)

# y = STF:
over.STF.STF = function(x, y, returnList = FALSE, fn = NULL, ...) {
	if (returnList) {
    	space.index = over(x@sp, y@sp, returnList = TRUE)
		time.index = timeMatch(x@time, y@time, returnList = TRUE) 
		n = length(y@sp)
		lst = vector("list", length(space.index) * length(time.index))
		k = 1
		for (i in seq(along = time.index)) {
			for (j in seq(along = space.index)) {
				nj = length(space.index[[j]])
				if (length(time.index[[i]]) == 0 || 
						length(space.index[[j]]) == 0)
					lst[[k]] = integer(0)
				else
					lst[[k]] = rep((time.index[[i]] - 1) * n, each = nj) +
						space.index[[j]]
				k = k + 1
			}
		}
		lst
	} else {
    	space.index = over(x@sp, y@sp)
		time.index = rep(timeMatch(x@time, y@time), each = length(space.index))
		# compute the index of x in y as y is STF:
    	(time.index - 1) * length(y@sp) + space.index # space.index gets recycled
	}
}
setMethod("over", signature(x = "STF", y = "STF"), over.STF.STF)

over.STS.STF = function(x, y, returnList = FALSE, fn = NULL, ...) {
	if (returnList) warning("returnList not supported yet")
    space.index = over(x@sp, y@sp)[x@index[,1]]
	time.index = timeMatch(x@time, y@time)[x@index[,2]]
	# compute the index of x in y as y is STF:
    idx = (time.index - 1) * length(y@sp) + space.index
	.index2list(idx, returnList)
}
setMethod("over", signature(x = "STS", y = "STF"), over.STS.STF)

over.STI.STF = function(x, y, returnList = FALSE, fn = NULL, ...) {
	#if (returnList) warning("returnList not supported yet")
    space.index = over(x@sp, y@sp)
	time.index = timeMatch(x@time, y@time, returnList)
	# compute the index of x in y as y is STF:
    idx = (unlist(time.index) - 1) * length(y@sp) + unlist(space.index)
	.index2list(idx, returnList)
}
setMethod("over", signature(x = "STI", y = "STF"), over.STI.STF)

# y = STI:
over.STF.STI = function(x, y, returnList = FALSE, fn = NULL, ...)
	over(as(x, "STS"), y, returnList = returnList, fn=fn, ...)
setMethod("over", signature(x = "STF", y = "STI"), over.STF.STI)

over.STS.STI = function(x, y, returnList = FALSE, fn = NULL, ...)
	over(as(x, "STI"), y, returnList = returnList, fn=fn, ...)
setMethod("over", signature(x = "STS", y = "STI"), over.STS.STI)

over.STI.STI = function(x, y, returnList = FALSE, fn = NULL, ...) {
	if (returnList) warning("returnList not fully supported yet")
	time.index = timeMatch(x@time, y@time, returnList = TRUE)
	ret = lapply(1:length(time.index), function(i) {
		ti = time.index[[i]] # the x[i] matching y entry indices
		if (length(ti) > 0)
			over(x@sp[i,], y@sp[ti,], returnList = TRUE)[[1]] + (ti - 1)
		else
			integer(0)
	})
	if (returnList)
		ret
	else
		unlist(lapply(ret, function(x) { x[1] }))
}
setMethod("over", signature(x = "STI", y = "STI"), over.STI.STI)

# y = STS:
over.ST.STS = function(x, y, returnList = FALSE, fn = NULL, ...) {
	if (returnList) warning("returnList not fully supported yet")
	ret = over(x, STF(y@sp, y@time), returnList = returnList, fn = fn)
	ix.sts = (y@index[,2] - 1) * length(y@sp) + y@index[,1]
	ix.stf = rep(as.integer(NA), nrow(y@time) * length(y@sp))
	ix.stf[ix.sts] = 1:nrow(y@index)
	ix.stf[ret]
}
setMethod("over", signature(x = "ST", y = "STS"), over.ST.STS)

overDFGenericST = function(x, y, returnList = FALSE, fn = NULL, ...) {
    stopifnot(identical(proj4string(x),proj4string(y)))
	if (is.null(fn) && !returnList) {
    	r = over(x, geometry(y), returnList = FALSE)
		ret = y@data[r,,drop=FALSE]
	} else {
    	r = over(x, geometry(y), returnList = TRUE)
    	ret = sp:::.overDF(r, y@data, length(x), returnList, fn, ...)
	}
    if (!returnList)
        row.names(ret) = row.names(x)
    ret
}
setMethod("over", signature(x = "STF", y = "STFDF"), overDFGenericST)
setMethod("over", signature(x = "STS", y = "STFDF"), overDFGenericST)
setMethod("over", signature(x = "STI", y = "STFDF"), overDFGenericST)
setMethod("over", signature(x = "STF", y = "STSDF"), overDFGenericST)
setMethod("over", signature(x = "STS", y = "STSDF"), overDFGenericST)
setMethod("over", signature(x = "STI", y = "STSDF"), overDFGenericST)
setMethod("over", signature(x = "STF", y = "STIDF"), overDFGenericST)
setMethod("over", signature(x = "STS", y = "STIDF"), overDFGenericST)
setMethod("over", signature(x = "STI", y = "STIDF"), overDFGenericST)

aggregate_ST_temporal = function(x, by, FUN = mean, ..., simplify = TRUE) {
	stopifnot("data" %in% slotNames(x))
	x = as(x, "STFDF")
	if (is.function(by))
		cc = by(index(x@time)) # time format index
	else if (is(by, "character")) { 
		ix = index(x@time)
		stopifnot(is(ix, c("Date", "POSIXt")))
		cc = cut(ix, by)
		if (is(ix, "Date"))
			cc = as.Date(cc)
		if (is(ix, "POSIXt"))
			cc = as.POSIXct(cc, tz = format(ix[1], "%Z"))
	}
	d = vector("list", length = ncol(x@data))
	for (i in 1:length(d)) {
		# use aggregate.zoo, returns zoo object:
		agg = aggregate(as.zoo(as(x[,,i], "xts")), cc, FUN = FUN, ...)
		d[[i]] = as.vector(t(agg))
	}
	names(d) = names(x@data)
	d = as.data.frame(d)
	if (simplify && length(time(agg)) == 1) {
		if ("data" %in% slotNames(x@sp))
			d = data.frame(x@sp@data, d)
   		addAttrToGeom(geometry(x@sp), d, match.ID = FALSE)
	} else
		STFDF(x@sp, time(agg), d)
}

setMethod("aggregateBy", signature(x = "ST", by = "function"), aggregate_ST_temporal)
setMethod("aggregateBy", signature(x = "ST", by = "character"), aggregate_ST_temporal)

setMethod("aggregateBy", signature(x = "ST", by = "Spatial"),
	function(x, by, FUN = mean, ..., simplify = TRUE) {
		stopifnot("data" %in% slotNames(x))
	# aggregate over space areas, keep time:
		x = as(x, "STFDF")
		ix = over(x@sp, geometry(by))
		sel = !is.na(ix)
		d = vector("list", length = ncol(x@data))
		for (i in 1:length(d)) {
			# use aggregate.zoo, returns zoo object:
			agg = aggregate(t(as(x[sel,,i], "xts")), list(ix[sel]), 
				FUN = FUN, ...)
			g = agg$Group.1 # first column
			d[[i]] = as.vector(as.matrix(agg[,-1])) # attributes, time-wide
		}
		names(d) = names(x@data)
		d = as.data.frame(d)
		if (simplify && length(by[g,]) == 1)
			xts(cbind(d, as.matrix(x@time)), index(x@time))
		else
			STFDF(by[g,], x@time, d)
	}
)

setMethod("aggregateBy", signature(x = "ST", by = "ST"),
	function(x, by, FUN = mean, ..., simplify = TRUE) {
		stopifnot("data" %in% slotNames(x))
    	by0 = by
    	if (gridded(by@sp))
       		by@sp = as(by@sp, "SpatialPolygons")
    	df = over(by, x, fn = FUN, ...)
		if (simplify && length(by@sp) == 1) # return xts:
			xts(cbind(df, as.matrix(by@time)), index(by@time))
		else if (simplify && nrow(by@time) == 1) { # return spatial:
			if ("data" %in% slotNames(by0@sp))
				df = data.frame(df, by0@sp@data)
    		addAttrToGeom(geometry(by0@sp), df, match.ID = FALSE)
		} else { #  by0 is STX:
			if ("data" %in% slotNames(by0))
				df = data.frame(df, by0@data)
    		addAttrToGeom(by0, df, match.ID = FALSE)
		}
	}
)

setMethod("aggregate", signature(x = "ST"),
	function(x, by, FUN = mean, ..., simplify = TRUE) # dispatch on "by" as well:
		aggregateBy(x, by, FUN = FUN, simplify = simplify, ...)
)
