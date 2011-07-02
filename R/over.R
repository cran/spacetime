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
	if (l == 1) # there's only one (unique) time stamp ...
		x
	else { 
		last = ux[l]
		dt = last - ux[l-1]
		c(x, last + dt)
	}
}

.timeMatch = function(x, y, returnList = FALSE, timeInterval = TRUE) {
	if (!identical(class(x), class(y)))
		warning(".timeMatch: time indices of x and y are of different class")
	if (returnList) { # get all matches:
		if (timeInterval) {
			#stop("timeInterval not supported when returnList = TRUE")
			x = augment.with.one(x) 
			if (length(x) == 1)
				list(which(y >= x[1]))
			else 
				lapply(1:(length(x)-1), 
					function(P) which(x[P] <= y & y < x[P+1]))
		} else
			lapply(1:length(x), function(P) { which(y %in% x[P]) })
	} else { # get first match:
		if (! timeInterval)
			ret = match(x, y)
		else {
			# add a time stamp to close open interval, 
			# if more than one time step is available
			y = augment.with.one(y) 
			l = length(y)
			ret = findInterval(x, y) # before first: 0; after last: l
			ret[ret == 0 | ifelse(l > 1, ret == l, FALSE)] = NA
		}
		ret
	}
}

over.xts = function(x, y, returnList = FALSE, fn = NULL, ..., 
		timeInterval = TRUE) {
	tm = .timeMatch(index(x), index(y), returnList = TRUE, timeInterval)
	if (returnList) { # get all matches:
		stopifnot(is.null(fn))
		lapply(tm, function(P) { y[P, drop=FALSE] })
	} else { 
		if (is.null(fn)) # get first match:
			fn = function(x, ...) { x[1, drop=FALSE] }
		l = lapply(tm, function(P) { apply(y[P, drop=FALSE], 2, fn, ...) })
		ret = do.call(rbind, l)
		xts(ret, index(x))
	}
}
setMethod("over", signature(x = "xts", y = "xts"), over.xts)

# y = STF:
over.STF.STF = function(x, y, returnList = FALSE, fn = NULL, ...,
		timeInterval = TRUE) {
	if (returnList) {
    	space.index = over(x@sp, y@sp, returnList = TRUE)
		time.index = .timeMatch(index(x@time), index(y@time), returnList = TRUE,
			timeInterval = timeInterval) 
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
		time.index = rep(.timeMatch(index(x@time), index(y@time),
			timeInterval = timeInterval), each = length(space.index))
		# compute the index of x in y as y is STF:
    	(time.index - 1) * length(y@sp) + space.index # space.index gets recycled
	}
}
setMethod("over", signature(x = "STF", y = "STF"), over.STF.STF)

over.STS.STF = function(x, y, returnList = FALSE, fn = NULL, ...,
		timeInterval = TRUE) {
	if (returnList) warning("returnList not fully supported yet")
    space.index = over(x@sp, y@sp)[x@index[,1]]
	time.index = .timeMatch(index(x@time), index(y@time), 
		timeInterval = timeInterval)[x@index[,2]]
	# compute the index of x in y as y is STF:
    idx = (time.index - 1) * length(y@sp) + space.index
	.index2list(idx, returnList)
}
setMethod("over", signature(x = "STS", y = "STF"), over.STS.STF)

over.STI.STF = function(x, y, returnList = FALSE, fn = NULL, ...,
		timeInterval = TRUE) {
	if (returnList) warning("returnList not fully supported yet")
    space.index = over(x@sp, y@sp)
	time.index = .timeMatch(index(x@time), index(y@time), 
		timeInterval = timeInterval)
	# compute the index of x in y as y is STF:
    idx = (time.index - 1) * length(y@sp) + space.index
	.index2list(idx, returnList)
}
setMethod("over", signature(x = "STI", y = "STF"), over.STI.STF)

# y = STI:
over.STF.STI = function(x, y, returnList = FALSE, fn = NULL, ...,
		timeInterval = FALSE)
	over(as(x, "STS"), y, returnList = returnList, fn=fn,
		timeInterval = timeInterval, ...)
setMethod("over", signature(x = "STF", y = "STI"), over.STF.STI)

over.STS.STI = function(x, y, returnList = FALSE, fn = NULL, ...,
		timeInterval = FALSE)
	over(as(x, "STI"), y, returnList = returnList, fn=fn,
		timeInterval = timeInterval, ...)
setMethod("over", signature(x = "STS", y = "STI"), over.STS.STI)

over.STI.STI = function(x, y, returnList = FALSE, fn = NULL, ...,
		timeInterval = FALSE) {
	if (returnList) warning("returnList not fully supported yet")
	time.index = .timeMatch(index(x@time), index(y@time), 
		returnList = TRUE, timeInterval = timeInterval)
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
over.ST.STS = function(x, y, returnList = FALSE, fn = NULL, ...,
		timeInterval = TRUE) {
	if (returnList) warning("returnList not fully supported yet")
	ret = over(x, STF(y@sp, y@time), returnList = returnList, fn = fn,
		timeInterval = timeInterval)
	ix.sts = (y@index[,2] - 1) * length(y@sp) + y@index[,1]
	ix.stf = rep(as.integer(NA), nrow(y@time) * length(y@sp))
	ix.stf[ix.sts] = 1:nrow(y@index)
	ix.stf[ret]
}
setMethod("over", signature(x = "ST", y = "STS"), over.ST.STS)

overDFGenericST = function(x, y, returnList = FALSE, fn = NULL, ...,
		timeInterval = TRUE) {
    stopifnot(identical(proj4string(x),proj4string(y)))
    r = over(x, geometry(y), returnList = TRUE, timeInterval = timeInterval)
    ret = sp:::.overDF(r, y@data, length(x), returnList, fn, ...)
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
setMethod("over", signature(x = "STF", y = "STIDF"), 
	function(x, y, returnList = FALSE, fn = NULL, ..., timeInterval = FALSE)
			overDFGenericST(x,y,returnList,fn,timeInterval=timeInterval,...))
setMethod("over", signature(x = "STS", y = "STIDF"), 
	function(x, y, returnList = FALSE, fn = NULL, ..., timeInterval = FALSE)
			overDFGenericST(x,y,returnList,fn,timeInterval=timeInterval,...))
setMethod("over", signature(x = "STI", y = "STIDF"), 
	function(x, y, returnList = FALSE, fn = NULL, ..., timeInterval = FALSE)
			overDFGenericST(x,y,returnList,fn,timeInterval=timeInterval,...))

aggregate.ST = function(x, by, FUN = mean, ..., simplify = TRUE) {
	stopifnot("data" %in% slotNames(x))
	if (is.function(by) || is.character(by)) { # temporal aggregation:
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
		if (simplify && length(time(agg)) == 1)
    		addAttrToGeom(x@sp, as.data.frame(d), match.ID = FALSE)
		else
			STFDF(x@sp, time(agg), as.data.frame(d))
	} else if (is(by, "Spatial")) { 
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
		if (simplify && length(by[g,]) == 1)
			xts(as.data.frame(d), index(x@time))
		else
			STFDF(by[g,], x@time, as.data.frame(d))
	} else {
		stopifnot(is(by, "ST"))
    	by0 = by
    	if (gridded(by@sp))
       		by@sp = as(by@sp, "SpatialPolygons")
    	df = over(by, x, fn = FUN, ...)
		if (simplify && length(by@sp) == 1) # return xts:
			xts(df, index(by@time))
		else if (simplify && nrow(by@time) == 1) # return spatial:
    		addAttrToGeom(by0@sp, df, match.ID = FALSE)
		else
    		addAttrToGeom(by0, df, match.ID = FALSE)
	}
}
