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

over.xts = function(x, y, returnList = FALSE, fn = NULL, ..., 
		timeInterval = TRUE) {
	ix = index(x)
	iy = index(y)
	if (returnList) { # get all matches:
		if (timeInterval)
			stop("timeInterval not supported when returnList = TRUE")
		lapply(1:nrow(x), function(x) { which(iy %in% ix[x]) })
	} else { # get first match:
		if (! timeInterval)
			ret = match(ix, iy)
		else {
			ret = findInterval(ix, iy)
			ret[ret == 0] = NA
		}
		ret
	}
}
setMethod("over", signature(x = "xts", y = "xts"), over.xts)

over.STF.STF = function(x, y, returnList = FALSE, fn = NULL, ...,
		timeInterval = TRUE) {
    space.index = over(x@sp, y@sp)
	time.index = rep(over(x@time, y@time, timeInterval = timeInterval), 
		each = length(space.index))
	# compute the index of x in y as y is STF:
    idx = (time.index - 1) * length(y@sp) + space.index # space.index gets recycled
	.index2list(idx, returnList)
}
setMethod("over", signature(x = "STF", y = "STF"), over.STF.STF)

over.STS.STF = function(x, y, returnList = FALSE, fn = NULL, ...,
		timeInterval = TRUE) {
    space.index = over(x@sp, y@sp)[x@index[,1]]
	time.index = over(x@time, y@time, timeInterval = timeInterval)[x@index[,2]]
	# compute the index of x in y as y is STF:
    idx = (time.index - 1) * length(y@sp) + space.index
	.index2list(idx, returnList)
}
setMethod("over", signature(x = "STS", y = "STF"), over.STS.STF)

over.STI.STF = function(x, y, returnList = FALSE, fn = NULL, ...,
		timeInterval = TRUE) {
    space.index = over(x@sp, y@sp)
	time.index = over(x@time, y@time, timeInterval = timeInterval)
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
	stopifnot(timeInterval == FALSE)
	time.index = over(x@time, y@time, returnList = TRUE, timeInterval = timeInterval)
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

over.ST.STS = function(x, y, returnList = FALSE, fn = NULL, ...,
		timeInterval = TRUE) {
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
    ret = sp:::.overDF(r, y@data, length(x), returnList, fn) 
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

aggregate.ST = function(x, by, FUN = mean, ...) {
    by0 = by
    if (gridded(by@sp))
        by@sp = as(by@sp, "SpatialPolygons")
    df = over(by, x, fn = FUN, ...)
    addAttrToGeom(by0, df, match.ID = FALSE)
}
