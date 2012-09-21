# TODO: use CRAN package interval

augment.with.one = function(x) {
	ux = unique(x)
	l = length(ux)
	if (l <= 1)
		stop("cannot derive time interval from length 1 or constant sequence")
	last = ux[l]
	dt = last - ux[l-1]
	c(x, last + dt)
}
delta = function(x) {
	if (is(x, "xts"))
		x = index(x)
	augment.with.one(as.POSIXct(x))[-1]
}

timeMatch = function(x, y, returnList = FALSE, end.x = NULL, end.y = NULL) {
	if (!(is.null(end.x) && is.null(end.y))) # one of them non-NULL:
		return(timeMatchIntervals(x, y, returnList, end.x, end.y))
	ti.x = !is.null(end.x) && any(end.x > x) #timeIsInterval(x)
	ti.y = !is.null(end.y) && any(end.y > y) #timeIsInterval(y)
	stopifnot(!(is.na(ti.x) || is.na(ti.y)))
	if (is(x, "xts"))
		x = index(x)
	if (ti.x)
		x1 = augment.with.one(x) 
	if (is(y, "xts"))
		y = index(y)
	if (ti.y)
		y1 = augment.with.one(y) 
	x = as.POSIXct(x)
	y = as.POSIXct(y)
	if (!identical(class(x), class(y)))
		warning("timeMatch: time indices of x and y are of different class")
	# now go through T/F for { returnList, ti.x, ti.y }:
	if (returnList) { # get all matches:
		if ("intervals" %in% .packages() && (ti.x || ti.y))
			return(timeMatchIntervals(x, y, TRUE, 
				ifelse(ti.x, x1, NULL), ifelse(ti.y, y1, NULL)))
		# else: do it ourselves...
		if (ti.x == FALSE && ti.y == FALSE)
			# DOUBLE LOOP:
			return(lapply(x, function(X) which(X == y)))
		if (ti.x == TRUE && ti.y == FALSE) {
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
			####
			# DOUBLE LOOP:
			ret = vector("list", length(x))
			for (i in 1:length(x))
				ret[[i]] = which(x1[i] < y1[-1] & y < x1[i+1])
				#ret[[i]] = which(!(x1[i] >= y1[-1] | x1[i+1] <= y))
			return(ret)
			#FI3 = function(n, i) {
			#	ret = lapply(1:n, function(x) integer(0))
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
			ret = findInterval(y, x1) # before first: 0; after last: l
			l = length(x1)
			ret[ret == 0 | ret == l] = NA
			return(sapply(1:length(x), function(P) which(ret == P)[1]))
		}
		if (ti.x == FALSE && ti.y == TRUE) {
			ret = findInterval(x, y1) # before first: 0; after last: l
			l = length(y1)
			ret[ret == 0 | ret == l] = NA
			return(ret)
		}
		if (ti.x == TRUE && ti.y == TRUE) {
			ret = rep(as.integer(NA), length(x))
			# DOUBLE LOOP:
			#for (i in 1:length(x))
			#	ret[i] = which(!(x1[i] >= y1[-1] | x1[i+1] <= y))[1], can be written as:
			for (i in 1:length(x))
				ret[i] = which(x1[i] < y1[-1] & y < x1[i+1])[1]
			return(ret)
		}
		stop("impossible to reach this point")
	}
}

timeMatchIntervals = function(x, y, returnList = FALSE, 
		end.x = NULL, end.y = NULL) {
	stopifnot(!(is.null(end.x) && is.null(end.y))) # one of them should be non-null!
	if ("intervals" %in% .packages()) {
		if (!is.null(end.x))
			x = Intervals(cbind(x, end.x), closed = c(TRUE, FALSE))
		else
			x = as.numeric(x)
		if (!is.null(end.y))
			y = Intervals(cbind(y, end.y), closed = c(TRUE, FALSE))
		else
			y = as.numeric(y)
		ret = interval_overlap(x, y)
	} else {
		if (is.null(end.x))
			end.x = x
		if (is.null(end.y))
			end.y = y
		stopifnot(all(end.x >= x & end.y >= y)) # sanity check
		ret = vector("list", length(x))
		for (i in seq(along = ret)) # DOUBLE LOOP:
			ret[[i]] = which(x[i] < end.y & end.x[i] > y)
	}
	if (!returnList)
		ret = sapply(ret, function(x) x[1])
	ret
}
