stConstruct = function(x, space, time, SpatialObj = NULL, 
		TimeObj = NULL, crs = CRS(as.character(NA))) {
	if (is(x, "matrix"))
		x = data.frame(x)
	stopifnot(is(x, "data.frame"))
	#stopifnot(isIndex(space) && isIndex(time)) 
	if (length(space) > 1 && length(time) == 1) { 
		# long format; space indicates columns with points, coordinates
			n = names(x)
		if (is.character(space))
			si = which(n %in% space)
		else
			si = space
		sp = SpatialPoints(x[si], crs)
		stopifnot(length(time) == 1)
		if (is.character(time))
			ti = which(n %in% time)
		else
			ti = time
		time = xts(1:nrow(x), x[[ti]])
		x = x[-c(si, ti)]
		return(STIDF(sp, time, x))
	} else if (length(space) == 1 && length(time) == 1) {
		# long format, space indicates index of SpatialObj:
		stopifnot(!is.null(SpatialObj))
		# check every combination is there:
		if (any(table(x[,space], x[,time]) != 1)) { # NOT a full grid:
			# stop("space/time combinations not complete")
			# why not try Irregular?
			return(STIDF(SpatialObj, xts(1:nrow(x), x[,time]), x))
		} else {
			sut = sort(unique(x[,time]))
			t = xts(1:length(sut), sut)
			sp = as.character(sort(unique(x[,space])))
			return(STFDF(SpatialObj[sp], t, 
				x[order(x[,time],as.character(x[,space])),]))
		}
	} else if (is.list(time)) {  
		# time-wide table; space coords or SpatialObj
		isIndex = function(x) is.character(x) || is.numeric(x)
		if (isIndex(space) && length(space) > 1)
			SpatialObj = SpatialPoints(x[,space], CRS)
		else if (missing(SpatialObj))
			SpatialObj = space
		xx = data.frame(lapply(time, function(g) stack(x[g])$values))
		return(STFDF(SpatialObj, TimeObj, xx))
	} else if (is.list(space)) { 
		# space-wide table:
		xx = data.frame(lapply(space, function(g) as.vector(t(x[g]))))
		return(STFDF(SpatialObj, time, 
			data.frame(values = as.vector(t(xx)))))
	} 
	stop("unknown parameter combination")
}
