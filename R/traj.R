setClass("ltraj", representation("list"))
setClass("STIDFtraj", representation("STIDF"),
  validity = function(object) {
    if (is.null(object$burst))
		stop("data.frame component should have a burst column")
    return(TRUE)
  }
)
setAs("ltraj", "STIDFtraj", 
	function(from) {
		d = do.call(rbind, from)
		n = unlist(lapply(from, nrow))
		# take burst to fill id:
		d$id = rep(unlist(t(sapply(from, attributes))[,4]), times = n)
		d$burst = rep(unlist(t(sapply(from, attributes))[,5]), times = n)
		new("STIDFtraj", STIDF(SpatialPoints(d[c("x","y")]), d$date, d))
	}
)
setAs("STIDFtraj", "ltraj", 
	function(from) {
		xy = coordinates(from@sp)
		da = index(from@time)
		as.ltraj(xy, da, id = from[["id"]], burst = from[["burst"]])
	}
)

plot.STIDFtraj = function(x,y,..., byBurst = TRUE, 
			col = "black", lty = 1, lwd = 1, 
			type = "l", pch = 1,
			add = FALSE) {
	if (! add)
		plot(as(x@sp, "Spatial"), ...) # sets up plotting area
	if (byBurst)
		f = x$burst
	else {
		stopifnot(!is.null(x$id))
		f = x$id
	}
	lst = split(data.frame(coordinates(x)), f)
	col = rep(col, length = length(lst))
	lwd = rep(lwd, length = length(lst))
	lty = rep(lty, length = length(lst))
	for (i in seq(along = lst))
		lines(as.matrix(lst[[i]]), col = col[i], lty = lty[i], lwd = lwd[i],
			type = type, pch = pch)
}

setMethod("plot", signature(x = "STIDFtraj", y = "missing"), plot.STIDFtraj)

rbind.STIDFtraj = function(...) {
    dots = list(...)
    names(dots) <- NULL # bugfix Clement Calenge 100417
	p4s = proj4string(dots[[1]]@sp)
    df = do.call("rbind", lapply(dots, function(x) as(x, "data.frame")))
	sp = SpatialPoints(df[coordnames(dots[[1]]@sp)]) 
	proj4string(sp) = p4s
	new("STIDFtraj", STIDF(sp, df$time, df))
}
