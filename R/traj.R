setClass("ltraj", representation("list"))
setClass("STSDFtraj", representation("STSDF"),
  validity = function(object) {
    if (is.null(object$burst))
		stop("data.frame component should have a burst column")
    return(TRUE)
  }
)
setAs("ltraj", "STSDFtraj", 
	function(from) {
		d = do.call(rbind, from)
		n = unlist(lapply(from, nrow))
		# take burst to fill id:
		d$id = rep(unlist(t(sapply(from, attributes))[,4]), times = n)
		d$burst = rep(unlist(t(sapply(from, attributes))[,5]), times = n)
		new("STSDFtraj", STSDF(SpatialPoints(d[c("x","y")]), d$date, d))
	}
)
setAs("STSDFtraj", "ltraj", 
	function(from) {
		xy = coordinates(from@sp)
		da = index(from@time)
		as.ltraj(xy, da, id = from[["id"]], burst = from[["burst"]])
	}
)

plot.STSDFtraj = function(x,y,..., byBurst = TRUE, 
			line.col = "black", add=FALSE) {
	if (byBurst)
		f = x$burst
	else {
		stopifnot(!is.null(x$id))
		f = x$id
	}
	if (! add)
		plot(as(x@sp, "Spatial"), ...) # sets up plotting area
	lst = split(data.frame(coordinates(x)), f)
	line.col = rep(line.col, length = length(lst))
	for (i in seq(along = lst))
		lines(as.matrix(lst[[i]]), col = line.col[i], ...)
}
setMethod("plot", signature(x = "STSDFtraj", y = "missing"), plot.STSDFtraj)

rbind.STSDFtraj = function(...) {
    dots = list(...)
    names(dots) <- NULL # bugfix Clement Calenge 100417
	p4s = proj4string(dots[[1]]@sp)
    df = do.call("rbind", lapply(dots, function(x) as(x, "data.frame")))
	sp = SpatialPoints(df[coordnames(dots[[1]]@sp)]) 
	proj4string(sp) = p4s
	new("STSDFtraj", STSDF(sp, df$time, df))
}
