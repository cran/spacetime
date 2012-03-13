setClass("ltraj", representation("list"))

setClass("STT",  # space-time trajectory/ies without data values
  representation("ST", traj = "list"),
  validity = function(object) {
    stopifnot(length(object@traj) > 0)
	stopifnot(length(object@sp) == 2)
	stopifnot(length(object@time) == 2)
	stopifnot(all(sapply(object@traj, class) == "STI"))
    return(TRUE)
  }
)

setClass("STTDF",  # space-time trajectory/ies with data values
  representation("STT", data = "data.frame"),
  validity = function(object) {
	stopifnot(sum(sapply(object@traj, length)) == nrow(object@data))
    return(TRUE)
  }
)

setAs("ltraj", "STTDF", 
	function(from) {
		d = do.call(rbind, from)
		ns = sapply(from, nrow)
		burst = sapply(from, function(x) attr(x, "burst"))
		id = sapply(from, function(x) attr(x, "id"))
		d$burst = factor(rep(burst, ns))
		d$id = factor(rep(id, ns))
		toSTI = function(x) {
			ret = STI(SpatialPoints(x[c("x", "y")]), x[["date"]])
			attr(ret, "burst") = attr(x, "burst")
			attr(ret, "id") = attr(x, "id")
			ret
		}
		STIbox = STI(SpatialPoints(cbind(range(d$x), range(d$y))), range(d$date))
		new("STTDF", new("STT", STIbox, traj = lapply(from, toSTI)), data = d)
	}
)
setAs("STTDF", "STIDF", 
	function(from) {
		sp = do.call(rbind, lapply(from@traj, function(x) x@sp))
		time = do.call(c, lapply(from@traj, index))
		STIDF(sp, time, from@data)
	}
)
setAs("STTDF", "ltraj", 
	function(from) {
		x = as(from, "STIDF")
		xy = coordinates(x@sp)
		da = index(x@time)
		as.ltraj(xy, da, id = x[["id"]], burst = x[["burst"]])
	}
)
setMethod("coordinates", "STT", function(obj) {
		do.call(rbind, lapply(obj@traj, coordinates))
	}
)

plot.STTDF = function(x, y,..., byBurst = TRUE, 
			col = "black", lty = 1, lwd = 1, 
			type = "l", pch = 1,
			add = FALSE) {
	if (add == FALSE)
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

setMethod("plot", signature(x = "STTDF", y = "missing"), plot.STTDF)

if (FALSE) {

rbind.STTDF = function(...) {
    dots = list(...)
    names(dots) <- NULL # bugfix Clement Calenge 100417
	p4s = proj4string(dots[[1]]@sp)
    df = do.call("rbind", lapply(dots, function(x) as(x, "data.frame")))
	sp = SpatialPoints(df[coordnames(dots[[1]]@sp)]) 
	proj4string(sp) = p4s
	new("STIDFtraj", STIDF(sp, df$time, df))
}
}
