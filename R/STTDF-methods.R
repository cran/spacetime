setClass("ltraj", representation("list"))

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
setAs("STT", "STI", 
	function(from) {
		sp = do.call(rbind, lapply(from@traj, function(x) x@sp))
		time = do.call(c, lapply(from@traj, index))
		o = order(time)
		new("STI", (ST(sp[o,,drop=FALSE],time[o,]))) # reorders!
	}
)
setAs("STTDF", "STIDF", 
	function(from) {
		sp = do.call(rbind, lapply(from@traj, function(x) x@sp))
		time = do.call(c, lapply(from@traj, index))
		STIDF(sp, time, from@data)
	}
)
setAs("STIDF", "STTDF", 
	function(from) {
		traj = lapply(split(from, from$burst), function(x) as(x, "STI"))
		STIbox = STI(SpatialPoints(cbind(range(from$x), range(from$y)), 
				from@sp@proj4string), range(from$date))
		new("STTDF", new("STT", STIbox, traj = traj), data = from@data)
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
