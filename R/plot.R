plot.STF = function(x, y, ...) {
	ny = length(x@sp)
	yy = 1:ny
	xx = index(x@time)
	nx = length(xx)
	time = rep(xx, each = ny)
	space = rep(yy, nx)
	plot(time, space, ...)
}
setMethod("plot", signature(x = "STF", y = "missing"), plot.STF)

plot.STS = function(x, y, ...) {
	yy = 1:length(x@sp)
	xx = index(x@time)
	time = xx[x@index[,2]]
	space = yy[x@index[,1]]
	plot(time, space, ...)
}
setMethod("plot", signature(x = "STS", y = "missing"), plot.STS)

plot.STI = function(x, y, ...) {
	space = 1:length(x@sp)
	time = index(x@time)
	plot(x = time, y = space, ...)
}
setMethod("plot", signature(x = "STI", y = "missing"), plot.STI)

#plot.STL = function(x, y, ..., type = 'l', labels = FALSE) {
#	y = 1:length(x@sp)
#	x1 = index(x@time)
#	x2 = x@endTime
#	space = rep(as.numeric(NA), 3*length(y))
#	space[(1:length(y))*3-2] = y
#	space[(1:length(y))*3-1] = y
#	time = rep(x1[1], 3*length(y))
#	time[(1:length(y))*3-2] = x1
#	time[(1:length(y))*3-1] = x2
#	time[(1:length(y))*3] = NA
#	plot(x = time, y = space, type = type, ...)
#	if (labels)
#		text(x2, y, pos = 4, row.names(x@sp))
#}
#setMethod("plot", signature(x = "STL", y = "missing"), plot.STL)

plot.STT = function(x, y, ..., type = 'l', col = 1:length(x@traj), labels = FALSE) {
	nt = sapply(x@traj, function(x) dim(x)[1])
	yy0 = 1:sum(nt)
	grp = rep(1:length(nt), times = nt)
	space = do.call(c, lapply(split(yy0, grp), function(x) c(x, NA)))
	time = do.call(c, lapply(x@traj, function(x) c(index(x), NA)))
	plot(x = time, y = space, ..., type = type, col = col)
	# labels(x2, y2, pos=4, sapply(x@traj, function(x) attr(x, "burst)))
}
setMethod("plot", signature(x = "STT", y = "missing"), plot.STT)

lines.STTDF = function(x, y = NULL, ...) {
	xy = coordinates(as(x, "STIDF"))
	lines(xy, ...)
}
