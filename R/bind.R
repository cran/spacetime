cbind.ST = function (..., deparse.level = 1) {
	args = list(...)
	# cbind data slots:
	df = do.call(cbind, lapply(args, function(x) x@data))
	ret = args[[1]]
	ret@data = df
	ret
}

rbind.STIDF = function(..., deparse.level = 1) {
	args = list(...)
	df = do.call(rbind, lapply(args, function(x) x@data))
	time = do.call(c, lapply(args, function(x) index(x@time)))
	sp = do.call(rbind, lapply(args, function(x) x@sp))
	STIDF(sp, time, df)
}

# pretty "lazy" implementations follow:

rbind.STSDF = function(..., deparse.level = 1) {
	args = list(...)
	as(do.call(rbind, lapply(args, function(x) as(x, "STIDF"))), "STSDF")
}

rbind.STFDF = function(..., deparse.level = 1) {
	args = list(...)
	n = names(args[[1]]@data)
	# as(do.call(rbind, lapply(args, function(x) as(x, "STIDF"))), "STFDF")
	sp = do.call(rbind, lapply(args, function(x) x@sp))
	args = lapply(args, function(x) as(x, "xts"))
	args = do.call(cbind, args)
	ret = stConstruct(args, sp)
	names(ret@data) = n
	ret
}