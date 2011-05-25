if (!isGeneric("stplot"))
	setGeneric("stplot", function(obj, ...)
		standardGeneric("stplot"))

stplot.STFDF = function(obj, names.attr = as.character(index(obj@time)),
		..., as.table = TRUE, at, animate = 0, mode = "xy", scaleX = 0) {
    z = names(obj@data)[1]
	if (missing(at))
		at = seq(min(obj[[z]]), max(obj[[z]]), length.out = 16)
	if (mode == "xt") { # space-time cross section
		scales = list(...)$scales
		if (is.null(scales))
			scales = list(draw=TRUE)
		else
			scales$draw = TRUE
		s = sp:::longlat.scales(obj@sp, scales, xlim = bbox(obj@sp)[1,], ylim = bbox(obj@sp)[2,])
		cn = coordnames(obj@sp)
		if (scaleX == 1) {
			scales["x"] = s["x"]
			f = as.formula(paste(z, "~", cn[1], "+ time"))
		} else if (scaleX == 2) {
			scales["x"] = s["y"]
			f = as.formula(paste(z, "~", cn[2], "+ time"))
		} else
			f = as.formula(paste(z, "~ sp.ID + time"))
		return(levelplot(f, as.data.frame(obj), at = at, scales = scales, ...))
	}
    form = as.formula(paste(z, "~ time"))
    sp = obj@sp
    df = unstack(as.data.frame(obj), form)
	x = addAttrToGeom(sp, df, match.ID=FALSE)
	if (animate > 0) {
		i = 0
		while (TRUE) {
			i = i + 1
			if (i > ncol(df)) 
				i = 1
			print(spplot(x[,i], main = names.attr[i], at = at, ...))
			Sys.sleep(animate)
		}
	} else
		spplot(x, names.attr = names.attr, as.table = as.table, at = at,...)
}

stplot.STIDF = function(obj, names.attr = index(obj@time), ...)
	stplot(as(obj, "STFDF"), names.attr = names.attr, ...)

stplot.STIDF = function(obj, names.attr = NULL, ..., 
		as.table = TRUE, by = c("time", "burst", "id"), 
		scales = list(draw=FALSE), xlab = NULL, ylab = NULL, 
		type = 'p', number = 6, overlap = 0, asp) {
	f =  paste(rev(coordnames(obj@sp)), collapse="~")
	by = by[1]
	f = paste(f, "|", by)
	if (missing(asp))
		asp = mapasp(obj@sp)
	obj = as.data.frame(obj)
	if (is.numeric(number) && number > 1)
		obj$time = equal.count(obj$time, number = number, overlap = overlap)
	xyplot(as.formula(f), obj, asp = asp, type = type,
		as.table = as.table, scales = scales, xlab = xlab, ylab = ylab, 
		...)
}

setMethod("stplot", signature("STFDF"),  stplot.STFDF)

setMethod("stplot", signature("STSDF"), stplot.STIDF)

setMethod("stplot", signature("STIDF"), stplot.STIDF)

setMethod("stplot", signature("STIDFtraj"),
	function(obj, ..., names.attr = NULL, by = "burst", type = 'l')
		stplot.STIDF(obj, names.attr = names.attr, by = by, type = type, ...)
)
