if (!isGeneric("stplot"))
	setGeneric("stplot", function(obj, ...)
		standardGeneric("stplot"))

stplot.STFDF = function(obj, names.attr = as.character(index(obj@time)),
		..., as.table = TRUE, at, cuts = 15, 
		animate = 0, mode = "xy", scaleX = 0, 
		auto.key = TRUE, key.space = "right") {
	ind = sp.ID = NULL # keep R CMD check happy in R 2.13 
    z = names(obj@data)[1]
	if (missing(at))
		at = seq(min(obj[[z]], na.rm = TRUE), max(obj[[z]], na.rm = TRUE), 
			length.out = cuts + 1)
	if (mode == "ts") { # multiple time series
		xyplot(as.formula(paste(z, "~", "time")), 
				as.data.frame(obj), groups = sp.ID, 
				type = 'l',auto.key = auto.key, as.table = as.table, ...)
	} else if (mode == "tp") { # time series in multiple panels
    	if (ncol(obj@data) == 1)
			xyplot(as.formula(paste(z, "~ time | sp.ID")), 
				as.data.frame(obj), type = 'l', auto.key = auto.key, 
				key.space = key.space, as.table = as.table, ...)
		else {
			n = names(obj@data)
			df = as.data.frame(obj)
			st = stack(df, n) # values ~ ind
			st$time = df$time
			st$sp.ID = df$sp.ID
			xyplot(as.formula(paste("values ~ time | sp.ID")), 
				st, type = 'l', groups = ind,
				key.space = key.space, auto.key = auto.key, 
				as.table = as.table, ...)
		}
	} else if (mode == "xt") { # space-time cross section
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
		levelplot(f, as.data.frame(obj), at = at, scales = scales, 
			cuts = cuts, as.table = as.table, ...)
	} else {
    	form = as.formula(paste(z, "~ time"))
    	sp = geometry(obj@sp)
    	df = data.frame(unstack(as.data.frame(obj), form))
		x = addAttrToGeom(sp, df, match.ID=FALSE)
		if (animate > 0) {
			i = 0
			while (TRUE) {
				i = i + 1
				if (i > ncol(df)) 
					i = 1
				print(spplot(x[,i], main = names.attr[i], at = at, 
					as.table = as.table, auto.key = auto.key, 
					key.space = key.space, ...))
				Sys.sleep(animate)
			}
		} else
			spplot(x, names.attr = names.attr, as.table = as.table, at = at,
				cuts = cuts, auto.key = auto.key, key.space = key.space, ...)
	}
}

#stplot.STIDF = function(obj, names.attr = index(obj@time), ...)
#	stplot(as(obj, "STFDF"), names.attr = names.attr, ...)

stplot.STIDF = function(obj, names.attr = NULL, ..., 
		as.table = TRUE, by = c("time", "burst", "id"), 
		scales = list(draw=FALSE), xlab = NULL, ylab = NULL, 
		type = 'p', number = 6, overlap = 0, asp) {
		#, panel = panel.xyplot, sp.layout = NULL) {
	f =  paste(rev(coordnames(obj@sp)), collapse=" ~ ")
	by = by[1]
	f = paste(f, "|", by)
	if (missing(asp))
		asp = mapasp(obj@sp)
	obj = as.data.frame(obj)
	if (is.numeric(number) && number > 1)
		obj$time = equal.count(obj$time, number = number, overlap = overlap)
	xyplot(as.formula(f), obj, asp = asp, type = type,
		as.table = as.table, scales = scales, xlab = xlab, ylab = ylab, ...)
}

setMethod("stplot", signature("STFDF"),  stplot.STFDF)

setMethod("stplot", signature("STSDF"), stplot.STIDF)

setMethod("stplot", signature("STIDF"), stplot.STIDF)

setMethod("stplot", signature("STIDFtraj"),
	function(obj, ..., names.attr = NULL, by = "burst", type = 'l')
		stplot.STIDF(obj, names.attr = names.attr, by = by, type = type, ...)
)
