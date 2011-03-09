if (!isGeneric("stplot"))
	setGeneric("stplot", function(obj, ...)
		standardGeneric("stplot"))

stplot.STFDF = function(obj, names.attr = index(obj@time), 
		..., as.table = TRUE) {
    form = as.formula(paste(names(obj@data)[1], "~time"))
    sp = obj@sp
    df = unstack(as.data.frame(obj), form)
	x = addAttrToGeom(sp, df, match.ID=FALSE)
    spplot(x, names.attr = names.attr, as.table = as.table, ...)
}

stplot.STIDF = function(obj, names.attr = index(obj@time), ...)
	stplot(as(obj, "STFDF"), names.attr = names.attr, ...)

stplot.STIDF = function(obj, names.attr = NULL, ..., 
		as.table = TRUE, by = c("time", "burst", "id"), 
		scales = list(draw=FALSE), xlab = NULL, ylab = NULL, 
		type = type, number = 6, overlap = 0, asp) {
	f =  paste(rev(coordnames(obj@sp)), collapse="~")
	by = by[1]
	f = paste(f, "|", by)
	if (missing(asp))
		asp = mapasp(obj@sp)
	obj = as.data.frame(obj)
	if (is.numeric(number) && number > 1)
		obj$time = equal.count(obj$time, number = number, overlap = overlap)
	xyplot(as.formula(f), obj, asp = asp, type='l',
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
