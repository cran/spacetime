stplot = function(obj, names.attr = index(obj@time), ...) {
	stopifnot(is(obj, "ST"))
    form = as.formula(paste(names(obj@data)[1], "~time"))
    sp = obj@sp
    df = unstack(as.data.frame(obj), form)
    if(is(sp, "SpatialPixels"))
        x = SpatialPixelsDataFrame(sp, df)
    else if(is(sp, "SpatialPoints"))
        x = SpatialPointsDataFrame(sp, df)
    else if(is(sp, "SpatialPolygons"))
        x = SpatialPolygonsDataFrame(sp, df, match.ID=FALSE)
    else if(is(sp, "SpatialLines"))
        x = SpatialLinesDataFrame(sp, df, match.ID=FALSE)
    else if(is(sp, "SpatialGrid"))
        x = SpatialGridDataFrame(sp, df)
    else
        stop("spatial obj of unsupported class")
    spplot(x, names.attr = names.attr, ...)
}
