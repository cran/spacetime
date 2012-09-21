EOF = function(x, how = c("spatial", "temporal"), returnPredictions = TRUE, ...) { 
	sp = x@sp
	index = index(x@time)
	x = as(x, "xts") # matrix
	if (how[1] == "spatial") {
		x = t(x)
		pr = prcomp(x, ...)
		if (! returnPredictions)
			return(pr)
		x = addAttrToGeom(sp, data.frame(predict(pr)), TRUE)
	} else if (how[1] == "temporal") {
		pr = prcomp(x, ...)
		if (! returnPredictions)
			return(pr)
		x = xts(data.frame(predict(pr)), index)
	} else
		stop("unknown mode: use spatial or temporal")
	names(x) = paste("EOF", 1:ncol(x), sep="")
	x
}
