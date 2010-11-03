as.STFDF.STPDF = function(from) {
	# fill the partial grid with NAs
	# mainly taken from as.SPixDF.SGDF in sp:
   	data = list()
   	n = length(from@sp) * nrow(from@time)
	index = length(from@sp) * (from@index[,2] - 1) + from@index[,1]
   	for (i in seq(along = from@data)) {
		v = vector(mode(from@data[[i]]), n)
      	if (is.factor(from@data[[i]]))
			v = factor(rep(NA, n), levels = levels(from@data[[i]]))
		else
			v[-index] = NA
		v[index] = from@data[[i]]
		data[[i]] = v
   	}
   	data = data.frame(data, stringsAsFactors = FALSE)
   	names(data) = names(from@data)
	STFDF(from@sp, from@time, data)
}
setAs("STPDF", "STFDF", as.STFDF.STPDF)

as.STPDF.STFDF = function(from) {
	# take out the NA cells and fill the index
	# NOTE: does not (yet) take out empty space/time entities 
	# -- shouls this be optional?
	n = length(from@sp)
	m = nrow(from@time)
	index = cbind(rep(1:n, m), rep(1:m, each=n))
	# copied from sp:
	sel = apply(sapply(from@data, is.na), 1, function(x) !all(x))
	index = index[sel,]
	STPDF(from@sp, from@time, from@data[sel,], index)
}
setAs("STFDF", "STPDF", as.STPDF.STFDF)

as.STSDF.STPDF = function(from) {
	# replicate the sp and time columns; keeps time always ordered?
	STSDF(from@sp[from@index[,1]], from@time[from@index[,2]], from@data)
}
setAs("STPDF", "STSDF", as.STSDF.STPDF)

as.STSDF.STFDF = function(from) {
	as(as(from, "STPDF"), "STSDF")
}
setAs("STFDF", "STSDF", as.STSDF.STFDF)

zerodist.sp = function(from) {
	#stopifnot(is(from@sp, "SpatialPoints"))
	z = zerodist(SpatialPoints(myCoordinates(from)))
	if(!is(from, "SpatialPoints") && nrow(z) > 0) {
		sel = apply(z, 1, function(x) identical(from[x[1]],from[x[2]]))
		z = z[sel,]
	}
	z
}

zerodist.xts = function(from) {
	t = index(from)
	n = length(t)
	n2 = n * n
	d = which(as.vector(outer(t, t, "-")) == 0)
	r = (d - 1) %% n
	c = (d - 1) %/% n
	ret = cbind(r,c) + 1
	ret[r < c, ]
}

reduce.index = function(index, z) {
	find.groups = function(z) {
		groups = list()
		i = 1
		while (nrow(z)) {
			g = min(z[,1])
			map = z[which(z[,1] == g), 2]
			gr = unique(c(g,map))
			groups[[i]] = gr
			z = z[!(z[,1] %in% gr),]
			i = i+1
		}
		groups
	}
	g = find.groups(z)
	for (i in seq(along = g))
		index[(g[[i]])] = i
	index
}

as.STPDF.STSDF = function(from) {
	# find replicates in sp and time, and fill index
	n = nrow(from@data)
	index = cbind(1:n, 1:n)
	z = zerodist.sp(from@sp)
	if (nrow(z)) {
		sp = from@sp[-z[,2],] # remove all subsequent duplicates
		index[,1] = reduce.index(index[,1], z) # reorganize index
	}
	z = zerodist.xts(from@time)
	if (nrow(z)) {
		time = from@time[-z[,2],]
		index[,2] = reduce.index(index[,2], z)
	}
	STPDF(sp, time, from@data, index)
}
setAs("STSDF", "STPDF", as.STPDF.STSDF)

as.STFDF.STSDF = function(from) {
	as(as(from, "STPDF"), "STFDF")
}
setAs("STSDF", "STFDF", as.STFDF.STSDF)
