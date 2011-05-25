ST.Match = function(x, y, returnList) {
	# return indexes in y at the S/T points in x, or NA
}

overST = function(x, y, returnList = FALSE, fn = NULL, ...) {
    stopifnot(identical(proj4string(x), proj4string(y)))
	r = ST.Match(x, y, returnList)
    if (!returnList)
        row.names(ret) = row.names(x)
    ret
}

overSTXDF = function(x, y, returnList = FALSE, fn = NULL, ...) {
    stopifnot(identical(proj4string(x), proj4string(y)))
    r = over(x, geometry(y), returnList = TRUE)
    ret = sp:::.overDF(r, y@data, length(x), returnList, fn, ...)
    if (!returnList)
        row.names(ret) = row.names(x)
    ret
}

setMethod("over",
    signature(x = "STI", y = "ST"),
	        overST)
setMethod("over",
    signature(x = "STI", y = "STFDF"),
	        overSTXDF)
setMethod("over",
    signature(x = "STI", y = "STSDF"),
	        overSTXDF)
setMethod("over",
    signature(x = "STI", y = "STIDF"),
	        overSTXDF)
