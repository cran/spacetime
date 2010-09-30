initTime = function(time) {
	# time = time[,1] # drop further columns
	# time[,1] = 1:nrow(time)
	time
}

ST = function(sp, time) {
	new("ST", sp = sp, time = initTime(time))
}

setMethod("[[", c("ST", "ANY", "missing"), 
	function(x, i, j, ...) {
		if (!("data" %in% slotNames(x)))
			stop("no [[ method for object without attributes")
		x@data[[i]]
	}
)
setReplaceMethod("[[", c("ST", "ANY", "missing", "ANY"), 
	function(x, i, j, value) {
		if (!("data" %in% slotNames(x)))
			stop("no [[ method for object without attributes")
		if (is.character(i)) {
			if (any(!is.na(match(i, dimnames(coordinates(x@sp))[[2]]))))
				stop(paste(i, "is already present as a coordinate name!"))
			if (i == "time")
				stop("cannot set time")
		}
		x@data[[i]] <- value
		x
	}
)

setMethod("$", "ST", 
	function(x, name) {
		if (!("data" %in% slotNames(x)))
			stop("no $ method for object without attributes")
		x@data[[name]]
	}
)

setReplaceMethod("$", "ST", 
	function(x, name, value) { 
		if (!("data" %in% slotNames(x)))
			stop("no $<- method for object without attributes")
		x@data[[name]] = value 
		x 
	}
)

dim.ST = function(x) {
	c(length(x@sp), nrow(x@time))
}
dim.STDF = function(x) {
	c(length(x@sp), nrow(x@time), ncol(x@data))
}
dim.STF = dim.STP = dim.STS = dim.ST
dim.STFDF = dim.STPDF = dim.STSDF = dim.STDF

setMethod("proj4string", "ST", function(obj) proj4string(obj@sp))

setMethod("is.projected", "ST", function(obj) is.projected(obj@sp))

setReplaceMethod("proj4string", signature(obj = "ST", value = "CRS"), 
	function(obj,value) {
		proj4string(obj@sp) = value
		obj
	}
)
setReplaceMethod("proj4string", signature(obj = "ST", value = "character"), 
	function(obj, value) {
		proj4string(obj@sp) = value
		obj
	}
)
