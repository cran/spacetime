if (!isGeneric("timeIsInterval"))
	setGeneric("timeIsInterval", function(x, ...)
		standardGeneric("timeIsInterval"))

if (!isGeneric("timeIsInterval<-"))
	setGeneric("timeIsInterval<-", function(x, value)
		standardGeneric("timeIsInterval<-"))

setMethod("timeIsInterval", "ST",
	function(x, ...) {
		timeIsInterval(x@time)
	}
)

setMethod("timeIsInterval", "ANY",
	function(x, ...) {
		a = attr(x, "timeIsInterval")
		if (is.null(a) || is.na(a))
			return(as.logical(NA))
		stopifnot(is.logical(a))
		a
	}
)

setReplaceMethod("timeIsInterval", c("STT", "logical"),
	function(x, value) {
		if (isTRUE(value))
			stop("timeIsInterval cannot be TRUE for STT objects")
		attr(x@time, "timeIsInterval") = value
		x
	}
)

setReplaceMethod("timeIsInterval", c("ST", "logical"),
	function(x, value) {
		if (isTRUE(value)) {
			if (min(diff(index(x@time))) <= 0)
				warning("zero-width time intervals may yield invalid matching results")
		}
		attr(x@time, "timeIsInterval") = value
		x
	}
)

setReplaceMethod("timeIsInterval", c("ANY", "logical"), # including xts
	function(x, value) {
		if (isTRUE(value) && is(x, "xts")) {
			if (min(diff(index(x))) <= 0)
				warning("zero-width time intervals may yield invalid matching results")
		}
		attr(x, "timeIsInterval") = value
		x
	}
)
