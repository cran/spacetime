setClass("ST",
  representation(sp = "Spatial", time = "xts", endTime = "POSIXct"),
  validity = function(object) {
    stopifnot(length(object@sp) >= 1)
	stopifnot(nrow(object@time) >= 1)
	stopifnot(nrow(object@time) == length(object@endTime))
	#stopifnot(!is.na(timeIsInterval(object)))
	if (any(names(object@time) %in% names(object@sp)))
		stop("name conflict: attribute names in sp and time slot must differ")
	return(TRUE)
  }
)

.checkAttrIsUnique = function(sp, time, data) {
	if (any(names(sp) %in% names(data)))
		stop("name conflict: attribute name(s) already present in sp slot of ST object")
	if (any(names(time) %in% names(data)))
		stop("name conflict: attribute name(s) already present in time slot of ST object")
}
