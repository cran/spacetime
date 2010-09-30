setClass("ST",
  representation(sp = "Spatial", time = "xts"),
  validity = function(object) {
    stopifnot(length(object@sp) >= 1)
	stopifnot(nrow(object@time) >= 1)
	return(TRUE)
  }
)
