setClass("STI", # space-time irregular
  representation("ST"),
  validity = function(object) {
    stopifnot(nrow(object@time) == length(object@sp))
    return(TRUE)
  }
)

setClass("STIDF", # space-time irregular data frame
  representation("STI", data = "data.frame"),
  validity = function(object) {
    n = nrow(object@data)
    stopifnot(n == length(object@sp))
	stopifnot(n == nrow(object@time))
    return(TRUE)
  }
)
