setClass("STS", # space-time sparse
  representation("ST"),
  validity = function(object) {
    stopifnot(nrow(object@time) == length(object@sp))
    return(TRUE)
  }
)

setClass("STSDF", # space-time sparse data frame
  representation("STS", data = "data.frame"),
  validity = function(object) {
    stopifnot(nrow(object@data) == length(object@sp))
	stopifnot(nrow(object@data) == nrow(object@time))
    return(TRUE)
  }
)
