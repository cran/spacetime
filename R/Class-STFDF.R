setClass("STF", # space-time full
  representation("ST"),
  validity = function(object) {
    return(TRUE)
  }
)

setClass("STFDF", # space-time full data frame
  representation("STF", data = "data.frame"),
  validity = function(object) {
    stopifnot(nrow(object@data) == length(object@sp) * nrow(object@time))
    return(TRUE)
  }
)
