#setClass("STL", # space-time line
  #representation("ST", endTime = "POSIXt"),
  #validity = function(object) {
    #stopifnot(nrow(object@time) == length(object@sp))
    #stopifnot(nrow(object@time) == nrow(object@endTime))
    #return(TRUE)
  #}
#)
#
#setClass("STLDF", # space-time irregular data frame
  #representation("STL", data = "data.frame"),
  #validity = function(object) {
    #n = nrow(object@data)
    #stopifnot(n == length(object@sp))
	#stopifnot(n == nrow(object@time))
    #.checkAttrIsUnique(object@sp, object@time, object@data)
    #return(TRUE)
  #}
#)
