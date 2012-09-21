library(spacetime)
x = as.POSIXct("2000-01-01") + (0:9) * 3600
y = x + 1
y[1] = y[2]
x
y
TI = function(x, ti) { 
	timeIsInterval(x) = ti
	x 
}
timeMatch(TI(y,FALSE),TI(y,FALSE))
timeMatch(TI(y,TRUE), TI(y,TRUE))

timeMatch(TI(x,FALSE),TI(y,FALSE))
timeMatch(TI(x,FALSE),TI(y,TRUE))
timeMatch(TI(x,TRUE), TI(y,FALSE))
timeMatch(TI(x,TRUE), TI(y,TRUE))

timeMatch(TI(x,FALSE),TI(y,FALSE), returnList = TRUE)
timeMatch(TI(x,FALSE),TI(y,TRUE), returnList = TRUE)
timeMatch(TI(x,TRUE), TI(y,FALSE), returnList = TRUE)
timeMatch(TI(x,TRUE), TI(y,TRUE), returnList = TRUE)
