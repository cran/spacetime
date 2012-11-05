require(spacetime)
options(xts_check_TZ=FALSE)

# go through time matching first:
x = as.POSIXct("2000-01-01", tz="GMT") + (0:9) * 3600
y = x + 1
y[1] = y[2]
format(x, tz="GMT")
format(y, tz="GMT")
end.x = delta(x)
end.y = delta(y)
TI = function(x, ti) { 
	#timeIsInterval(x) = ti
	x 
}
timeMatch(TI(y,FALSE),TI(y,FALSE))
timeMatch(TI(y,TRUE), TI(y,TRUE), end.x = end.y, end.y = end.y)

timeMatch(TI(x,FALSE),TI(y,FALSE))
timeMatch(TI(x,FALSE),TI(y,TRUE), end.y = end.y)
timeMatch(TI(x,TRUE), TI(y,FALSE), end.x = end.x)
timeMatch(TI(x,TRUE), TI(y,TRUE), end.x = end.x, end.y = end.y)

timeMatch(TI(x,FALSE),TI(y,FALSE), returnList = TRUE)
timeMatch(TI(x,FALSE),TI(y,TRUE), returnList = TRUE, end.y = end.y)
timeMatch(TI(x,TRUE), TI(y,FALSE), returnList = TRUE, end.x = end.x)
timeMatch(TI(x,TRUE), TI(y,TRUE), returnList = TRUE, end.x = end.x, end.y = end.y)

end.x = delta(x)
y = x + 1 # don't replicate the first
end.y = delta(y)

a = timeMatch(TI(x,TRUE), TI(y,TRUE), end.x = end.x, end.y = end.y)
library(intervals)
b = timeMatch(TI(x,TRUE), TI(y,TRUE), end.x = end.x, end.y = end.y)
detach()
all.equal(a, b)

a = timeMatch(TI(x,TRUE), TI(y,TRUE), end.y = end.y)
library(intervals)
b = timeMatch(TI(x,TRUE), TI(y,TRUE), end.y = end.y)
detach()
all.equal(a, b)

a = timeMatch(TI(x,TRUE), TI(y,TRUE), end.x = end.x)
library(intervals)
b = timeMatch(TI(x,TRUE), TI(y,TRUE), end.x = end.x)
detach()
all.equal(a, b)

# with end points:

# next, try ST?DF objects:
t = xts(1:10, as.POSIXct("2010-05-01", tz="GMT")+3600*1:10)
xy = SpatialPixels(SpatialPoints(expand.grid(1:10,1:10)))
y = STFDF(xy, t, data.frame(a = 1:1000))
stplot(y)
x = y
all(over(x, y) == 1:1000)

t0 = xts(1:4, c(index(t)[1]-1,index(t)[c(2,4,10)]))
x = c(4,5,6,8)
xy = SpatialPixels(SpatialPoints(expand.grid(x,rep(1,4))))
x = STFDF(xy, t0, data.frame(a = 1:64))
over(x, y)

x = as(x, "STSDF")
over(x, y)

xy = SpatialPoints(cbind(c(1,3,5,10),c(5, 3, 8, 2)))
x = STIDF(xy, t0, data.frame(a = 1:4))
over(x,y)

t1 = xts(1:4, c(index(t)[1]-1,index(t)[c(2,2,10)]))
x1 = STIDF(xy[c(1,2,2,4)], t1, data.frame(a = 1:4))

over(x1, x)
over(x, x1)
over(x1, x, returnList = TRUE)
over(x, x1, returnList = TRUE)
