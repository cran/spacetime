require(spacetime)
t = xts(1:10, as.POSIXct(Sys.time()+60*1:10))
xy = SpatialPixels(SpatialPoints(expand.grid(1:10,1:10)))
y = STFDF(xy, t, data.frame(a = 1:1000))
stplot(y)
x = y
over(x, y)

require(spacetime)
t0 = xts(1:4, c(index(t)[1]-1,index(t)[c(2,4,10)]))
x = c(4,5,6,8)
xy = SpatialPixels(SpatialPoints(expand.grid(x,x)))
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
