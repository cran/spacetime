library(RPostgreSQL)
drv <- dbDriver("PostgreSQL")
dbname = "edzer"
user = "edzer"
password = "pw"
#password = ""
con <- dbConnect(drv)
con <- dbConnect(drv, dbname=dbname, user=user, password=password)
library(spacetime)
data(air)
rural = as(rural, "STSDF")
p = rural@sp
sp = SpatialPointsDataFrame(p, data.frame(geom_id=1:length(p)))
library(rgdal)
OGRstring = paste("PG:dbname=", dbname, " user=", user, 
	" password=", password, sep = "")
writeOGR(sp, OGRstring, "rural_space", driver = "PostgreSQL")

