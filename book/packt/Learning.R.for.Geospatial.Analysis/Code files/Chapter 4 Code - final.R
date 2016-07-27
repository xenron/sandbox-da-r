### From Chapter 3
dates = read.csv("C:\\Data\\modis_dates.csv")
dates$date = as.Date(
	paste(dates$year, dates$month, dates$day, sep = "-"))
month = c(12, 1:11)
season = rep(c("winter","spring","summer","fall"), each = 3)
seasons = data.frame(month, season)
library(plyr)
dates = join(dates, seasons, "month")
###

matrix(1:6, ncol = 3)

matrix(1:6, nrow = 3)
matrix(1:6, nrow = 2)

matrix(12:1, ncol = 4, nrow = 2)
matrix(12:1, ncol = 4, nrow = 4)

x = matrix(7:12, ncol = 3, byrow = TRUE)
x

nrow(x)
ncol(x)

dim(x)

as.vector(x)

x[, c(1,3)]

x[2, ]

x[2, , drop = FALSE]

m = matrix(NA, ncol = 3, nrow = 3)
m
m[2:3, 1:2] = matrix(1:4, nrow = 2)
m

apply(x, 2, mean)

colMeans(x)

y = array(1:24, c(2,2,3))
y

y[2,1,3]
apply(y, 3, mean)

library(raster)
r1 = raster(x)
r1

band1 = raster("C:\\Data\\landsat_15_10_1998.tif")

band1

band4 = raster("C:\\Data\\landsat_15_10_1998.tif", band = 4)

stack(band1, band4)

l_00 = brick("C:\\Data\\landsat_04_10_2000.tif")
l_00

class(l_00[[2]])

class(l_00[[1:3]])

#writeRaster(l_00, 
#	"C:\\Data\\landsat_04_10_2000.img", 
#	format = "HFA", 
#	overwrite = FALSE)

nrow(l_00)
ncol(l_00)
nlayers(l_00)

dim(l_00)

ncell(l_00)

res(l_00)

extent(l_00)

proj4string(l_00)

CRS(proj4string(l_00))

proj4string(l_00) = NA
proj4string(l_00)

proj4string(l_00) = 
	CRS("+proj=utm +zone=36 +ellps=WGS84 +units=m +no_defs")
proj4string(l_00)

l1 = raster("C:\\Data\\landsat_15_10_1998.tif")
proj4string(l_00) = CRS(proj4string(l1))

names(l_00)

names(l_00) = paste("Band", 1:6, sep = "_")
names(l_00)

################
# Figure 04_01 #
################
png("C:\\Data\\4367OS_04_01.png",width=5.5,height=4,units="in",res=300)
hist(l_00)
dev.off()

library(rasterVis)

################
# Figure 04_02 #
################
png("C:\\Data\\4367OS_04_02.png",width=5.5,height=4,units="in",res=300)
levelplot(l_00, par.settings = RdBuTheme, contour = FALSE)
dev.off()

r = brick("C:\\Data\\modis.tif")
r

################
# Figure 04_03 #
################
library(plotKML)
plotKML(r[[1]])

getwd()
setwd("C:\\Data")
getwd()

r[[1]][1:5]

mean(r[[1]][], na.rm = TRUE)

r[[1]][1, 1:5]

r[[1:3]][1, 1:5]

v = r[45, 33][1, ]

################
# Figure 04_04 #
################
png("C:\\Data\\4367OS_04_04.png",width=5.5,height=3,units="in",res=300)
plot(v ~ dates$date, type = "l", xlab = "Time", ylab = "NDVI")
dev.off()

u = r[[1:3]][1:2, 1:2, drop = FALSE]

################
# Figure 04_05 #
################
png("C:\\Data\\4367OS_04_05.png",width=5.5,height=2.5,units="in",res=300)
levelplot(u, layout = c(3,1), par.settings = RdBuTheme)
dev.off()

as.matrix(u[[1]])

as.array(u[[1:2]])

as.matrix(u[[1:2]])

min_ndvi = min(r, na.rm = TRUE)

min(r[[1]][], na.rm = TRUE)

range_ndvi = range(r, na.rm = TRUE)

################
# Figure 04_06 #
################
png("C:\\Data\\4367OS_04_06.png",width=5.5,height=2.5,units="in",res=300)
levelplot(range_ndvi, par.settings = RdBuTheme, contour = TRUE)
dev.off()

prop_na = function(x) length(x[is.na(x)]) / length(x)
prop_na(c(10,3,NA,2))

prop_na_r = calc(r, fun = prop_na)

ndvi = function(x) (x[4] - x[3]) / (x[4] + x[3])
ndvi_00 = calc(l_00, fun = ndvi)

################
# Figure 04_07 #
################
png("C:\\Data\\4367OS_04_07.png",width=5.5,height=5.5,units="in",res=300)
levelplot(ndvi_00, par.settings = RdBuTheme, contour = FALSE)
dev.off()

sum(is.na(r[[1]])[])

temp = r[[1]]
temp[is.na(temp)] = mean(temp[], na.rm = TRUE)

################
# Figure 04_08 #
################
png("C:\\Data\\4367OS_04_08.png",width=5.5,height=2.5,units="in",res=300)
levelplot(stack(r[[1]], temp), 
	par.settings = RdBuTheme, contour = FALSE)
dev.off()

l_rec = ndvi_00
l_rec[l_rec <= 0.2] = 0
l_rec[l_rec > 0.2] = 1

################
# Figure 04_09 #
################
png("C:\\Data\\4367OS_04_09.png",width=5,height=5.5,units="in",res=300)
plot(l_rec)
dev.off()

l_rec = reclassify(ndvi_00, c(-Inf, 0.2, 0, 0.2, Inf, 1))

