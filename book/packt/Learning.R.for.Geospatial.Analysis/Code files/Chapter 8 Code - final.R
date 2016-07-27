library(raster)
dem_spain = raster("C:\\Data\\spain_elev.tif")
dem_spain = aggregate(dem_spain, 4)
spain_stations = read.csv("C:\\Data\\spain_stations.csv", 
	stringsAsFactors = FALSE)
spain_annual = read.csv("C:\\Data\\spain_annual.csv", 
	stringsAsFactors = FALSE)

create_pnt = function(stations,annual,year,variable,new_proj) {
library(plyr)
	# (1) Promoting stations to SpatialPointsDataFrame
	coordinates(stations) = ~ longitude + latitude
	# (2) Defining geographic CRS
	proj4string(stations) = CRS("+proj=longlat +datum=WGS84")
	# (3) Removing Canary Islands stations
	stations = stations[coordinates(stations)[, 1] > -10, ]
	# (4) Subsetting climatic data
	annual = annual[
		annual$year == year & 
		annual$variable == variable, ]
	# (5) Joining meteorological data with stations layer
	stations@data = join(stations@data, annual, by = "station")
	# (6) Removing incomplete records
	stations = stations[complete.cases(stations@data), ]
	# (7) transforming to the required CRS
	spTransform(stations, CRS(new_proj))
}

dat = create_pnt(stations = spain_stations,
	annual = spain_annual,
	year = 2002,
	variable = "mmnt",
	new_proj = proj4string(dem_spain))

head(dat@data)

nrow(dat)

grid = rasterToPoints(dem_spain, spatial = TRUE)

library(rgeos)
dist = gDistance(dat, grid, byid = TRUE)

dim(dist)

nearest_dat = apply(dist, 1, which.min)

grid$nn = dat$value[nearest_dat]

grid = rasterize(grid, dem_spain, "nn")

################
# Figure 08_01 #
################
png("C:\\Data\\4367OS_08_01.png",width=5.5,height=5.5,units="in",res=300)
plot(grid)
plot(dat, add = TRUE)
dev.off()

library(gstat)
g = gstat(formula = value ~ 1, data = dat)

print(g)

z = interpolate(dem_spain, g)

z = mask(z, dem_spain)

################
# Figure 08_04 #
################
png("C:\\Data\\4367OS_08_04.png",width=5.5,height=5.5,units="in",res=300)
plot(z)
plot(dat, add = TRUE)
dev.off()

g1 = gstat(formula = value ~ 1, data = dat, set = list(idp=0.3))
g2 = gstat(formula = value ~ 1, data = dat, set = list(idp=30))
z1 = interpolate(dem_spain, g1)
z2 = interpolate(dem_spain, g2)
z1 = mask(z1, dem_spain)
z2 = mask(z2, dem_spain)

################
# Figure 08_05 #
################
png("C:\\Data\\4367OS_08_05.png",width=5.5,height=2,units="in",res=300,pointsize=10)
par(mfrow = c(1,3), mai = rep(0.4, 4))
plot(z1, main = c("beta = 0.3"))
plot(dat, add = TRUE, pch = 20, cex = 0.5)
plot(z, main = c("beta = 2"))
plot(dat, add = TRUE, pch = 20, cex = 0.5)
plot(z2, main = c("beta = 30"))
plot(dat, add = TRUE, pch = 20, cex = 0.5)
dev.off()

cv = gstat.cv(g)

head(cv@data)

sqrt(sum((-cv$residual)^2)/nrow(cv))

rmse = function(x) sqrt(sum((-x$residual)^2)/nrow(x))
rmse(cv)

ev = variogram(g)

################
# Figure 08_07 #
################
png("C:\\Data\\4367OS_08_07.png",width=5.5,height=5.5,units="in",res=300)
plot(ev)
dev.off()

library(automap)
v = autofitVariogram(formula = value ~ 1, input_data = dat)

################
# Figure 08_08 #
################
png("C:\\Data\\4367OS_08_08.png",width=5.5,height=5.5,units="in",res=300)
plot(v)
dev.off()

g = gstat(formula = value ~ 1, data = dat, model = v$var_model)
g

z = interpolate(dem_spain, g)
z = mask(z, dem_spain)

################
# Figure 08_09 #
################
png("C:\\Data\\4367OS_08_09.png",width=5.5,height=5.5,units="in",res=300)
plot(z)
plot(dat, add = TRUE)
dev.off()

cv = gstat.cv(g)
rmse(cv)

################
# Figure 08_10 #
################
png("C:\\Data\\4367OS_08_10.png",width=5.5,height=5.5,units="in",res=300)
plot(value ~ elevation, dat@data, 
	xlab = "Elevation (m)", 
	ylab = "Temperature (degrees Celsius)")
dev.off()

v = autofitVariogram(formula = value ~ elevation, 
	input_data = dat)

g = gstat(formula = value ~ elevation, 
	data = dat, 
	model = v$var_model)
g

names(dem_spain) 
names(dem_spain) = "elevation"
names(dem_spain)

z = interpolate(dem_spain, g, xyOnly = FALSE)

z = mask(z, dem_spain)

################
# Figure 08_11 #
################
png("C:\\Data\\4367OS_08_11.png",width=5.5,height=5.5,units="in",res=300)
plot(z)
plot(dat, add = TRUE)
dev.off()

cv = gstat.cv(g)

rmse(cv)

cv_results = expand.grid(
	variable = c("mmnt", "mmxt"),
	year = 2001:2010,
	method = c("IDW", "OK", "UK"),
	rmse = NA)

head(cv_results)

for(i in row(cv_results)) {
	# (1) Create point layer as required
	dat = create_pnt(stations = spain_stations,
		annual = spain_annual,
		year = cv_results$year[i],
		variable = cv_results$variable[i],
		new_proj = proj4string(dem_spain))
	# (2) Create *form* and *v_mod* objects
	if(cv_results$method[i] == "IDW") {
		form = value ~ 1
		v_mod = NULL} else {  
			if(cv_results$method[i] == "OK") {
				form = value ~ 1}
			if(cv_results$method[i] == "UK") {
				form = value ~ elevation}
				v_mod = 
					autofitVariogram(
					formula = form, 
					input_data = dat)$var_model}
	# (3) Create gstat object
	g = gstat(formula = form, data = dat, model = v_mod)
	# (4) Perform cross validation
	cv = gstat.cv(g)
	# (5) Calculate RMSE and assign to cv_results
	cv_results$rmse[i] = rmse(cv)
}

head(cv_results)

tapply(cv_results$rmse, cv_results$method, mean)

spainT_tab = expand.grid(
	year = 2006:2010,
	variable = c("mmnt", "mmxt"))

spainT_tab

spainT = stack()

for(i in 1:nrow(spainT_tab)) {
	# (1) Create point layer as required
	dat = create_pnt(stations = spain_stations,
		annual = spain_annual,
		year = spainT_tab$year[i],
		variable = spainT_tab$variable[i],
		new_proj = proj4string(dem_spain))
	# (2) Automatically fit variogram model
	v = autofitVariogram(formula = value ~ elevation, 
		input_data = dat)
	# (3) Create gstat object
	g = gstat(formula = value ~ elevation, 
		model = v$var_model, 
		data = dat)
	# (4) Interpolate!
	z = interpolate(dem_spain, g, xyOnly = FALSE)
	# (5) Mask and add predicted surface to results stack
	spainT = stack(spainT, mask(z, dem_spain))
}

names(spainT)

names(spainT) = paste(spainT_tab$variable, 
	spainT_tab$year, 
	sep = "_")
names(spainT)

library(rasterVis)

################
# Figure 08_12 #
################
png("C:\\Data\\4367OS_08_12.png",width=6.5,height=3.5,units="in",res=300)
levelplot(spainT,
	par.settings = "BuRdTheme", 
	layout = c(5,2))
dev.off()

mmnt_layers = which(spainT_tab$variable == "mmnt")
mmnt_layers
mmxt_layers = which(spainT_tab$variable == "mmxt")
mmxt_layers

means = stack(mean(spainT[[mmnt_layers]]), 
	mean(spainT[[mmxt_layers]]))
names(means) = c("mmnt", "mmxt")

means = means[[spainT_tab$variable]]

names(means)

spainT = spainT - means

range(spainT[], na.rm = TRUE)

################
# Figure 08_13 #
################
png("C:\\Data\\4367OS_08_13.png",width=6.5,height=3.5,units="in",res=300)
levelplot(spainT, 
	par.settings = "BuRdTheme", 
	layout = c(5,2), 
	contour = TRUE, 
	at = c(-4:-1,1:4))
dev.off()

spainT = as.data.frame(spainT, xy = TRUE)
head(spainT)

spainT = spainT[complete.cases(spainT), ]
head(spainT)

library(reshape2)
spainT = melt(spainT, id.vars = c("x", "y"))
head(spainT)

spainT$year = as.numeric(substr(spainT$variable, 6, 9))
spainT$variable = substr(spainT$variable, 1, 4)
head(spainT)
