library(raster)

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

### From Chapter 4
l_00 = brick("C:\\Data\\landsat_04_10_2000.tif")
ndvi = function(x) (x[4] - x[3]) / (x[4] + x[3])
ndvi_00 = calc(l_00, fun = ndvi)
l_rec = ndvi_00
l_rec[l_rec <= 0.2] = 0
l_rec[l_rec > 0.2] = 1
r = brick("C:\\Data\\modis.tif")
###

### From Chapter 6
dem1 = getData("SRTM", lon=33, lat=33)
dem2 = getData("SRTM", lon=38, lat=33)
dem = merge(dem1, dem2)
haifa_buildings = readOGR("C:\\Data", "haifa_buildings")
haifa_surrounding = extent(haifa_buildings) + 0.25
dem = crop(dem, haifa_surrounding)
dem = projectRaster(from = dem, 
	crs = proj4string(r), 
	method = "ngb", 
	res = 90)
slope = terrain(dem, "slope")
aspect = terrain(dem, "aspect")

l_rec_focal = focal(l_rec, 
	w = matrix(1, nrow=3, ncol=3), 
	fun = max)
l_rec_focal_clump = clump(l_rec_focal, gaps = FALSE)
###

### From Chapter 5
library(rgeos)
haifa_buildings = readOGR("C:\\Data", "haifa_buildings")
haifa_natural = readOGR("C:\\Data", "haifa_natural")
israel_adm = getData("GADM", country = "ISR", level = 1)
haifa_adm = israel_adm[israel_adm$NAME_1 == "Haifa", ]
haifa_adm = spTransform(haifa_adm, CRS(proj4string(l_00)))
haifa_buildings = spTransform(haifa_buildings, CRS(proj4string(l_00)))
haifa_natural = spTransform(haifa_natural, CRS(proj4string(l_00)))
buildings_ch = gConvexHull(haifa_buildings)
buildings_ch = gIntersection(buildings_ch, haifa_adm)
haifa_natural = gUnaryUnion(haifa_natural)
haifa_natural = gIntersection(haifa_natural, buildings_ch)
buildings_50m = gBuffer(haifa_buildings, width = 50)
haifa_natural = gDifference(haifa_natural, buildings_50m)
###

library(ggmap)
towns_names = c("Lahav Kibbutz", "Lehavim")
towns = geocode(towns_names)
coordinates(towns) = ~ lon + lat
proj4string(towns) = CRS("+proj=longlat +datum=WGS84")
towns = spTransform(towns, CRS(proj4string(l_00)))

################
# Figure 07_01 #
################
png("C:\\Data\\4367OS_07_01.png",width=4.55,height=5.5,units="in",res=300)
plotRGB(l_00, r = 3, g = 2, b = 1, stretch = "lin")
plot(towns, col = "red", pch = 16, add = TRUE)
text(coordinates(towns), towns_names, pos = 3, col = "white")
dev.off()

towns_r = rasterize(towns, r)

towns_r[!is.na(towns_r)]

towns_r = crop(towns_r, extent(towns) + 3000)

################
# Figure 07_02 #
################
png("C:\\Data\\4367OS_07_02.png",width=5.5,height=3.45,units="in",res=300)
plot(towns_r, col = c("lightblue", "brown"))
plot(towns, add = TRUE)
text(coordinates(towns), towns_names, pos = 3)
dev.off()

haifa_ext = extent(haifa_buildings) + 2000

################
# Figure 07_03 #
################
png("C:\\Data\\4367OS_07_03.png",width=5.1,height=5.5,units="in",res=300)
plot(slope, ext = haifa_ext)
plot(haifa_buildings, add = TRUE)
plot(haifa_natural, col = "lightgreen", add = TRUE)
dev.off()

natural_mask = mask(slope, haifa_natural)

natural_mask = crop(natural_mask, haifa_ext)

buildings_mask = mask(slope, haifa_buildings)
buildings_mask = crop(buildings_mask, haifa_ext)

################
# Figure 07_04 #
################
png("C:\\Data\\4367OS_07_04.png",width=5.5,height=2.5,units="in",res=300)
plot(stack(natural_mask, buildings_mask))
dev.off()

buildings_ctr = gCentroid(haifa_buildings, byid = TRUE)

buildings_mask = mask(slope, buildings_ctr)
buildings_mask = crop(buildings_mask, haifa_ext)

################
# Figure 07_05 #
################
png("C:\\Data\\4367OS_07_05.png",width=5.5,height=2.5,units="in",res=300)
plot(stack(natural_mask, buildings_mask))
dev.off()

u = r[[1:2]][1:3, 1:3, drop = FALSE]

u[2, 3] = NA
u[[1]][3, 2] = NA

u_pnt = rasterToPoints(u, spatial = TRUE)

################
# Figure 07_06 #
################
png("C:\\Data\\4367OS_07_06.png",width=5.5,height=5.35,units="in",res=300)
plot(u[[1]])
plot(u_pnt, add = TRUE)
dev.off()

u_pnt@data

range(dem[], na.rm = TRUE)

dem_contour = rasterToContour(dem, levels = seq(0, 500, 100))

################
# Figure 07_07 #
################
png("C:\\Data\\4367OS_07_07.png",width=5.1,height=5.5,units="in",res=300)
plot(dem)
plot(dem_contour, add = TRUE)
dev.off()

dem_contour@data

pol = rasterToPolygons(l_rec_focal_clump, dissolve = TRUE)

pol$area = gArea(pol, byid = TRUE) / 1000^2
pol = pol[pol$area > 1, ]

pol@data

################
# Figure 07_08 #
################
png("C:\\Data\\4367OS_07_08.png",width=4.55,height=5.5,units="in",res=300)
plotRGB(l_00, r = 3, g = 2, b = 1, stretch = "lin")
plot(pol, border = "yellow", lty = "dotted", add = TRUE)
dev.off()

dist_towns = gDistance(towns, pol, byid = TRUE)
dist_towns

dist_order = order(dist_towns[, 1])
dist_order

forests = pol[dist_order[1:2], ]

forests@data

################
# Figure 07_09 #
################
png("C:\\Data\\4367OS_07_09.png",width=4.55,height=5.5,units="in",res=300)
plotRGB(l_00, r = 3, g = 2, b = 1, stretch = "lin")
plot(towns[1, ], col = "red", pch = 16, add = TRUE)
plot(pol, border = "yellow", lty = "dotted", add = TRUE)
plot(forests, border = "red", lty = "dotted", add = TRUE)
text(gCentroid(pol, byid = TRUE), 
	round(dist_towns[,1]), 
	col = "White")
dev.off()

forests$name = c("Lahav", "Kramim")
forests@data

dem_spain = raster("C:\\Data\\spain_elev.tif")

stations = read.csv("C:\\Data\\spain_stations.csv",
	stringsAsFactors = FALSE)

coordinates(stations) = ~ longitude + latitude

proj4string(stations) = CRS("+proj=longlat +datum=WGS84")
stations = spTransform(stations, CRS(proj4string(dem_spain)))

################
# Figure 07_10 #
################
png("C:\\Data\\4367OS_07_10.png",width=5.5,height=4.75,units="in",res=300)
plot(dem_spain)
plot(stations, add = TRUE)
dev.off()

stations$elev_dem = extract(dem_spain, stations)

head(stations@data)

################
# Figure 07_11 #
################
png("C:\\Data\\4367OS_07_11.png",width=5.5,height=5.5,units="in",res=300)
plot(elev_dem ~ elevation, stations, 
	xlim = c(0, 2000), 
	ylim = c(0, 2000),
	xlab = "Elevation from station record (m)",
	ylab = "Elevation from DEM (m)")
abline(a = 0, b = 1, lty = "dotted")
dev.off()

l_98 = brick("C:\\Data\\landsat_15_10_1998.tif")
l_03 = brick("C:\\Data\\landsat_11_09_2003.tif")
ndvi_98 = calc(l_98, fun = ndvi)
ndvi_03 = calc(l_03, fun = ndvi)

l_dates = as.Date(c("1998-10-15", "2000-10-04", "2003-09-11"))

l_forests = extract(stack(ndvi_98, ndvi_00, ndvi_03), 
	forests, 
	fun = mean,
	na.rm = TRUE)

l_forests

r_forests = extract(r,   
	forests, 
	fun = mean,
	na.rm = TRUE)

dim(r_forests)

l_forests = as.data.frame(t(l_forests))
l_forests

colnames(l_forests) = forests$name
l_forests$date = l_dates
l_forests$sat = "Landsat"

l_forests

r_forests = as.data.frame(t(r_forests))
colnames(r_forests) = forests$name
r_forests$date = dates$date
r_forests$sat = "MODIS"

forests_ndvi = rbind(l_forests, r_forests)

head(forests_ndvi)

library(reshape2)
forests_ndvi = melt(forests_ndvi, 
	measure.vars = forests$name, 
	variable.name = "forest",
	value.name = "ndvi")

forests$name

head(forests_ndvi)

