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
library(raster)
l_00 = brick("C:\\Data\\landsat_04_10_2000.tif")
ndvi = function(x) (x[4] - x[3]) / (x[4] + x[3])
ndvi_00 = calc(l_00, fun = ndvi)
l_rec = ndvi_00
l_rec[l_rec <= 0.2] = 0
l_rec[l_rec > 0.2] = 1
r = brick("C:\\Data\\modis.tif")
###

dem1 = getData("SRTM", lon=33, lat=33)
dem2 = getData("SRTM", lon=38, lat=33)

dem = merge(dem1, dem2)

haifa_buildings = readOGR("C:\\Data", "haifa_buildings")
haifa_surrounding = extent(haifa_buildings) + 0.25

################
# Figure 06_01 #
################
png("C:\\Data\\4367OS_06_01.png",width=5.5,height=3.45,units="in",res=300,pointsize=10)
plot(dem)
plot(haifa_surrounding, add = TRUE)
dev.off()

dem = crop(dem, haifa_surrounding)

################
# Figure 06_02 #
################
png("C:\\Data\\4367OS_06_02.png",width=5.0,height=5.5,units="in",res=300)
plot(dem)
dev.off()

dem_agg8 = aggregate(dem, fact = 8)
dem_agg16 = aggregate(dem, fact = 16)

################
# Figure 06_03 #
################
png("C:\\Data\\4367OS_06_03.png",width=5.5,height=2.0,units="in",res=300,pointsize=10)
par(mfrow = c(1,3), mai = rep(0.5, 4))
plot(dem, main = "Original image")
plot(dem_agg8, main = "8x8 aggregated")
plot(dem_agg16, main = "16x16 aggregated")
dev.off()

l_date = which.min(abs(dates$date - as.Date("2000-10-04")))

l_date

l_resample = resample(ndvi_00, r[[l_date]], method = "ngb")

################
# Figure 06_04 #
################
png("C:\\Data\\4367OS_06_04.png",width=5.5,height=3.0,units="in",res=300,pointsize=10)
par(mfrow=c(1,2), mai = rep(0.75, 4))
plot(ndvi_00, main = "Original Landsat image")
plot(l_resample, ext = extent(ndvi_00), 
	main = "Resampled to MODIS")
dev.off()

r_resample = resample(r[[l_date]], ndvi_00, method = "ngb")

r_resample = extend(r_resample, r[[l_date]])

################
# Figure 06_05 #
################
png("C:\\Data\\4367OS_06_05.png",width=5.5,height=3.0,units="in",res=300,pointsize=10)
par(mfrow=c(1,2), mai = rep(0.75, 4))
plot(r[[l_date]], main = "Original MODIS image")
plot(extent(ndvi_00), add = TRUE)
plot(r_resample, main = "Resampled to Landsat")
dev.off()

r_resample_ngb = resample(r[[l_date]], ndvi_00, method = "ngb")
r_resample_bil = resample(r[[l_date]], ndvi_00, 
	method = "bilinear")
resample_results = stack(r_resample_ngb, r_resample_bil)
names(resample_results) = c("Nearest neighbor", 
	"Bilinear interpolation")

################
# Figure 06_06 #
################
png("C:\\Data\\4367OS_06_06.png",width=5.5,height=3.5,units="in",res=300)
library(rasterVis)
levelplot(resample_results, 
	par.settings = RdBuTheme, 
	contour = TRUE)
dev.off()

dem

dem = projectRaster(from = dem, 
	crs = proj4string(r), 
	method = "ngb", 
	res = 90)

dem

################
# Figure 06_07 #
################
png("C:\\Data\\4367OS_06_07.png",width=5.0,height=5.5,units="in",res=300)
plot(dem)
dev.off()

matrix(1, nrow = 3, ncol = 3)

l_rec_focal = focal(l_rec, 
	w = matrix(1, nrow = 3, ncol = 3), 
	fun = max)

l_rec_focal_clump = clump(l_rec_focal, gaps = FALSE)

################
# Figure 06_08 #
################
png("C:\\Data\\4367OS_06_08.png",width=5.5,height=2.0,units="in",res=300,pointsize=10)
par(mfrow = c(1,3), mai = rep(0.5, 4))
plot(l_rec, main = "Original image")
plot(l_rec_focal, main = "Filtered")
plot(l_rec_focal_clump, main = "Clumped")
dev.off()

slope = terrain(dem, "slope")
aspect = terrain(dem, "aspect")

################
# Figure 06_09 #
################
png("C:\\Data\\4367OS_06_09.png",width=5.5,height=2.5,units="in",res=300,pointsize=10)
plot(stack(slope, aspect))
dev.off()

hill = hillShade(slope, aspect, 20, 235)

################
# Figure 06_10 #
################
png("C:\\Data\\4367OS_06_10.png",width=5.5,height=5.5,units="in",res=300)
levelplot(hill, par.settings = GrTheme, margin = FALSE)
dev.off()

seasons = c("winter", "spring", "summer", "fall")
season_means = stack()

for(i in seasons) {
	season_means = stack(season_means,
	overlay(r[[which(dates$season == i)]],
	fun = function(x) mean(x, na.rm = TRUE)))
	}

names(season_means) = seasons

################
# Figure 06_11 #
################
png("C:\\Data\\4367OS_06_11.png",width=5.5,height=5.5,units="in",res=300)
levelplot(season_means, par.settings=RdBuTheme, contour=TRUE)
dev.off()

month_means = stackApply(r, 
	indices = dates$month, 
	fun = mean, 
	na.rm = TRUE)

names(month_means) = month.abb

################
# Figure 06_12 #
################
png("C:\\Data\\4367OS_06_12.png",width=5.5,height=4.5,units="in",res=300)
levelplot(month_means, par.settings = RdBuTheme, contour = TRUE)
dev.off()

min_month = overlay(month_means, fun = which.min)

dates$year[1:30]

as.numeric(factor(dates$year))[1:30]

s = raster(nrows = nrow(r), ncols = nlayers(r),
	xmn = 0, xmx =  nlayers(r), ymn = 0, ymx = nrow(r))

raster_rowMeans = function(x, layer) {
	rowMeans(as.matrix(x[[layer]]),
	na.rm = TRUE)
	}

for(i in 1:nlayers(r)) {
	s[ ,i] = raster_rowMeans(r, i)
	}

################
# Figure 06_14 #
################
png("C:\\Data\\4367OS_06_14.png",width=5.5,height=2.5,units="in",res=300)
levelplot(s,
	par.settings = RdBuTheme,
	contour = TRUE,
	margin = FALSE,
	at = seq(0,0.6,0.05))
dev.off()

s = raster(nrows = nrow(r), ncols = nlayers(r),
	xmn = 0, xmx =  nlayers(r), ymn = 0, ymx = nrow(r))

r_array = as.array(r)

s[] = apply(r_array, 3, rowMeans, na.rm = TRUE)

