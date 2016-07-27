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
hill = hillShade(slope, aspect, 20, 235)
l_rec_focal = focal(l_rec, 
	w = matrix(1, nrow=3, ncol=3), 
	fun = max)
l_rec_focal_clump = clump(l_rec_focal, gaps = FALSE)
s = raster(nrows = nrow(r), ncols = nlayers(r),
	xmn = 0, xmx =  nlayers(r), ymn = 0, ymx = nrow(r))
r_array = as.array(r)
s[] = apply(r_array, 3, rowMeans, na.rm = TRUE)
###

### From Chapter 5
track = readOGR("C:\\Data\\GPS_log.gpx","tracks")
track = spTransform(track, CRS(proj4string(r)))
library(rgeos)
county = readOGR("C:\\Data", "USA_2_GADM_fips", 
	stringsAsFactors = FALSE)
county = county[
	county$NAME_1 != "Alaska" &
	county$NAME_1 != "Hawaii", ]
county = county[county$TYPE_2 != "Water body", ]
newProj = CRS("+proj=laea +lat_0=45 +lon_0=-100 
	+x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")
county = spTransform(county, newProj)
county$area = gArea(county, byid = TRUE) / 1000^2
dat = read.csv("C:\\Data\\CO-EST2012-Alldata.csv")
selected_cols = c("STATE", "COUNTY", "CENSUS2010POP")
dat = dat[, colnames(dat) %in% selected_cols]
colnames(dat) = tolower(colnames(dat))
dat = dat[dat$county != 0, ]
dat$state = formatC(dat$state, width = 2, flag = "0")
dat$county = formatC(dat$county, width = 3, flag = "0")
dat$FIPS = paste0(dat$state, dat$county)
county@data = join(county@data, 
	dat[, colnames(dat) %in% c("FIPS", "census2010pop")], 
	by = "FIPS")
county$density = county$census2010pop / county$area
boundary = readOGR("C:\\Data", "CTYUA_DEC_2013_EW_BFE")
buildings = readOGR("C:\\Data", "london_buildings")
natural = readOGR("C:\\Data", "london_natural")
buildings = spTransform(buildings, CRS(proj4string(boundary)))
natural = spTransform(natural, CRS(proj4string(boundary)))
city = boundary[boundary$CTYUA13NM == "City of London", ]
in_city = gContains(city, buildings, byid = TRUE)
buildings = buildings[in_city[,1], ]
river = natural[natural$type == "riverbank", ]
river = gUnaryUnion(river)
dist = gDistance(buildings, river, byid = TRUE)
buildings$dist_river = dist[1, ]
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

### From Chapter 7
haifa_ext = extent(haifa_buildings) + 2000
natural_mask = mask(slope, haifa_natural)
natural_mask = crop(natural_mask, haifa_ext)
buildings_ctr = gCentroid(haifa_buildings, byid = TRUE)
buildings_mask = mask(slope, buildings_ctr)
buildings_mask = crop(buildings_mask, haifa_ext)
library(ggmap)
towns_names = c("Lahav Kibbutz", "Lehavim")
towns = geocode(towns_names)
coordinates(towns) = ~ lon + lat
proj4string(towns) = CRS("+proj=longlat +datum=WGS84")
towns = spTransform(towns, CRS(proj4string(l_00)))
pol = rasterToPolygons(l_rec_focal_clump, dissolve = TRUE)
pol$area = gArea(pol, byid = TRUE) / 1000^2
pol = pol[pol$area > 1, ]
dist_towns = gDistance(towns, pol, byid = TRUE)
dist_order = order(dist_towns[, 1])
forests = pol[dist_order[1:2], ]
forests$name = c("Lahav", "Kramim")
l_98 = brick("C:\\Data\\landsat_15_10_1998.tif")
l_03 = brick("C:\\Data\\landsat_11_09_2003.tif")
ndvi_98 = calc(l_98, fun = ndvi)
ndvi_03 = calc(l_03, fun = ndvi)
l_dates = as.Date(c("1998-10-15", "2000-10-04", "2003-09-11"))
l_forests = extract(stack(ndvi_98, ndvi_00, ndvi_03), 
	forests, 
	fun = mean,
	na.rm = TRUE)
r_forests = extract(r,   
	forests, 
	fun = mean,
	na.rm = TRUE)
l_forests = as.data.frame(t(l_forests))
colnames(l_forests) = forests$name
l_forests$date = l_dates
l_forests$sat = "Landsat"
r_forests = as.data.frame(t(r_forests))
colnames(r_forests) = forests$name
r_forests$date = dates$date
r_forests$sat = "MODIS"
forests_ndvi = rbind(l_forests, r_forests)
library(reshape2)
forests_ndvi = melt(forests_ndvi, 
	measure.vars = forests$name, 
	variable.name = "forest",
	value.name = "ndvi")
###

### From Chapter 8
dem_spain = raster("C:\\Data\\spain_elev.tif")
dem_spain = aggregate(dem_spain, 4)
spain_stations = read.csv("C:\\Data\\spain_stations.csv", 
	stringsAsFactors = FALSE)
spain_annual = read.csv("C:\\Data\\spain_annual.csv", 
	stringsAsFactors = FALSE)
create_pnt = function(stations,annual,year,variable,new_proj) {
	library(plyr)
	coordinates(stations) = ~ longitude + latitude
	proj4string(stations) = CRS("+proj=longlat +datum=WGS84")
	stations = stations[coordinates(stations)[, 1] > -10, ]
	annual = annual[
		annual$year == year & 
		annual$variable == variable, ]
	stations@data = join(stations@data, annual, by = "station")
	stations = stations[complete.cases(stations@data), ]
	spTransform(stations, CRS(new_proj))
}
names(dem_spain) = "elevation"
spainT_tab = expand.grid(
	year = 2006:2010,
	variable = c("mmnt", "mmxt"))
spainT = stack()
library(gstat)
library(automap)
for(i in 1:nrow(spainT_tab)) {
	dat = create_pnt(stations = spain_stations,
		annual = spain_annual,
		year = spainT_tab$year[i],
		variable = spainT_tab$variable[i],
		new_proj = proj4string(dem_spain))
	v = autofitVariogram(formula = value ~ elevation, 
		input_data = dat)
	g = gstat(formula = value ~ elevation, 
		model = v$var_model, 
		data = dat)
	z = interpolate(dem_spain, g, xyOnly = FALSE)
	spainT = stack(spainT, mask(z, dem_spain))
}
names(spainT) = paste(spainT_tab$variable, 
	spainT_tab$year, 
	sep = "_")
mmnt_layers = which(spainT_tab$variable == "mmnt")
mmxt_layers = which(spainT_tab$variable == "mmxt")
means = stack(mean(spainT[[mmnt_layers]]), 
	mean(spainT[[mmxt_layers]]))
means = means[[spainT_tab$variable]]
spainT = spainT - means
spainT = as.data.frame(spainT, xy = TRUE)
spainT = spainT[complete.cases(spainT), ]
library(reshape2)
spainT = melt(spainT, id.vars = c("x", "y"))
spainT$year = as.numeric(substr(spainT$variable, 6, 9))
spainT$variable = substr(spainT$variable, 1, 4)
###

### From Chapter 2
dat = read.csv("C:\\Data\\338284.csv", stringsAsFactors = FALSE)
time = dat$DATE
tmax = dat$TMAX
time = as.Date(as.character(time), format = "%Y%m%d")
tmax[tmax == -9999] = NA
tmax = tmax / 10
w = time > as.Date("2005-12-31") & time < as.Date("2014-1-1")
time = time[w]
tmax = tmax[w]
dat = data.frame(time = time, tmax = tmax)
###

head(dat)

library(ggplot2)
ggplot(dat, aes(x = time, y = tmax)) + 
	geom_line()

################
# Figure 09_01 #
################
ggsave("C:\\Data\\4367OS_09_01.png", width = 5.5, height = 3.5)

#ggplot(dat, aes(x = time, y = tmax))

ggplot(data = dat, aes(x = time, y = tmax)) + 
	layer(geom = "line", stat = "identity")

ggplot(dat, aes(x = time, y = tmax)) + 
	geom_line(colour = "black") +
	scale_x_date() +
	scale_y_continuous()

ggplot(dat, aes(x = time, y = tmax)) + 
	geom_line() +
	scale_x_date(name = "Time") +
	scale_y_continuous(
		name = 
			expression(paste("Maximum temperature (", degree, "C)"))) +
	theme_bw() +
	theme(panel.grid = element_blank())

################
# Figure 09_02 #
################
ggsave("C:\\Data\\4367OS_09_02.png", width = 5.5, height = 3.5)

ggplot() +
	geom_line(
    data = forests_ndvi[forests_ndvi$sat == "MODIS", ],
		aes(x = date, y = ndvi, colour = forest)) +
	geom_point(
    data = forests_ndvi[forests_ndvi$sat == "Landsat", ],
		aes(x = date, y = ndvi, fill = forest), shape = 21) +
	scale_x_date("Time") +
	scale_y_continuous("NDVI") +
	scale_fill_discrete("Forest") +
	scale_colour_discrete("Forest") +
	theme_bw() +
	theme(panel.grid = element_blank(),
		legend.position = "top")

################
# Figure 09_03 #
################
ggsave("C:\\Data\\4367OS_09_03.png", width = 5.5, height = 3)

build = data.frame(cover = "Buildings", 
slope = buildings_mask[!is.na(buildings_mask)])
nat = data.frame(cover = "Natural", 
slope = natural_mask[!is.na(natural_mask)])

slopes = rbind(nat, build)
head(slopes)

################
# Figure 09_04 #
################
ggplot(slopes, aes(x = slope)) +
	geom_histogram() +
	facet_grid(cover ~ .) +
	scale_x_continuous("Slope (radians)") +
	scale_y_continuous("Count") +
	theme_bw()
ggsave("C:\\Data\\4367OS_09_04a.png", width = 3.5, height = 5.5)
ggplot(slopes, aes(x = slope)) +
	geom_histogram() +
	facet_wrap(~ cover, ncol = 1) +
	scale_x_continuous("Slope (radians)") +
	scale_y_continuous("Count") +
	theme_bw()
ggsave("C:\\Data\\4367OS_09_04b.png", width = 3.5, height = 5.5)

ggplot(dat, aes(x = time, y = tmax)) + 
	geom_line()
ggsave("C:\\Data\\4367OS_09_01.png", width = 5.5, height = 3.5)

tmax_line = ggplot(dat, aes(x = time, y = tmax)) + 
	geom_line()

ggsave(plot = tmax_line, 
	filename = "C:\\Data\\4367OS_09_01.png", 
	width = 5.5, 
	height = 3.5) 

tmax1 = ggplot(dat, aes(x = time, y = tmax)) 
tmax2 = geom_line()
tmax1 + tmax2

county_f = fortify(county, region = "FIPS")
head(county_f)

colnames(county_f)[which(colnames(county_f) == "id")] = "FIPS"
county_f = join(county_f, county@data, "FIPS")
head(county_f)

states = getData("GADM", country = "USA", level = 1)
states = states[!(states$NAME_1 %in% c("Alaska", "Hawaii")), ]
states = spTransform(states, CRS(proj4string(county)))
states_f = fortify(states, region = "NAME_1")
head(states_f)

sp_minimal =
	theme_bw() +
	theme(axis.text = element_blank(),
	axis.title = element_blank(),
	axis.ticks = element_blank())

ggplot() + 
	geom_polygon(data = states_f, 
	aes(x = long, y = lat, group = group), 
		colour = "black", fill = NA) +
	coord_equal() +
	sp_minimal

################
# Figure 09_05 #
################
ggsave("C:\\Data\\4367OS_09_05.png", width = 5.5, height = 3.25)

ggplot() + 
	geom_polygon(data = county_f, 
		colour = NA, 
		aes(x = long, y = lat, group = group, fill = density)) +
	geom_polygon(data = states_f, 
		colour = "white", size = 0.25, fill = NA,
		aes(x = long, y = lat, group = group)) +
	scale_fill_gradientn(
		name = expression(paste("Density (",km^-2,")")), 
		colours = rainbow(7), 
		trans = "log10", 
		labels = as.character,
		breaks = 10^(-1:5)) +
	coord_equal() +
	sp_minimal

################
# Figure 09_06 #
################
ggsave("C:\\Data\\4367OS_09_06.png", width = 5.5, height = 3.25)

dem_hill = data.frame(coordinates(dem), 
	elev = dem[],
	hill = hill[])
dem_hill = dem_hill[complete.cases(dem_hill), ]
head(dem_hill)

################
# Figure 09_07 #
################
haifa_relief = ggplot(dem_hill, aes(x = x, y = y)) + 
	geom_raster(aes(fill = elev)) +
	scale_fill_gradientn("Elevation (m)", 
		colours = terrain.colors(10)) +
	coord_equal() + 
	theme_bw() +
	theme(axis.title = element_blank(),
		axis.text.y = element_text(angle = 90, hjust = 0.5))
haifa_relief
ggsave("C:\\Data\\4367OS_09_07a.png", width = 4.5, height = 5.5)
haifa_relief_shade = haifa_relief + 
	geom_raster(aes(alpha = hill), fill = "black") +
	scale_alpha_continuous("Hillshade", range = c(0.5, 0))
haifa_relief_shade
ggsave("C:\\Data\\4367OS_09_07b.png", width = 4.5, height = 5.5)

haifa_relief_shade + 
	geom_contour(aes(z = elev), colour = "black", alpha = 0.3)

################
# Figure 09_08 #
################
ggsave("C:\\Data\\4367OS_09_08.png", width = 4.5, height = 5.5)

haifa_buildings_ctr = gCentroid(haifa_buildings, byid = TRUE)
haifa_buildings_ctr = as.data.frame(haifa_buildings_ctr)
head(haifa_buildings_ctr)

haifa_relief_shade + 
	geom_contour(aes(z = elev), colour = "black", alpha = 0.3) +
	geom_point(data = haifa_buildings_ctr, size = 0.5) +
	geom_density2d(data = haifa_buildings_ctr, 
		aes(colour = ..level..)) +
	scale_colour_gradient("Density", low = "blue", high = "red") +
	scale_x_continuous(
    limits = c(min(haifa_buildings_ctr$x)-2000, 
		  max(haifa_buildings_ctr$x)+2000)) +
	scale_y_continuous(
    limits = c(min(haifa_buildings_ctr$y)-2000, 
      max(haifa_buildings_ctr$y)+2000))

################
# Figure 09_09 #
################
ggsave("C:\\Data\\4367OS_09_09.png", width = 5.5, height = 5.0)

ggplot(spainT, aes(x = x, y = y, fill = value)) +
	geom_raster() +
	scale_fill_gradient2(
    expression(paste("Value (", degree, "C)")), 
		low = "blue", high = "red", limits = c(-4,4)) +
	geom_contour(
		aes(z = value), 
		colour = "black", size = 0.1, breaks = c(-4:-1,1:4)) +
	coord_equal() +
	facet_grid(variable ~ year) +
	sp_minimal

################
# Figure 09_10 #
################
ggsave("C:\\Data\\4367OS_09_10.png", width = 6.5, height = 3)

buildings_geo = spTransform(buildings, 
	CRS("+proj=longlat +datum=WGS84"))
city_geo = spTransform(city, 
	CRS("+proj=longlat +datum=WGS84"))

buildings_ctr = coordinates(gCentroid(buildings_geo))
buildings_ctr

buildings_f = fortify(buildings_geo, region = "osm_id")
head(buildings_f)

colnames(buildings_f)[which(colnames(buildings_f) == "id")] = 
	"osm_id"
buildings_f = join(buildings_f, buildings@data, "osm_id")
head(buildings_f)

city_f = fortify(city_geo, region = "CTYUA13NM")
head(city_f)

city_map = get_map(location = buildings_ctr, 
	maptype = "hybrid", 
	zoom = 14)

library(scales)
ggmap(city_map) + 
	geom_polygon(data = buildings_f,
		aes(x = long, y = lat, group = group, 
      fill = dist_river), 
		size = 0.1, colour = "black") +
	geom_polygon(data = city_f, 
		aes(x = long, y = lat, group = group), 
		colour = "yellow", fill = NA) +
	scale_fill_gradient2("Distance\nto river (m)", 
		low = muted("blue"), high = muted("red"), 
    midpoint = 500) +
	labs(x = "Longitude", y = "Latitude")

################
# Figure 09_11 #
################
ggsave("C:\\Data\\4367OS_09_11.png", width = 5.5, height = 4.75)

towns_geo = spTransform(towns, 
	CRS("+proj=longlat +datum=WGS84"))
track_geo = spTransform(track, 
	CRS("+proj=longlat +datum=WGS84"))
forests_geo = spTransform(forests, 
	CRS("+proj=longlat +datum=WGS84"))

towns_f = data.frame(coordinates(towns_geo), name = towns_names)
towns_f
track_f = fortify(track_geo)
head(track_f)
forests_f = fortify(forests_geo, region = "name")
head(forests_f)

forests_ctr = coordinates(gCentroid(forests_geo))
forests_map = get_map(location = forests_ctr, 
	maptype = "satellite", 
	zoom = 12)

ggmap(forests_map) + 
	geom_polygon(data = forests_f, 
		aes(x = long, y = lat, group = group, colour = id), 
		fill = NA) +
	geom_path(data = track_f, 
		aes(x = long, y = lat), 
		colour = "yellow") +
	geom_text(data = towns_f, 
		aes(x = lon, y = lat, label = name), 
		colour = "white", size = 2.5, fontface = "bold") +
	scale_colour_discrete("Forest") +
	labs(x = "Longitude", y = "Latitude")

################
# Figure 09_12 #
################
ggsave("C:\\Data\\4367OS_09_12.png", width = 5.5, height = 4.75)

dem_df = as.data.frame(aggregate(dem, 5), xy = TRUE)
dem_df = dem_df[complete.cases(dem_df), ]
colnames(dem_df)[3] = "z"
head(dem_df)

x_range = diff(range(dem_df$x, na.rm = TRUE))
x_range
y_range = diff(range(dem_df$y, na.rm = TRUE))
y_range
z_range = diff(range(dem_df$z, na.rm = TRUE))
z_range

library(lattice)

################
# Figure 09_13 #
################
png("C:\\Data\\4367OS_09_13.png",width=5.5,height=5.5,units="in",res=300)
wireframe(z ~ x * y, 
	data = dem_df,
	drape = TRUE, 
	colorkey = TRUE,
	col.regions = terrain.colors(100),
	screen = list(z = 165, x = -60, y = 0),
	aspect = c(y_range/x_range, 7*(z_range/x_range)),
	zoom = 1.1)
dev.off()

s_df = as.data.frame(as.matrix(s))

colnames(s_df) = dates$date
s_df[1:5, 1:5]

s_df$coord = yFromRow(r)
s_df = melt(s_df, 
	id.vars = "coord", 
	variable.name = "date", 
	value.name = "ndvi")
head(s_df)

library(lubridate)
s_df$date = decimal_date(as.Date(s_df$date))
head(s_df)

################
# Figure 09_14 #
################
png("C:\\Data\\4367OS_09_14.png",width=5.5,height=3,units="in",res=300)
wireframe(ndvi ~ date * coord, 
	data = s_df, 
	drape = TRUE, 
	arrows = FALSE,
	col.regions = 
		colorRampPalette(c("darkred","white","darkblue"))(100),
	screen = list(z = 15, x = -55, y = 10),
	aspect = c(0.3, 0.2),
	panel.aspect = c(0.45),
	lty = 0,
	scales = list(arrows = FALSE, cex = 0.6),
	xlab = list("Time"),
	ylab = list("Y", cex = 0),
	zlab = list("NDVI", cex = 0),
	zoom = 0.95)
dev.off()

