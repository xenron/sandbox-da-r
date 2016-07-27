addresses = c(
	"2200 Sunport Blvd, Albuquerque, NM 87106, USA", 
	"7401 Paseo Del Volcan Northwest Albuquerque, NM 87121, USA",
	"121 Aviation Dr, Santa Fe, NM 87507, USA")

library(ggmap)
airports = geocode(addresses)
airports

airports$name = c("Albuquerque International",
	"Double Eagle II",
	"Santa Fe Municipal")
airports

library(sp)
coordinates(airports) = ~ lon + lat

class(airports)

airports

proj4string(airports) = CRS("+proj=longlat +datum=WGS84")

library(rgdal)
#writeOGR(airports, "C:\\Data", "airports", "ESRI Shapefile")

#airports = readOGR("C:\\Data", 
#	"airports", 
#	stringsAsFactors = FALSE)

track = readOGR("C:\\Data\\GPS_log.gpx", "tracks")

class(track)

county = readOGR("C:\\Data", "USA_2_GADM_fips", 
	stringsAsFactors = FALSE)

summary(airports)

proj4string(airports)

length(airports)

row.names(airports)

dimensions(airports)

nrow(county)
ncol(county)
dim(county)

county$NAME_2[1:10]

unique(county$TYPE_2)

airports@data

head(county@data)

str(airports)

airports_sp = as(airports, "SpatialPoints")

#airports_sp@data

as(airports_sp, "SpatialPointsDataFrame")@data

county = county[
	county$NAME_1 != "Alaska" &
	county$NAME_1 != "Hawaii", ]

county = county[
	!(county$NAME_1 %in% 
	c("Alaska", "Hawaii")), ]

county = county[county$TYPE_2 != "Water body", ]

################
# Figure 05_01 #
################
png("C:\\Data\\4367OS_05_01.png",width=5.5,height=4.5,units="in",res=300,pointsize=5)
plot(county)
dev.off()

newProj = CRS("+proj=laea +lat_0=45 +lon_0=-100 
	+x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")
county = spTransform(county, newProj)

################
# Figure 05_02 #
################
png("C:\\Data\\4367OS_05_02.png",width=5.5,height=4.5,units="in",res=300,pointsize=5)
plot(county)
dev.off()

library(raster)
l_03 = brick("C:\\Data\\landsat_11_09_2003.tif")

track = spTransform(track, CRS(proj4string(l_03)))

################
# Figure 05_03 #
################
png("C:\\Data\\4367OS_05_03.png",width=3.95,height=5.5,units="in",res=300)
plotRGB(l_03, r = 3, g = 2, b = 1, stretch = "lin", 
	ext = extent(track) + 10000)
plot(track, add = TRUE, col = "yellow")
dev.off()

coordinates(airports)

library(rgeos)
gArea(county) / 1000^2

county$area = gArea(county, byid = TRUE) / 1000^2

head(county@data)

county_nv_ut = county[county$NAME_1 %in% c("Nevada", "Utah"), ]

states = gUnaryUnion(county_nv_ut, id = county_nv_ut$NAME_1)

row.names(states)

################
# Figure 05_04 #
################
png("C:\\Data\\4367OS_05_04.png",width=5.5,height=5.5,units="in",res=300,pointsize=5)
plot(county_nv_ut, border = "lightgrey", lty = "dotted")
plot(states, add = TRUE)
county_ctr = gCentroid(county_nv_ut, byid = TRUE)
text(county_ctr, county_nv_ut$NAME_2, cex = 1.5)
dev.off()

airports = spTransform(airports, CRS(proj4string(county)))

nm = county[county$NAME_1 == "New Mexico", ]

################
# Figure 05_05 #
################
png("C:\\Data\\4367OS_05_05.png",width=5.5,height=5.5,units="in",res=300)
plot(nm)
plot(airports, col = "red", pch = 16, add = TRUE)
dev.off()

over(airports, nm)

cbind(airports@data, over(airports, nm))

################
# Figure 05_06 #
################
png("C:\\Data\\4367OS_05_06.png",width=5.5,height=5.5,units="in",res=300)
plot(nm[airports, ])
plot(airports, add = TRUE, col = "red", pch = 16, cex = 1.5)
text(airports, airports$name, pos = 1)
dev.off()

boundary = readOGR("C:\\Data", "CTYUA_DEC_2013_EW_BFE")

proj4string(boundary)

buildings = readOGR("C:\\Data", "london_buildings")
natural = readOGR("C:\\Data", "london_natural")

proj4string(buildings)
proj4string(natural)

buildings = spTransform(buildings, CRS(proj4string(boundary)))
natural = spTransform(natural, CRS(proj4string(boundary)))

city = boundary[boundary$CTYUA13NM == "City of London", ]

in_city = gContains(city, buildings, byid = TRUE)

class(in_city)

head(in_city)

buildings = buildings[in_city[,1], ]

river = natural[natural$type == "riverbank", ]

river = gUnaryUnion(river)

################
# Figure 05_07 #
################
png("C:\\Data\\4367OS_05_07.png",width=5.5,height=5.5,units="in",res=300,pointsize=5)
plot(buildings, col = "sandybrown")
plot(river, col = "lightblue", add = TRUE)
plot(boundary, border = "dimgrey", add = TRUE)
dev.off()

haifa_buildings = readOGR("C:\\Data", "haifa_buildings")
haifa_natural = readOGR("C:\\Data", "haifa_natural")

israel_adm = getData("GADM", country = "ISR", level = 1)

haifa_adm = israel_adm[israel_adm$NAME_1 == "Haifa", ]

haifa_adm = spTransform(haifa_adm, CRS(proj4string(l_03)))
haifa_buildings = 
	spTransform(haifa_buildings, CRS(proj4string(l_03)))
haifa_natural = 
	spTransform(haifa_natural, CRS(proj4string(l_03)))

################
# Figure 05_09 #
################
png("C:\\Data\\4367OS_05_09.png",width=5.5,height=5.5,units="in",res=300,pointsize=5)
plot(haifa_natural, col = "lightgreen")
plot(haifa_buildings, add = TRUE)
plot(haifa_adm, add = TRUE)
dev.off()

buildings_ch = gConvexHull(haifa_buildings)

buildings_ch = gIntersection(buildings_ch, haifa_adm)

haifa_natural = gUnaryUnion(haifa_natural)

haifa_natural = gIntersection(haifa_natural, buildings_ch)

buildings_50m = gBuffer(haifa_buildings, width = 50)

haifa_natural = gDifference(haifa_natural, buildings_50m)

################
# Figure 05_10 #
################
png("C:\\Data\\4367OS_05_10.png",width=5.5,height=5.5,units="in",res=300,pointsize=5)
plot(buildings_ch, col = "lightgrey", border = "lightgrey")
plot(haifa_adm, add = TRUE)
plot(haifa_natural, col = "lightgreen", add = TRUE)
plot(haifa_buildings, add = TRUE)
dev.off()

dist = gDistance(buildings, river, byid = TRUE)

class(dist)

dim(dist)

buildings$dist_river = dist[1, ]

head(buildings@data)

dat = read.csv("C:\\Data\\CO-EST2012-Alldata.csv")

colnames(dat)[1:15]

selected_cols = c("STATE", "COUNTY", "CENSUS2010POP")

dat = dat[, colnames(dat) %in% selected_cols]

head(dat)

colnames(dat) = tolower(colnames(dat))
colnames(dat)

dat = dat[dat$county != 0, ]

county$FIPS[1:10]

dat$state = formatC(dat$state, width = 2, flag = "0")
dat$county = formatC(dat$county, width = 3, flag = "0")

dat$state[1:10]
dat$county[1:10]

dat$FIPS = paste0(dat$state, dat$county)

dat$FIPS[1:10]

library(plyr)
county@data = join(county@data, 
	dat[, colnames(dat) %in% c("FIPS", "census2010pop")], 
	by = "FIPS")

head(county@data)

county@data[is.na(county$census2010pop), 
	c("NAME_1", "NAME_2")]

county$density = county$census2010pop / county$area

head(county@data)