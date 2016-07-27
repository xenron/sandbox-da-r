### loading packages

if (!require("ggplot2")) {
install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
}
if (!require("maps")) {
install.packages("maps", dependencies = TRUE)
library(maps)
}
if (!require("mapdata")) {
install.packages("mapdata", dependencies = TRUE)
library(mapdata)
}
if (!require("fields")) {
install.packages("fields", dependencies = TRUE)
library(fields)
}

pdf("chloropleth_maps.pdf", height = 7,width = 10)

### 1) average temperature USA

# 1.1) reading in and processing data
usa_map <- map_data(map = "state")

usa_temp <- read.csv("usa_temp.csv", comment.char = "#")

usa_data <- merge(usa_temp, usa_map, 
by.x ="state", by.y = "region") # case sensitive

usa_sorted <- usa_data[order(usa_data["order"]),]

# 1.2) plotting USA chloropleth maps
usa_map1 <- ggplot(data = usa_sorted) + 
	geom_polygon(aes(x = long, y = lat, 
group = group, fill = fahrenheit)) + 
	ggtitle("USA Map 1")
print(usa_map1)

usa_map2 <- usa_map1 + coord_map("polyconic") + 
	ggtitle("USA Map 2 - polyconic")
print(usa_map2)

usa_map3 <- usa_map2 + 
	geom_path(aes(x = long, y = lat, group = group), 
color = "black") + 
	ggtitle("USA Map 3 - black contours")
print(usa_map3)

usa_map4 <- usa_map3 + 
	scale_fill_gradient(low = "yellow", high = "red") + 
	ggtitle("USA Map 4 - gradient 1")
print(usa_map4)

usa_map5 <- usa_map3 + 
scale_fill_gradient2(low = "steelblue", mid = "yellow",
	high = "red",  midpoint = colMeans(usa_sorted["fahrenheit"])) + 
	ggtitle("USA Map 5 - gradient 2")
print(usa_map5)


### 2) South American population count

# 2.1) reading in and processing data
south_am_map <- map_data("worldHires", 
region = c("Argentina", "Bolivia", "Brazil",
	"Chile", "Colombia", "Ecuador", "Falkland Islands",
"French Guiana", "Guyana", "Paraguay", "Peru",
"Suriname", "Uruguay", "Venezuela"))

south_am_pop <- read.csv("south_america_pop.csv", 
comment.char = "#")

south_am_data <- merge(south_am_pop, south_am_map,  
	by.x = "country", by.y = "region")

south_am_sorted <- south_am_data[order(
south_am_data["order"]),]

# 2.2) creating chloropleth maps
south_am_map1 <- ggplot(data = south_am_sorted) + 
	geom_polygon(aes(x = long, y = lat, 
group = group, fill = population)) +
	geom_path(aes(x = long, y = lat, group = group), 
color = "black") + 
	coord_map("polyconic") + 
scale_fill_gradient(low = "lightyellow",
	high = "red", guide = "legend")
print(south_am_map1)

south_am_map2 = south_am_map1 + 
	theme(panel.background = element_blank(),
	axis.text = element_blank(), 
	axis.title = element_blank(), 
	axis.ticks = element_blank())
print(south_am_map2)

dev.off()
cat("Heat maps saved as ", getwd(),
"/chloropleth_maps.pdf", sep = "")

### 3) Volcano contour plot

pdf("contour_plot.pdf", height = 7,width = 10)

data(volcano)
image.plot(volcano)
contour(volcano, add = TRUE)

dev.off()

cat("Heat maps and contour plot saved as ", getwd(),
"/chloropleth_maps.pdf and", getwd(), "/contour_plot.pdf" , sep = "")

