### loading packages
if (!require("gplots")) {
install.packages("gplots", dependencies = TRUE)
library(gplots)
}
if (!require("lattice")) {
install.packages("lattice", dependencies = TRUE)
library(lattice)
}

### loading data
data(AirPassengers)
cat("\nAirPassengers data:\n")
print(AirPassengers)     
cat("\nData type of AirPassengers:", 
	class(AirPassengers), "\n\n")

### converting data
rowcolNames <- list(as.character(1949:1960), month.abb)
air_data <- matrix(AirPassengers, 
	ncol = 12, 
	byrow = TRUE, 
	dimnames = rowcolNames)
cat("\nair_data:\n") 
print(air_data)
cat("\nData type of air_data:", 
	class(air_data), "\n\n")

### drawing heat maps
pdf("firstHeatmaps.pdf")

# 1) Air Passengers #1
print(levelplot(air_data,
	col.regions=heat.colors, 
	xlab = "year",
	ylab = "month", 
	main = "Air Passengers #1"))

# 2) Air Passengers #2
heatmap.2(air_data, 
	trace = "none", 
	density.info = "none", 
	xlab = "month", 
	ylab = "year", 
	main = "Air Passengers #2")

# 3) Air Passengers #3
heatmap.2(air_data, 
	trace = "none", 
	xlab = "month", 
	ylab = "year", 
	main = "Air Passengers #3",
	density.info = "histogram",
	dendrogram = "column",
	keysize = 1.8)

dev.off()
cat("Heat maps saved as ", getwd(),
	"/first_heatmaps.pdf", sep = "")
