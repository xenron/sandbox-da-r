if (!require("gplots")) {
install.packages("gplots", dependencies = TRUE)
library(gplots)
}
if (!require("MASS")) {
install.packages("MASS", dependencies = TRUE)
library(MASS)
}
if (!require("RColorBrewer")) {
install.packages("RColorBrewer", dependencies = TRUE)
library(RColorBrewer)
}

# Writing out matrix file
data(mtcars)
car_data <- mtcars[,1:7]
write.matrix(car_data, "car_data.csv", sep = ",")
norm_cars <- scale(car_data) # automatically matrix

# Creating heat map
svg("car_heatmap.svg")
heatmap.2(norm_cars, 
	density.info = "none",
	trace = "none",
	dendrogram = "none",
	Rowv = FALSE, 
	Colv = FALSE,
	col = colorRampPalette(c("red", "yellow", "green"))(n = 75),
	margin = c(5,10))
	

dev.off()

cat("car_data.csv and car_heatmap.svg saved in ", getwd(),
	sep = "")