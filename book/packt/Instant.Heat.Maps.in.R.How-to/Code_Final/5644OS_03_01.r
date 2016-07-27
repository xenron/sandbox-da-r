### loading packages
if (!require("gplots")) {
install.packages("gplots", dependencies = TRUE)
library(RColorBrewer)
}
if (!require("RColorBrewer")) {
install.packages("RColorBrewer", dependencies = TRUE)
library(RColorBrewer)
}

### reading in data
gene_data <- read.csv("arabidopsis_genes.csv")
row_names <- gene_data[,1]
gene_data <- data.matrix(gene_data[,2:ncol(gene_data)])
rownames(gene_data) <- row_names
cat("\n\narabidopsis_genes.csv\n")
print(gene_data)

### setting heatmap.2() default parameters
heat2 <- function(...) heatmap.2(gene_data, 
	tracecol = "black",
	dendrogram = "column", 
	Rowv = NA, 
	trace = "none", 
	margins = c(8,10), 
	density.info = "density", ...)

pdf("custom_heatmaps.pdf")

### 1) customizing colors
# 1.1) in-built color palettes
heat2(col = terrain.colors(n = 1000), 
	main = "1.1) Terrain Colors") 

# 1.2) RColorBrewer palettes
heat2(col = brewer.pal(n = 9, "YlOrRd"),
	main = "1.2) Brewer Palette")

# 1.3) creating own color palettes
my_colors <- c(y1 = "#F7F7D0", 
	y2 = "#FCFC3A", 
	y3 = "#D4D40D", 
	b1 = "#40EDEA",
	b2 = "#18B3F0", 
	b3 = "#186BF0", 
	r1 = "#FA8E8E",
	r2 = "#F26666", 
	r1 = "#C70404")
heat2(col = my_colors, 
	main = "1.3) Own Color Palette")
my_palette <- colorRampPalette(c("blue", "yellow", "red"))(n = 1000)
heat2(col = my_palette, main = "1.3) ColorRampPalette")

# 1.4) gray scale
heat2(col = gray(level = (0:100)/100),
	main ="1.4) Gray Scale")

### 2) adding cell notes
fold_change <- 2^gene_data
rounded_fold_changes  <- round(fold_change, 2)
heat2(cellnote = rounded_fold_changes, 
	notecex = 0.5, 
	notecol = "black", 
	col = my_palette,
	main = "2) Cell Notes")
cat("\n\narabidopsis_genes.csv Fold changes\n")

### 3) adding column side colors
heat2(ColSideColors = c("red", "gray", "red", rep("green", 13)),
	main = "3) ColSideColors")

dev.off()
cat("\n\nHeat maps saved as ", getwd(),
	"/custom_heatmaps.pdf", sep = "")