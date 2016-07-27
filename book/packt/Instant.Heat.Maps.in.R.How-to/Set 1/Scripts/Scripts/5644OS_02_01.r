# if you are running the script from a different location
# than the location of the data sets, uncomment the
# next line and point setwd() to the data set location
# setwd("/home/username/Datasets")

### loading packages
if (!require("gplots")) {
install.packages("gplots", dependencies = TRUE)
library(gplots)
}
if (!require("lattice")) {
install.packages("lattice", dependencies = TRUE)
library(lattice)
}
if (!require("xlsx")) {
install.packages("xlsx", dependencies = TRUE)
library(xlsx)
}

pdf("readingData.pdf")

### loading data and drawing heat maps

# 1) gene_expression.txt
gene_data <- read.table("gene_expression.txt",
	comment.char = "/", 
	blank.lines.skip = TRUE,
	header = TRUE,  
	sep = "\t", 
	nrows = 20)
gene_data <- data.matrix(gene_data)
gene_ratio <- outer(gene_data[,"Treatment"],
	gene_data[,"Control"], 
	FUN = "/")
heatmap.2(gene_ratio, 
	xlab = "Control", 
	ylab = "Treatment", 
	trace = "none",
	main = "gene_expression.txt")
cat("gene_expression.txt\n")
print(gene_ratio)

# 2) runners.csv
runner_data <- read.csv("runners.csv")
rownames(runner_data) <- runner_data[,1]
runner_data <- data.matrix(runner_data[,2:ncol(runner_data)])
colnames(runner_data) <- c(2003:2012)
runner_data[runner_data == 0.00] <- NA
heatmap.2(runner_data, 
	dendrogram = "none", 
	Colv = NA,
	Rowv = NA,
	trace = "none", 
	na.color = "gray",
	main = "runners.csv",
	margin = c(8,10))

cat("\n\nrunners.csv\n")
print(runner_data)

# 3) apple_stocks.xlsx
stocks_table <- read.xlsx("apple_stocks.xlsx", 
	sheetIndex = 1, 
	rowIndex = c(1:28), 
	colIndex = c(1:5,7))
row_names <- (stocks_table[,1])
stocks_matrix <- data.matrix(
	stocks_table[2:ncol(stocks_table)])
rownames(stocks_matrix) <- as.character(row_names)
stocks_data = t(stocks_matrix)
print(levelplot(stocks_data, 
	col.regions = heat.colors, 
	margin = c(10,10),  
	scales = list(x = list(rot = 90)), 
	main = "apple_stocks.xlsx",
	ylab = NULL,
	xlab = NULL))
cat("\n\napple_stocks.xlsx\n")
print(stocks_data)

dev.off()
cat("\n\nHeat maps saved as ", getwd(),
	"/reading_data.pdf", sep = "")
