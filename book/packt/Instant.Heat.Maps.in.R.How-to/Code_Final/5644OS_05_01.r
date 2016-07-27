if (!require("gplots")) {
install.packages("gplots", dependencies = TRUE)
library(gplots)
}
if (!require("RColorBrewer")) {
install.packages("RColorBrewer", dependencies = TRUE)
library(gplots)
}

### converting data
data(co2)
rowcolNames <- list(as.character(1959:1997), month.abb) 
co2_data <- matrix(co2,
	ncol = 12,
	byrow = TRUE,
	dimnames = rowcolNames)

heat2 <- function(...) 
	heatmap.2(co2_data,trace = "none", 
	density.info = "none", 
	dendrogram = "none",
	Colv = FALSE,
	Rowv = FALSE,
	col = colorRampPalette(c("blue", "yellow", "red"))(n = 100),
	margin = c(5,8),
	lhei = c(0.25,1.25),
	 ...)


png("1_PNG_default.png")

heat2(main = "PNG default")
dev.off()

png("2_PNG_highres.png", 
	width = 5*300, 
	height = 5*300, 
	res = 300, 
	pointsize = 8)
heat2(main = "PNG High Resolution")
dev.off()

jpeg("3_JPEG_highres.png",
	width = 5*300, 
	height = 5*300, 
	res = 300, 
	pointsize = 8)
heat2(main = "JPEG default")
dev.off()

bmp("4_BMP_default.bmp",
width = 5*300, 
	height = 5*300, 
	res = 300, 
	pointsize = 8)
heat2(main = "BMP default")
dev.off()

pdf("5_PDF_default.pdf",
	width = 5,
	height = 5,
	pointsize = 8)
heat2(main = "PDF default")
dev.off()

svg("6_SVG_default.svg",
	width = 5,
	height = 5,
	pointsize = 8)
heat2(main = "SVG default")
dev.off()

postscript("7_PostScript_default.ps",
	width = 5,
	height = 5,
	pointsize = 8)
heat2(main = "PostScript default")
dev.off()

png("8_PNG_transp.png", 
	width = 5*300, 
	height = 5*300, 
	res = 300, 
	pointsize = 8,
	bg = "transparent")
heat2(main = "PNG Transparent Background")
dev.off()

pdf("9_PDF_mono.pdf",
	family = "Courier",
	paper = "USr")
heat2(main = "PDF Monospace Font")
dev.off()

cat("Heat maps saved in ", getwd(),
	sep = "")


