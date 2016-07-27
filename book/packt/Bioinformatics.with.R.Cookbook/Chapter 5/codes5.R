################################################################################
### 1. Code to support the Recipe "Differential Expression Analysis" in chapter 5
### Requires:  limma, affy, antiProfilesData and affyPLM R libraries 
################################################################################

biocLite(c("limma", "antiProfilesData"))
library(affy) # Package for affy data handling
library(antiProfilesData) # Package containing input data
library(affyPLM) # Normalization package for eSet
library(limma) # limma analysis package
data(apColonData)
myData <- apColonData[, sampleNames(apColonData)[1:16]]
myData_quantile <- normalize.ExpressionSet.quantiles(myData,transfn="none")
design=model.matrix(~0 + pData(myData)$Status)
fit <- lmFit(myData_quantile, design)
fit
fitE <- eBayes(fit)
tested<-topTable(fitE, adjust="fdr", sort.by="B", number=Inf)
DE <- tested[tested$adj.P.Val < 0.01,]

################################################################################
### 2. Code to support the Recipe "Working with multiple groups in data" in chapter 5
### Requires:  leukemiasEset R library
################################################################################

library(leukemiasEset)
data(leukemiasEset)
eset <- leukemiasEset[, sampleNames(leukemiasEset)[c(1:3, 13:15, 25:27, 49:51)]]

design=model.matrix(~0 + factor(pData(eset)$LeukemiaType))
colnames(design) <- unique(as.character(pData(eset)$LeukemiaType))
fit <- lmFit(eset, design) 
contrast.matrix <- makeContrasts(NoL- ALL, NoL- AML, NoL- CLL,  levels = design) 
fit2 <- contrasts.fit(fit, contrast.matrix) 
fit2 <- eBayes(fit2) 
tested2<-topTable(fit2, adjust="fdr", sort.by="B", number=Inf, coef=1)
DE2 <- tested2[tested2$adj.P.Val < 0.01,]

################################################################################
### 3. Code to support the Recipe "Handling time series data" in chapter 5
### Requires:  Mfuzz R library
################################################################################
biocLite("Mfuzz")
library(Mfuzz)
library(affyPLM)
data(yeast)
plotDensity(yeast)
boxplot(yeast)
yeast.norm <- normalize.ExpressionSet.quantiles(yeast)
pData(yeast.norm)
times <- pData(yeast.norm)$time 
times <- as.factor(times)      
design = model.matrix(~0 + factor(pData(yeast.norm)$time))
colnames(design)[1:17]=c(paste("T", 0:16, sep=""))
cont=makeContrasts(T0-T1, T0-T2, T0-T3, T0-T4, T0-T5, T0-T6, T0-T7, T0-T8, T0-T9, T0-T10, T0-T11, T0-T12, T0-T13, T0-T14, T0-T15, T0-T16, levels=design) 
fit=lmFit(yeast.norm, cont)
fitE =eBayes(fit)
toptable(fitE, sort.by = 'logFC', coef=1)

T = topTable(fitE, coef=1, number=Inf)
fc=as.matrix(T[,"logFC"])
rownames(fc) = rownames(T)
for(con in 2:16){
	T = topTable(fitE, coef=con, number=Inf)
	fc = cbind(fc, as.matrix(T[match(rownames(fc), rownames(T)),"logFC"]))
}
plot(x=c(1:16),y=fc[1,], ylim=c(-4,4),type='l', xlab="time points", ylab="log fold change", main="Log fold changes compared to zeroth time point for 6 genes")
for(i in 2:6)
lines(fc[i,], col=i)
legend("topright", rownames(fc)[1:6], pch=1,col=1:6)


################################################################################
### 4. Code to support the Recipe "Functional enrichment of data" in chapter 5
### Requires:  GOstats R library
################################################################################

all_genes <-rownames(tested2)
sel_genes<-rownames(DE2)
library(biomaRt)
mart<- useDataset("hsapiens_gene_ensembl", useMart("ensembl"))
all_genes<- c(getBM(filters= "ensembl_gene_id", attributes= c("entrezgene"), values= all_genes, mart= mart))
sel_genes<- c(getBM(filters= "ensembl_gene_id", attributes= c("entrezgene"), values= sel_genes, mart= mart))

library(GOstats)
hgCutoff <- 0.05
params <- new("GOHyperGParams",  geneIds=sel_genes, universeGeneIds= all_genes, annotation="hgu95av2.db", ontology="BP", pvalueCutoff=hgCutoff, conditional=FALSE, testDirection="over") 
hgOver <-  hyperGTest(params)
summary(hgOver)
geneCounts(hgOver) 
plot(goDag(hgOver))
htmlReport(hgOver, file="ALL_hgco.html") 

################################################################################
### 5. Code to support the Recipe "Clustering microarray data" in chapter 5
### Requires:  EMA R library
################################################################################

c.data= exprs(eset[1:100, ])
library(EMA)
c.array= clustering(data=c.data, metric="pearson", method="ward")
plot(c.array)
c.gene= clustering(data=t(c.data), metric="pearson", method="ward") 
plot(c.gene)
################################################################################
### 6. Code to support the Recipe "Co-expression network from microarray data" in chapter 5
### Requires:  WGCNA R library
################################################################################

library(WGCNA)
myData_Sel = exprs(eset[rownames(DE2)[1:25],1:3 ])

myData_Sel <- t(myData_Sel)
myMat <- adjacency(myData_Sel, type="signed")
adjMat <- myMat
adjMat[abs(adjMat)>0.90] <-1
adjMat[abs(adjMat)<=0.90] <-0
diag(adjMat) <- 0
myGraph <- as(adjMat, "graphNEL")
myGraph
plot(myGraph, nodeAttrs=makeNodeAttrs(myGraph, fontsize=28, fillcolor="grey"))
#################################################################################
