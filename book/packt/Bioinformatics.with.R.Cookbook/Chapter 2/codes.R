################################################################################
### 1. Code to support the Recipe "Annotation databases" in chapter 2
### Requires: Biobase, annotate hgu133a.db R libraries available from Bioconductor
################################################################################

#source("http://bioconductor.org/biocLite.R") # Required only if not installed already
#biocLite(c("Biobase", "annotate", "hgu133a.db")) # Required only if not installed already
library(hgu133a.db) # loads the database library
myMap <- hgu133aENTREZID # Creates the map object for probes
mapped_probes <- mappedkeys(myMap) # gets probe ids from the map object
MyEntrez <- as.list(myMap[mapped_probes[1:5]]) # Accesses Entrez id for a set probes
if(length(MyEntrez) > 0){ # Returns the first available Entrez id for each probe if available (length>0)
	MyEntrez[[1]]
}


################################################################################
### 2. Code to support the Recipe "ID conversion" in chapter 2
### Requires: org.Hs.eg.db R libraries available from Bioconductor
################################################################################

#source("http://bioconductor.org/biocLite.R") # Required only if not installed already
#biocLite("org.Hs.eg.db") # Required only if not installed already
library(org.Hs.eg.db) # loads the  library
myEIDs <- c("1", "10", "100", "1000", "37690") # Create vecotor of input Entrez IDs
mySymbols <- mget(myEIDs, org.Hs.egSYMBOL, ifnotfound=NA) # gets gene symbols 
mySymbols <-unlist(mySymbols)
mySymbols <- mget(myEIDs, org.Hs.egSYMBOL, ifnotfound=NA) # gets gene symbols 
mySymbols <-unlist(mySymbols)[!is.na(unlist(mySymbols))] # Removes the symbol with value NA
myEIDs <- unlist(mget(mySymbols, org.Hs.egSYMBOL2EG)) # gets Entrez IDs for input gene symbols

################################################################################
### 3. Code to support the Recipe "The KEGG annotation of Genes" in chapter 2
### Requires: KEGG.db R libraries available from Bioconductor
################################################################################

#source("http://bioconductor.org/biocLite.R") # Required only if not installed already
#biocLite("KEGG.db") # Required only if not installed already
library(KEGG.db) # loads the  library
myEIDs <- c("1109", "6718") # Create vecotor of input Entrez IDs
kegg <- as.character(unlist(mget(as.character(myEIDs), KEGGEXTID2PATHID, ifnotfound=NA)))
kegg <- sapply(strsplit(kegg, "hsa"), "[[", 2)
myPath <- unlist(mget(kegg, KEGGPATHID2NAME, ifnotfound=list(NA)))
> myPath
## Results
#                                         00120 
#              "Primary bile acid biosynthesis" 
#                                         00140 
#                "Steroid hormone biosynthesis" 
#                                         00980 
#"Metabolism of xenobiotics by cytochrome P450" 
#                                         01100 
#                          "Metabolic pathways" 
#                                         00120 
#              "Primary bile acid biosynthesis" 
#                                         00140 
#                "Steroid hormone biosynthesis" 
#                                         01100 
#                          "Metabolic pathways" 
> KEGGPATHID2EXTID$hsa00140
## Results
# [1] "100510686" "10720"     "10941"     "1109"      "1312"      "1543"     
# [7] "1545"      "1551"      "1576"      "1577"      "1581"      "1583"     
#[13] "1584"      "1585"      "1586"      "1588"      "1589"      "1645"     
#[19] "1646"      "3283"      "3284"      "3290"      "3291"      "3292"     
#[25] "3293"      "3294"      "412"       "51144"     "51478"     "54490"    
#[31] "54575"     "54576"     "54577"     "54578"     "54579"     "54600"    
#[37] "54657"     "54658"     "54659"     "574537"    "64816"     "6715"     
#[43] "6716"      "6718"      "6783"      "6820"      "7363"      "7364"     
#[49] "7365"      "7366"      "7367"      "7923"      "79644"     "79799"    
#[55] "8630"      "8644"      "9420"     
> KEGGPATHID2EXTID$sce00100
## Results
# [1] "YCR048W" "YGL001C" "YGL012W" "YGR060W" "YGR175C" "YHR007C" "YHR072W"
# [8] "YHR190W" "YLR056W" "YLR100W" "YML008C" "YMR015C" "YMR202W" "YNL280C"
# [15] "YNR019W"
biocLite(KEGGREST)
library(KEGGREST)
genes <- keggGet("hsa00140")
genes

################################################################################
### 4. Code to support the Recipe "The GO annotation of Genes" in chapter 2
### Requires: org.Hs.eg.db and GO.db R libraries available from Bioconductor
################################################################################
#source("http://bioconductor.org/biocLite.R") # Required only if not installed already
#biocLite(c("org.Hs.eg.db", "GO.db")) # Required only if not installed already
library(org.Hs.eg.db) # loads the library
myEIDs <- c("1", "10", "100") # Create vecotor of input Entrez IDs
myGO <- unlist(org.Hs.egGO[[as.character(myEIDs[1])]])
myGO_All <- mget(myEIDs, org.Hs.egGO)
GOgenes <- org.Hs.egGO2ALLEGS[["GO:0008150"]]
GOgenes_All <- mget("GO:0008150", org.Hs.egGO2ALLEGS)

################################################################################
### 5. Code to support the Recipe "The GO enrichment of Genes" in chapter 2
### Requires: topGO and ALL R libraries available from Bioconductor
################################################################################
#source("http://bioconductor.org/biocLite.R") # Required only if not installed already
#biocLite(c("topGO", "ALL")) # Required only if not installed already
library(topGO) # loads the data library
library(ALL) # loads the data library
data(ALL)
data(geneList)
affyLib <-paste(annotation(ALL), "db", sep=".")
library(package=affyLib, character.only=TRUE)
myGOData <- new("topGOdata", ontology="BP", allGenes=geneList, geneSel=topDiffGenes, nodeSize=10, annot= annFUN.db, affyLib=affyLib)
Myenrichment_Fisher <- runTest(myGOData, algorithm= "classic", statistic="fisher")
Myenrichment_Fisher
## Results
#Description:  
#Ontology: BP 
#'classic' algorithm with the 'fisher' test
#893 GO terms scored: 42 terms with p < 0.01
#Annotation data:
#    Annotated genes: 314 
#    Significant genes: 50 
#    Min. no. of genes annotated to a GO: 10 
#    Nontrivial nodes: 801 

score(Myenrichment_Fisher) # Displays p values for every GO term
geneData(Myenrichment_Fisher) # A table showing Medata data for enrichment
## Results
#  Annotated Significant    NodeSize    SigTerms 
#        315          50          10         776 

Myenrichment_KS <- runTest(myGOData, algorithm= "classic", statistic="ks")
enrich_table <-GenTable(myGOData, classicFisher=Myenrichment_Fisher,topNodes = 20)
head(enrich_table) # get the enrichment results as table
## Results
#       GO.ID                                        Term Annotated Significant
#1 GO:0001906                                cell killing        11           7
#2 GO:0001909             leukocyte mediated cytotoxicity        11           7
#3 GO:0002252                     immune effector process        33          13
#4 GO:0032946 positive regulation of mononuclear cell ...        33          13
#5 GO:0050671 positive regulation of lymphocyte prolif...        33          13
#6 GO:0070665 positive regulation of leukocyte prolife...        33          13
#  Expected classicFisher
#1     1.75       0.00035
#2     1.75       0.00035
#3     5.25       0.00047
#4     5.25       0.00047
#5     5.25       0.00047
#6     5.25       0.00047

showSigOfNodes(myGOData, score(Myenrichment_Fisher), firstSigNodes=5, useInfo="all") # Plot the enrichment GO graph
gostat <- termStat(myGOData, names(score(Myenrichment_Fisher)))
plot(score(Myenrichment_Fisher), score(Myenrichment_KS)[names(score(Myenrichment_Fisher))], xlab="P values Fisher test", ylab="P values KS test", cex=(gostat$Annotated/max(gostat$Annotated))*4, col=heat.colors(gostat$Significant))
print(showGroupDensity(myGOData, enrich_table[1, "GO.ID"], ranks=TRUE))
################################################################################
### 6. Code to support the Recipe "The KEGG enrichment of Genes" in chapter 2
### Requires: clusterProfiler R libraries available from Bioconductor
################################################################################
#source("http://bioconductor.org/biocLite.R") # Required only if not installed already
#biocLite("clusterProfiler") # Required only if not installed already
library(clusterProfiler)
data(gcSample)
genes <-gcSample[[3]]
kegg_enrichment=enrichKEGG(genes, pvalueCutoff = 0.01)
summary(kegg_enrichment)
## Results
#               ID                            Description GeneRatio  BgRatio
#hsa04512 hsa04512               ECM-receptor interaction     9/152  85/5894
#hsa04974 hsa04974       Protein digestion and absorption     8/152  81/5894
#hsa04060 hsa04060 Cytokine-cytokine receptor interaction    16/152 265/5894
#               pvalue    p.adjust      qvalue
#hsa04512 0.0003077198 0.005846677 0.001943494
#hsa04974 0.0010627953 0.007753160 0.002577228
#hsa04060 0.0012241832 0.007753160 0.002577228
#                                                                                  geneID
#hsa04512                                    7057/3339/3695/1101/3679/3910/3696/1302/3693
#hsa04974                                         477/4311/1308/1299/5646/1302/23428/7373
#hsa04060 2919/4982/3977/6375/8200/608/8792/3568/3082/2057/1438/8718/655/50615/51561/7042
#         Count
#hsa04512     9
#hsa04974     8
#hsa04060    16

plot(kegg_enrichment)





