################################################################################
### 1. Code to support the Recipe "Retrieving a sequence from UniProt" in chapter 4
### 
################################################################################

library(biomaRt) 
myMart <- useMart("unimart", dataset="uniprot") 
myMart
listAttributes(myMart) 
myProt <- getBM(attributes=c("pdb_id","protein_name", "ensembl_id","go_id", "name"), filter="accession", values="O95544", mart=myMart) 
myProt 
ensemblMart <- useMart(biomart = "ensembl", dataset = "hsapiens_gene_ensembl") 
myProtein <- biomaRt::getSequence(id=myProt$ensembl_id[1], type="ensembl_gene_id", seqType = "peptide", mart=ensemblMart) 
myProtein$peptide[1] 
#source("http://bioconductor.org/biocLite.R")
biocLite("UniProt.ws") 
library(UniProt.ws)
availableUniprotSpecies(pattern="sapiens") 
taxId(UniProt.ws) <- 9606 
head(keytypes(UniProt.ws)) 
head(cols(UniProt.ws)) 
keys <- "O95544" 
cols <- c("PDB", "UNIPROTKB", "SEQUENCE") 
kt <- "UNIPROTKB" 
myProtein2 <- select(UniProt.ws, keys, cols, kt) 
myProtein2$SEQUENCE 
################################################################################
### 2. Code to support the Recipe "Protein sequence analysis" in chapter 4
### 
################################################################################

install.packages("bio3d_2.0-1.tar.gz", repos=NULL, type="source") 
library(bio3d) 
pdb1 <- read.pdb("1BG2") 
pdb2 <- read.pdb("2VVG") 
pdb3 <- read.pdb("1MKJ") 
s1 <- aa321(pdb1$seqres) 
s2 <- aa321(pdb2$seqres) 
s3 <- aa321(pdb3$seqres) 
raw <- seqbind(seqbind(s1, s2),s3) 
aln <- seqaln(raw, id=c("1BG2","2VVG ","1MKJ ")) 
aln2html(aln, append=FALSE, file="Myalign.html") 
################################################################################
### 3. Code to support the Recipe "computing features of a protein sequence" in chapter 4
### 
################################################################################

install.packages("protr") 
library(bio3d) 
library(protr) 
pdb1 <- read.pdb("1BG2") 
s1 <- aa321(pdb1$seqres) 
s1 <- paste(s1, sep="",collapse="") 
library(seqinr) 
s1 <- c2s(s1) 
extractAAC(s1) 
extractAPAAC(s1, props = c("Hydrophobicity", "Hydrophilicity"), lambda = 30, w = 0.05, customprops = NULL)
extractCTDC(s1) 
extractCTDD(s1) 
extractDC(s1) 
################################################################################
### 4. Code to support the Recipe "Handling the PDB file" in chapter 4
### 
################################################################################

library(protr) 
library(bio3d) 
pdb <- read.pdb("1BG2") 
class(pdb) 
attributes(pdb) 
head(pdb) 
head(pdb$atom[, c("x","y","z")]) 
head(pdb$atom[pdb$calpha, c("resid", "elety", "x","y","z")]) 
aa321(pdb$seqres) 
write.pdb(pdb, file="myPDBfile.pdb") 
read.pdb("myPDBfile.pdb")
################################################################################
### 5. Code to support the Recipe "Working with the InterPro domain annotation" in chapter 4
### 
################################################################################

library(biomaRt) 
myMart <- useMart("ensembl", dataset="hsapiens_gene_ensembl") 
ensemblID <- "ENSG00000008130"
unip <- useDataset("uniprot",mart=useMart("unimart"))
interpro <- getBM(attributes=c("interpro_id","go_id"),filters="ensembl_id", values="ENSG00000008130",mart=unip) 
head(interpro) 
################################################################################
### 6. Code to support the Recipe "Understanding the Ramchandran plot" in chapter 4
### 
################################################################################

library(bio3d) 
pdb <- read.pdb("1BG2") 
tor <- torsion.pdb(pdb) 
plot(tor$phi, tor$psi, main="(A) Ramchandran Plot 1BG2") 
scatter.psi <- tor$psi 
scatter.phi <- tor$phi 
library(RColorBrewer) # load RColourBrewer package 
k <- 10 # define number of colours 
my.cols <- rev(brewer.pal(k, "RdYlBu")) # Brew color pallette
smoothScatter(x=scatter_phi, y=scatter_psi, colramp=colorRampPalette(my.cols), xlim=c(-180,180), ylim=c(- 180,180),xlab="Phi", ylab="Psi", main="(B) Ramchandran Plot 1BG2", pch=19, cex=0.00) 
################################################################################
### 7. Code to support the Recipe "Searching for similar proteins" in chapter 4
### 
################################################################################

library(bio3d) 
pdb <- read.pdb("1BG2") 
mySeq <- aa321(pdb$seqres) 
MyBlast <- blast.pdb( seq.pdb(pdb) ) 
head(MyBlast$hit.tbl)
top.hits <- plot(MyBlast) 
head(top.hits$hits) 
################################################################################
### 8. Code to support the Recipe "Working with the secondary structure features of proteins" in chapter 4
### 
################################################################################

library(bio3d) 
protein <- read.pdb("1BG2") 
SS <- dssp(pdb) 
head(SS)
################################################################################
### 8. Code to support the Recipe "Visualizing protein structures" in chapter 4
### 
################################################################################

install.packages("Rknots") 
myprotein <- loadProtein("1BG2") 
plotDiagram(myprotein$A, ends = c(), lwd = 2.5)
ramp <- colorRamp(c('blue', ' white', ' red' )) 
pal <- rgb(ramp(seq(0,1,length=100)), max=255) 
plotKnot3D(myprotein$A, colors=list(pal), lwd=8, radius=0.4, showNC=TRUE, text=FALSE) 
