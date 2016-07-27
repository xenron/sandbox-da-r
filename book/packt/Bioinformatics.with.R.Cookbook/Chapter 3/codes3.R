################################################################################
### 1. Code to support the Recipe "Getting sequence composition details" in chapter 3
### Requires:  Biostrings R library (also available as a separate file)
################################################################################

library(seqinr)
choosebank("genbank") 
query(listname = "actino", query="SP=Mycobacterium tuberculosis AND K=rpoB")
query(listname = "proteo", query="SP=Escherichia coli AND K=rpoB")
mynames.Actino<-getName(actino)
mynames.Proteo<-getName(proteo)
myActino <- getSequence(actino$req[[644]])
myProteo <- getSequence(proteo$req[[1]])
GC(myActino)
GC(myProteo)

################################################################################
### 2. Code to support the Recipe "Pairwise sequence alignment" in chapter 3
### Requires:  Biostrings R library (also available as a separate file)
################################################################################

permutest.seq <-function(seq1, seq2, n=1000, gapOp=2, gapExt=1, subMat="BLOSUM62", plot=FALSE){
	s=s2c(seq1)
	p=table(s)/length(s)
	aa=names(table(s))
	RandomScores <- c()
	for(i in 1:n){
		ranseq= c2s(sample(aa, length(s), rep=TRUE, prob=p))
		RandomScores[i] = pairwiseAlignment(seq2, ranseq, substitutionMatrix = subMat, gapOpening = gapOp, gapExtension = gapExt, scoreOnly = TRUE)  
		cat(".")
	}
myscore=pairwiseAlignment(seq1, seq2, substitutionMatrix = subMat, gapOpening = gapOp, gapExtension = gapExt, scoreOnly = FALSE)
pv=sum(RandomScores > score(myscore))/n
cat("P value is ")
cat(pv)
if(plot)
hist(RandomScores, xlab="Score for random sequences", main= "Histogram for Score of random sequences")
return(list(Pvalue=pv, alignment=myscore))
}
# sequence1 <- "HEWCM"
# sequence2 <- "HEECM"
# library(Biostrings)
# PV=permutest.seq(sequence1,sequence2, plot=TRUE)
################################################################################
### 3. Code to support the Recipe "Multiple sequence alignment" in chapter 3
### Requires: muscle R library
################################################################################
library(muscle)
mySeq<- muscle::read.fasta("fastaMSA.fasta")# Please assign the path to the file fastaMSA.fasta (In the code directory for the chapter)
MyMSA <- muscle(mySeq)
print(MyMSA, from=1, to=51)

# 5 sequences with 148 positions

# Position 1 to 148:

                        1                                                   51
sp|P24894|COX2_CAEEL    MNNFFQGYNLLFQHSLFASYMDWFHSFNCSLLLGVLV-FVTLLFGYLIFGT
sp|P00403|COX2_HUMAN    MAHAAQ---VGLQDATSPIMEELITFHDHALMIIFLICFLVLYALFLTLTT
sp|P26456|COX2_GORGO    MAHAAQ---VGLQDATSPIMEELITFHDHALMIIFLICFLVLYALFLTLTT
sp|Q38PR9|COX2_MAMPR    MAYPLQ---LGFQDATSPVMEELLHFHDHTLMIIFLISSLVLYIIMLMLTT
sp|Q9TA26|COX2_LOXAF    MAYPLQ---LGFQDATSPVMEELLHFHDHTLMIIFLISSLVLYIIMLMLTS
sp|P48890|COX2_FELCA    MAYPFQ---LGFQDATSPIMEELLHFHDHTLMIVFLISSLVLYIISLMLTT
sp|P68530|COX2_BOVIN    MAYPMQ---LGFQDATSPIMEELLHFHDHTLMIVFLISSLVLYIISLMLTT
sp|P48660|COX2_HORSE    MAYPFQ---LGFQDATSPIMEELLHFHDHTLMIVFLISSLVLYIISSMLTT
  sp|P00406|COX2_RAT    MAYPFQ---LGLQDATSPIMEELTNFHDHTLMIVFLISSLVLYIISLMLTT
sp|P00405|COX2_MOUSE    MAYPFQ---LGLQDATSPIMEELMNFHDHTLMIVFLISSLVLYIISLMLTT

################################################################################
### 4. Code to support the Recipe "Phylogenetic analysis in R" in chapter 3
### Requires: ape R package 
################################################################################
library(ape)
myset<-c("U15717","U15718", "U15719", "U15720","U15721","U15722","U15723","U15724")
myseqs<-read.GenBank(myset)
mydist<-dist.dna(myseqs)
myphylo<-triangMtd(mydist)
png("phylo.png",height=5.5, width=5.5,units="in",res=300, pointsize=10)
par(mfrow=c(2,2))
plot(myphylo, type="phylogram", edge.color="red", cex=1, edge.width=1,main="(A) Phylogram")
plot(myphylo, type="cladogram", edge.color="red", cex=1, edge.width=1, main="(B) Cladogram")
plot(myphylo, type="fan", edge.color="red", cex=1, edge.width=1, main="(C) Fan")
plot(myphylo, type="unrooted", edge.color="red", cex=1, edge.width=1, main="(D) Unrooted")
dev.off()
################################################################################
### 5. Code to support the Recipe "Pattern finding in sequence" in chapter 3
### Requires: Biostrings R package
################################################################################

myCodonFinder <- function(sequence){
			startCodon <-DNAString("ATG") # Assign start codons
			stopCodons <- list("TAA", "TAG", "TGA") # Assign stop codons
			codonPosition <- list() #initialize the output to be returned as a list
	codonPosition$Start <- matchPattern(startCodon, sequence) # search start codons
	x=list()
for(i in 1:3){  # iterate over all stop codons
x[[i]]= matchPattern(DNAString (stopCodons[[i]]), sequence)
codonPosition$Stop=x
 }
return(codonPosition) # returns results
}
mynucleotide <- DNAString("aacataatgcagtagaacccatgagccc")
myCodonFinder(mynucleotide)

