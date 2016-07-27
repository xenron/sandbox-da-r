################################################################################
### 1. Code to support the Recipe "Pairwise sequence alignment" in chapter 3
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
# PV=permutest.seq(sequence1,sequence2, plot=TRUE)
