################################################################################
### 1. Code to support the Recipe "HWE from SNP files" in chapter 6
### Requires:  SNPassoc and GWASExactHW R libraries 
################################################################################
freqCalc <- function(d, pat){
freq=data.frame() # initialize a data.frame
x=c() # initialize an empty vector counting for NAs
for (i in 1:ncol(d)){ # iterate over the data for each snp
	t= as.numeric(summary(d[,i]))# get value for each SNP
	freq=rbind(freq, t)# append to the data.frame
	if(any(is.na(d[,i]))| length(t)<3){ # removes NA values 
		x=c(x,i)
	}
}

rownames(freq)=colnames(SNPs)
colnames(freq)= c("nAA", "nAa", "naa")
freq=freq[-x,]
freq=freq[, c("nAA", "nAa", "naa")]
return(freq)
vn=c()
for(p in 1:length(pat)){
vn=c(vn, grep(pat[p], rownames(freq)))
}
freq=freq[vn, c("nAA", "nAa", "naa")]
return(freq)
}
# How to run
# library(SNPassoc)
# data(SNPs)
# library(GWASExactHW)
# freq2 <- freqCalc(SNPs, pat="snp")
# myTestsnp=HWExact(freq2)

#plot(-log10(myTestsnp), type="b", ylab="-log10(p-value)", main="HWE p-values for SNPS")
#abline(h=-log10(0.05), col="red")
#sum(myTestsnp <0.05)
#rownames(freq)[which(myTestsnp < 0.05)]
###################################################

