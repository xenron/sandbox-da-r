################################################################################
### 1. Code to support the Recipe "Working with PubMed in R" in chapter 1
### Requires: RISmed R package available from CRAN
################################################################################
################################################################################
#**************************     1.START    ************************************#
################################################################################
install.packages("RISmed") # Install the package if not already installed 
library("RISmed") # load the RISmed library
data(myeloma) # Loads the package data on search for myloma
# str(myeloma) 
AbstractText(myeloma) # Gives the abstracts of the 5 entries in tyhe myeloma data
Author(myeloma) # Gives the names of authors of the 5 entries in tyhe myeloma data
ArticleTitle(myeloma) # Gives the article title of the 5 entries in tyhe myeloma data
Title(myeloma) # Gives the journal title of the 5 entries in tyhe myeloma data
PMID(myeloma) # Gives the PMIDs of the 5 entries in tyhe myeloma data
# To create your own query on cancer first create a EUtilsSummary object
cancer <- EUtilsSummary("cancer[ti]", type="esearch", db="pubmed") # The created object carries information on the hits found for search
class(cancer)
# [1] "EUtilsSummary"
# attr(,"package")
# [1] "RISmed"

cancer.ris <- EUtilsGet(cancer, type="efetch", db="pubmed") # This creates Medline object for our search on cancer carrying fetched abstracts and other details 
class(cancer.ris)
# [1] "Medline"
# attr(,"package")
# [1] "RISmed"
AbstractText(cancer.ris)[1:3] # Gives the abstracts of the 3 entries in the cancer.ris data
Author(cancer.ris)[1:3] # Gives the names of authors of the 3 entries in the cancer.ris data
ArticleTitle(cancer.ris)[1:3] # Gives the article title of the 3 entries in the cancer.ris data
Title(cancer.ris)[1:3] # Gives the journal title of the 3 entries in the cancer.ris data
PMID(cancer.ris)[1:3] # Gives the PMIDs of the 3 entries in the cancer.ris data
### Further instructions 
# * please use the corresponding help functions to know about the use of other argument
# * Several other constrains are possible for search type in ?EUtilsGet or ?EUtilsSummary
#   in your R terminal to know about them
# * Please make sure that your machine/computer in connected to the internet
# * Avoid fetching huge number of hits to avoid overloading
################################################################################
#**************************      1.END     ************************************#
################################################################################

################################################################################
### 2. Code to support the Recipe "Retrieving data-Biomart" in chapter 1
### Requires: biomaRt R package available from Bioconductor
################################################################################
################################################################################
#**************************     2.START    ************************************#
################################################################################
source("http://bioconductor.org/biocLite.R") #Load the biocLite function from Bioconductor
biocLite("biomaRt") # Install the biomaRt library
library(biomaRt) # Load the biomaRt library
mart <- useMart(biomart="ensembl", dataset="hsapiens_gene_ensembl") # Set the mart
myResults <- getBM(attribute= c("hgnc_symbol"), mart=mart) # Fetch the genes (attributes) from BioMart
N <- 50
mysample <- sample(myResults$hgnc_symbol, N) # Randomy sample 50 (N) genes
head(mysample) # First six genes
# [1] "NOXRED1" "NOTCH2"  "SAMD7"   "CEP120"  "SNRPD3"  "TTC14"  
seq1 <- getSequence(id="BRCA1", type="hgnc_symbol", seqType="peptide", mart=mart) # Fetch peptide sequence for all BRCA1 entries in the database
show(seq1) # Displays the sequence
seq2 <- getSequence(id="ENST00000520540", type='ensembl_transcript_id',seqType='gene_flank',upstream = 30,mart = mart)# Fetches sequence 20 upstream position for 1939_at (1:20 nucleotides)
show(seq2) # Displays the sequence
mart <- useMart(biomart="ensembl", dataset="hsapiens_gene_ensembl") # Set the mart
geneList <- read.csv("myList.csv", head=TRUE, sep="\t") # Read the file with refseq ids provided with code files
results <- getBM(attributes =c("refseq_mrna", "hgnc_symbol"), filters="refseq_mrna", values=geneList[,2], mart=mart) # Get the gene symbol for refseq ids
results # Display the results

################################################################################
#**************************      2.END     ************************************#
################################################################################

