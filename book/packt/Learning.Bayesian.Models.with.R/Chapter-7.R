###################################################################################################
#Codes used in Chapter 7 of book Learning Bayesian Models with R                                  #
#Author Hari M. Koduvely                                                                          #
###################################################################################################
#R packages required for this chapter
#All installations needs to be done only one time
install.packages("bgmm") 
install.packages("lda") 
install.packages("topicmodels")

#Loading required packages
library(bgmm)
library(lda)
library(tm)
library(topicmodels)

###################################################################################################
#Bayesian Mixture Model                                                                           #
###################################################################################################
#importing all the datasets in a folder and create a master data file
flist <- list.files(path = "C:/Users/Hari Koduvely/SkyDrive/Documents/Book - BMLR/Data/ADL_Dataset/HMP_Dataset/Brush_teeth", pattern = "*.txt")
setwd("C:/Users/Hari Koduvely/SkyDrive/Documents/Book - BMLR/Data/ADL_Dataset/HMP_Dataset/Brush_teeth")
all.data <- lapply(flist, read.table, sep = " ", header = FALSE)
combined.data <- as.data.frame(do.call(rbind, all.data))
combined.data.XY <- combined.data[,1:2]

df <- read.table("C:/Users/Hari Koduvely/SkyDrive/Documents/Book - BMLR/Data/seeds_dataset.txt", sep = "\t", header=FALSE)
df2 <- read.csv("C:/Users/Hari Koduvely/SkyDrive/Documents/Book - BMLR/Data/perfume_data.csv", sep = ",", header=FALSE)

#calling bgmm functions
unsupmod1 <- unsupervised(combined.data.XY, k=4)
plot.mModel(as.data.frame(unsupmod1))
Z <- as.data.frame(unsupmod1$tij)
plot.mModel(Z)

###################################################################################################
#Topic  Models                                                                                    #
###################################################################################################
#creation of training corpus from reuters data set
dirsourcetrain <- DirSource(directory = "C:/Users/Hari Koduvely/SkyDrive/Documents/Book - BMLR/Data/C50/C50train/AaronPressman")
xtrain <- VCorpus(dirsourcetrain)
xtrain <- tm_map(xtrain, stripWhitespace) #remove extra white space
xtrain <- tm_map(xtrain,content_transformer(tolower)) #chaning to lower case
xtrain <- tm_map(xtrain, removeWords, stopwords("english")) #removing stop words
xtrain <- tm_map(xtrain, stemDocument) #stemming the document
xtrain <-  as.data.frame.matrix(DocumentTermMatrix(xtrain)) #creating Document-Term Matrix

#creation of test set
dirsourcetest <- DirSource(directory = "C:/Users/Hari Koduvely/SkyDrive/Documents/Book - BMLR/Data/C50/MyTest/AaronPressman")
xtest <- VCorpus(dirsourcetest)
xtest <- tm_map(xtest, stripWhitespace) #remove extra white space
xtest <- tm_map(xtest,content_transformer(tolower)) #chaning to lower case
xtest <- tm_map(xtest, removeWords, stopwords("english")) #removing stop words
xtest <- tm_map(xtest, stemDocument) #stemming the document
xtest <-  as.data.frame.matrix(DocumentTermMatrix(xtest)) #creating Document-Term Matrix


#training lda model using package topic  models
ldamodel <- LDA(xtrain, 10, method = "VEM")
perp <- perplexity(ldamodel) #computation of perplexity, only with VEM method
perp
topicslist <- topics(ldamodel, 10, xtest)   #extracting topics from test data
postprob <- posterior(ldamodel,xtest)
postprob$topics
perp <- perplexity(ldamodel, xtest)

