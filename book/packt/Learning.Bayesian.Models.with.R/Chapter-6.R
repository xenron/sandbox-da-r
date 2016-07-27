###################################################################################################
#Codes used in Chapter 6 of book Learning Bayesian Models with R                                  #
#Author Hari M. Koduvely                                                                          #
###################################################################################################
#R packages required for this chapter
#All installations needs to be done only one time
install.packages("tm")    
install.packages("e1071")  
install.packages("SnowballC")
install.packages("pROC")
install.packages("BayesLogit") 

#loading required packages
library(tm)
library(e1071)
library(SnowballC)
library(pROC)
library(BayesLogit)

###################################################################################################
#Naive Bayes Model                                                                                #
###################################################################################################
spamdata <- read.table("/Users/harikoduvely/Projects/Book/Data/smsspamcollection/SMSSpamCollection", sep ="\t",stringsAsFactors = default.stringsAsFactors())

#Splitting data into training and test set in the ratio 80:20
set.seed(123)

samp <- sample.int(nrow(spamdata),as.integer(nrow(spamdata)*0.2),replace=F)
spamTest <- spamdata[samp,]
spamTrain <- spamdata[-samp,]
ytrain <- as.factor(spamTrain[,1])
ytest <- as.factor(spamTest[,1])
xtrain <- as.vector(spamTrain[,2])
xtest <- as.vector(spamTest[,2])

#Data Preprocessing
xtrain <- VCorpus(VectorSource(xtrain))
xtrain <- tm_map(xtrain, stripWhitespace) #remove extra white space
xtrain <- tm_map(xtrain, removePunctuation) #remove punctuation
xtrain <- tm_map(xtrain, removeNumbers) #remove numbers
xtrain <- tm_map(xtrain,content_transformer(tolower)) #changing to lower case
xtrain <- tm_map(xtrain, removeWords, stopwords("english")) #removing stop words
xtrain <- tm_map(xtrain, stemDocument) #stemming the document
xtrain <- DocumentTermMatrix(xtrain) #creating Document-Term Matrix
xtrain <-  as.data.frame.matrix(xtrain) 

xtest <- VCorpus(VectorSource(xtest))
xtest <- tm_map(xtest, stripWhitespace) #remove extra white space
xtest <- tm_map(xtest, removePunctuation) #remove punctuation
xtest <- tm_map(xtest, removeNumbers) #remove numbers
xtest <- tm_map(xtest,content_transformer(tolower)) #chaning to lower case
xtest <- tm_map(xtest, removeWords, stopwords("english")) #removing stop words
xtest <- tm_map(xtest, stemDocument)
xtest <-  as.data.frame.matrix(DocumentTermMatrix(xtest)) #creating Document-Term Matrix

#Training the Naive Bayes Model
nbmodel <- naiveBayes(xtrain,ytrain, laplace=3)

#Prediction using trained model
ypred.nb <- predict(nbmodel, xtest, type = "class", threshold = 0.075)

#Converting classes to 0 and 1 for plotting ROC
fconvert <- function(x){
  if(x == "spam"){ y <- 1}
  else {y <- 0}
  y
}

ytest1 <- sapply(ytest, fconvert, simplify = "array")
ypred1 <- sapply(ypred.nb, fconvert, simplify = "array")

roc(ytest1, ypred1, plot = T)

#Confusion matrix
confmat <- table(ytest,ypred.nb)
confmat

tab <- nbmodel$tables
fham <- function(x){
  y <- x[1,1]
  y
}
hamvec <- sapply(tab, fham, simplify = "array")
hamvec <- sort(hamvec, decreasing = T)

fspam <- function(x){
  y <- x[2,1]
  y
}
spamvec <- sapply(tab, fspam, simplify = "array")
spamvec <- sort(spamvec, decreasing = T)

prb <- cbind(spamvec, hamvec)
print.table(prb, max = 20)

##########################################################################################################
#Bayesian Logistic Regression                                                                            #
##########################################################################################################
set.seed(123)

PDdata <- read.table("/Users/harikoduvely/Projects/Book/Data/parkinsons.csv", sep=",", header=TRUE, row.names = 1)
rnames <- row.names(PDdata)
cnames <- colnames(PDdata, do.NULL = TRUE, prefix = "col")
colnames(PDdata)[17] <- "y"
PDdata$y <- as.factor(PDdata$y)

rnames.strip <- substr(rnames,10,12)
PDdata1 <- cbind(PDdata,rnames.strip)
rnames.unique <- unique(rnames.strip)
samp <- sample(rnames.unique,as.integer(length(rnames.unique)*0.2),replace=F)
PDtest <- PDdata1[PDdata1$rnames.strip %in% samp,-24]  # -24 to remove last colummn
PDtrain <- PDdata1[!(PDdata1$rnames.strip %in% samp),-24] # -24 to remove last colummn

#Ordinary Logistic Regression
fit.glm <- glm(y ~ ., data = PDtrain, family = "binomial")
summary(fit.glm)
ypred <- predict.glm(fit.glm, newdata = PDtrain[,-17], type = "response")
roc(PDtrain[,17],ypred, levels = c(0,1), plot = T)

#Bayesian Logistic Regression
xtrain <- PDtrain[,-17]
ytrain <- PDtrain[,17]
blmodel <- logit(ytrain, xtrain, n=rep(1, length(ytrain)), m0 = rep(0, ncol(xtrain)), P0 = matrix(0, nrow=ncol(xtrain), ncol=ncol(xtrain)), samp = 1000, burn = 500)

psi = blmodel$beta %*% t(xtrain)  # samp x n
p   = exp(psi) / (1 + exp(psi) )  # samp x n
ypred.bayes <- colMeans(p)
roc(ytrain, ypred.bayes, plot = T)
