################################################################################
### 1. Code to support the Recipe "Data Clustering with R" in chapter 9
### Requires: data file yeast.rda and yeast100.rda available from Books webpage
################################################################################
load("yeast.rda") # Use the complete path to the code directory
myData <- yeast[,2:9]
k <- 4
kmeans_result <- kmeans(myData, k)
table(kmeans_result$cluster) 
par(mfrow=c(1,2))
plot(myData[c("mit", "gvh")], col = kmeans_result$cluster, main = "(A) Plot with clusters") 
plot(myData[c("mit", "gvh")], col = yeast$class, main = "(B) Plot with actual classes") 
points(kmeans_result$centers[,c("mit", "gvh")], col = 1:4, pch = 8, cex=2) 
## Heirarchical clustering
load("yeast100.rda") # Use the complete path to the code directory
myData_100 <-yeast100[,2:9]
myDist <- dist(myData_100) 
hc <- hclust(myDist, method="ave") 
groups <- cutree(hc, k=k) 
plot(hc, hang <- -1, labels=groups) 
rect.hclust(hc, k = 4, which = NULL, x = NULL, h = NULL, border = 2, cluster = NULL)
################################################################################
### 2. Code to support the Recipe "Visualizing clusters" in chapter 9
### Requires: fpc, vegan  R libraries 
################################################################################
plot(myData, col = kmeans_result$cluster) 
## Shade plot
install.packages("fpc")
library(fpc)
clusplot(myData, kmeans_result$cluster, color=T, shade=T, labels=2, lines=0)
## Spider plot
library(vegan)
groups <- levels(factor(kmeans_result$cluster))
ordiplot(cmdscale(dist(myData)), type = "n") # might issue a warning
cols <- rainbow(nlevels(factor(kmeans_result$cluster)))
for(i in seq_along(groups)){
	points(cmdscale(dist(myData))[factor(kmeans_result$cluster) == groups[i], ], col = cols[i], pch = 16)
}
ordispider(cmdscale(dist(myData)), factor(kmeans_result$cluster), label = TRUE) 
ordihull(cmdscale(dist(myData)), factor(kmeans_result$cluster), lty = "dotted")
#### Sihlhoutte plot
myDaisy <- (daisy(myData))^2 
mySil <- silhouette(kmeans_result$cluster, myDaisy)
plot(mySil)

################################################################################
### 3. Code to support the Recipe "Supervised learning for classification" in chapter 3
### Requires: MASS, e1071 rpart R libraries (from corresponding repositories) and data set cancer.rda from Books webpage
################################################################################
library(MASS)
load("cancer.rda")
train_row <- sample(1:83, 60)
train <- mldata[train_row,] # use sampled indexes to extract training data
test <- mldata[-train_row,] # test set is select by selecting all the other data points
testClass <- test$tumor 
test$tumor <- NULL 
myLD <- lda(tumor ~ ., train) #Warning message: In lda.default(x, grouping, ...) : variables are collinear
testRes_lda <- predict(myLD, test)
sum(testRes_lda$class == testClass) 
[1] 18
sum(testRes_lda$class != testClass)
[1] 5
##### DT
library(rpart)
myDT <- rpart(tumor~ ., data = train, control = rpart.control(minsplit = 10))
plot(myDT)
text(myDT, use.n=T)
testRes_dt = predict(myDT, newdata= test) 
classes<- round(testRes_dt)
head(classes)
###########SVM
library(e1071)
mySVM <- svm(tumor ~ ., data = train) 
testRes_svm  <- predict(mySVM, test) 
testRes_svm

################################################################################
### 4. Code to support the Recipe "Probabilistic learning in R with Naive bayes" in chapter 9
### Requires: e1071,  libraries (from corresponding repositories) and data set cancer.rda from Books webpage
################################################################################
library(e1071)
model <- naiveBayes(tumor ~ ., data = train)   
testRes_nb <- predict(model, test)     
table(testRes_nb, testClass) 


###
cl=c()
for(i in 1:nrow(classes)){
	cl<- c(cl,names(which(classes[i,]==1)))
}
allRes<-data.frame(Orig=testClass, lda=testRes_lda$class, dt=cl, svm=testRes_svm, nb=testRes_nb)

################################################################################
### 5. Code to support the Recipe "Do a bootstrap in machine learning" in chapter 9
### Requires: Biobase, annotate hgu133a.db R libraries available from Bioconductor
################################################################################
data(iris)
myData = iris[c(1:150),]

corPred <- function(data, label,indices){
		train = myData[indices,] # indexes for training data
		test = myData[-indices,] # indexes for test data
		testClass = test[,label] # assigns class labels (species)
		colnames(train)[ncol(train)]="Class"
		mySVM = svm(Class ~ ., data = train, cost = 100, gamma = 1) # learn model using SVM
		myPred  = predict(mySVM, test) # prediction on test set
 		TP = sum(myPred == testClass) # calculate True positives
 		return(TP)
}

myboot <- function(d, label,iter){
		bootres = c()
		for(i in 1:iter){
			indices = sample(1:nrow(d), floor((2*nrow(d))/3)) # samples indexes
			res = corPred(d,label,indices) # runs corPred function 
			bootres = c(bootres, res) # append results
}
return(list(BOOT.RES = bootres, BOOT.MEAN = mean(bootres), BOOT.SD = sd(bootres)))
}
res.bs <- myboot(d = myData, label="Species",iter = 10000)

################################################################################
### 6. Code to support the Recipe "k fold cross validation for classifiers" in chapter 9
### Requires: e1071 library
################################################################################
library(e1071)
data(iris)
myData=iris[1:100,]
myData$Species=factor(as.character(myData$Species))
k=10
index <- sample(1:k,nrow(myData),replace=TRUE)
folds <- 1:k
myRes=data.frame()
for (i in 1:k){
	training <- subset(myData, index %in% folds[-i]) # create training set
	test <- subset(myData, index %in% c(i)) # create test set
  	mymodel <- svm(training$Species ~., data=training) # train model
  	actual <- test[,ncol(test)] # get actual lables
	temp <- data.frame(predict(mymodel, test [,-ncol(test)])) # run model on test set
	colnames(temp)="Predicted"
  	results <- data.frame(Predicted=temp, Actual=actual) # create data.frame for results
	myRes <- rbind(myRes, results) # append results for each iteration
}
table(myRes)

################################################################################
### 7. Code to support the Recipe "Measures of performance" in chapter 9
### Requires: ROCR, pROC, caret R libraries 
################################################################################
install.packages("caret")
library(caret)
data(iris)
myData <- iris
indices <- sample(1:nrow(myData), floor((2*nrow(myData))/3))
train <- myData[indices,] # indexes for training data
test <- myData[-indices,] # indexes for test data
testClass <- test[,"Species"] # assigns class labels (species)
mySVM <- svm(Species ~ ., data = train, cost = 100, gamma = 1) # learn model using SVM
myPred  <- predict(mySVM, test)
myLabels <- testClass
myCM <- confusionMatrix(myPred, myLabels)
CMtable <- myCM$table
myCM$byClass
################################################################################
### 8. Code to support the Recipe "Visualizing ROC curve in R" in chapter 9
### Requires: ROCR pROC R libraries 
################################################################################
library(pROC)
roc_lda <- plot.roc(as.numeric(testRes_lda$class), as.numeric(testClass))
plot(roc_lda)
roc_svm <- plot.roc(as.numeric(testRes_svm), as.numeric(testClass))
plot(roc_lda, col="grey")
lines(roc_svm, col="black")
legend("bottomright", c("svm", "lda"), fill = c("black","grey"))

multiclass.roc(as.numeric(myPred), as.numeric(testClass), percent=TRUE)
################################################################################
################################################################################