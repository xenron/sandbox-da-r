install.packages("randomForest")
#randomForest
library(randomForest)
data(iris)
sample = iris[sample(nrow(iris)),]
train = sample[1:105,]
test = sample[106:150,]
model =randomForest(Species~.,data=train,mtry=4,importance =TRUE,proximity=TRUE)
model
'
Call:
 randomForest(formula = Species ~ ., data = train, mtry = 4, importance = TRUE,      proximity = TRUE) 
               Type of random forest: classification
                     Number of trees: 500
No. of variables tried at each split: 4

        OOB estimate of  error rate: 5.71%
Confusion matrix:
           setosa versicolor virginica class.error
setosa         31          0         0  0.00000000
versicolor      0         33         3  0.08333333
virginica       0          3        35  0.07894737
'
pred = predict(model,newdata=test[,-5])
pred 


