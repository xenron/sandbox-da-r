install.packages("tree")
#decision trees
library(tree)
data(iris)
sample = iris[sample(nrow(iris)),]
train = sample[1:105,]
test = sample[106:150,]
model = tree(Species~.,train)
summary(model)
pred = predict.tree(model,test[,-5],type="class")
pred


'Classification tree:
tree(formula = Species ~ ., data = train, x = TRUE, y = TRUE)
Variables actually used in tree construction:
[1] "Petal.Length" "Sepal.Length" "Petal.Width" 
Number of terminal nodes:  5 
Residual mean deviance:  0.1332 = 13.32 / 100 
Misclassification error rate: 0.0381 = 4 / 105 '
