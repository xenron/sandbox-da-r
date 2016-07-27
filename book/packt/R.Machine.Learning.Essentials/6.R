
# VALIDATE THE KNN
# load the packages
library('kknn')
library('data.table')

# define a function building and cross-validating the KNN
validateKnn <- function(
  dtFeatures, # data table with the features
  arrayFeatures, # feature names array
  k = 10, # knn parameter
  kernel = 'rectangular', # knn parameter
  distance = 1 # knn parameter
){
  
  # 1 define the training/test set rows
  indexTrain <- sample(
    x=c(TRUE, FALSE),
    size=nrow(dtFeatures),
    replace=TRUE,
    prob=c(0.9, 0.1)
  )
  
  # 2 define the training/test set
  dtTrain <- dtFeatures[indexTrain]
  dtTest <- dtFeatures[!indexTrain]
  
  # 3 define the formula
  formulaOutput <- 'language ~'
  formulaFeatures <- paste(arrayFeatures, collapse = ' + ')
  formulaKnn <- paste(formulaOutput, formulaFeatures)
  formulaKnn <- formula(formulaKnn)
  
  # 4 build the KNN model
  modelKnn <- kknn(
    formula = formulaKnn,
    train = dtTrain,
    test = dtTest,
    k = k,
    kernel = kernel,
    distance = distance
  )
  
  # 5 defining the predicted language
  languageFitted <- modelKnn$fitted.values
  
  # 6 count the corrected predictions and the total
  languageReal <- dtTest[, language]
  nRows <- length(languageReal)
  
  # 7 define the accuracy index
  percCorrect <- sum(languageFitted == languageReal) / nRows
  
  return(percCorrect)
}

# load the features and define a data table
setwd('~/Downloads/flags')
dfFeatures <- read.table(file = 'dtFeatures.txt')
dtFeatures = data.table(dfFeatures)

# define a vector with the feature names
arrayFeatures <- names(dfFeatures)
arrayFeatures <- arrayFeatures[arrayFeatures != 'language']

# evaluate the KNN accuracy
validateKnn(
  dtFeatures = dtFeatures,
  arrayFeatures = arrayFeatures[1:10],
  k = 8
)

# define a function cross-validating the KNN iteratively
cvKnn <- function(
  dtFeatures, # data table with the features
  nIterations=10, # number of iterations
  ... # feature names array and knn parameters
){
  
  # 1 initialize the accuracy array
  arrayPercCorrect <- c()
  
  for(iIteration in 1:nIterations){
    
    # 2 build and validate the knn
    percCorrect <- validateKnn(dtFeatures, ...)
    
    # 3 add the accuracy to the array
    arrayPercCorrect <- c(arrayPercCorrect, percCorrect)
  }
  
  return(arrayPercCorrect)
}

# determine the accuracies
arrayPercCorrect = cvKnn(
  dtFeatures, nIterations=500,
  arrayFeatures=arrayFeatures
)

# compute the average accuracy
percCorrectMean <- mean(arrayPercCorrect)
percCorrectMean

# plot the accuracy at each iteration
plot(
  x = arrayPercCorrect,
  ylim = c(0, 1),
  xlab = 'Iteration', ylab = 'Accuracy',
  main = 'Accuracy at each iteration'
)
help(abline)
abline(h=percCorrectMean, col='red', lty='dashed')
abline(h=min(arrayPercCorrect), col='blue', lty='dashed')
abline(h=max(arrayPercCorrect), col='blue', lty='dashed')

# plot the average accuracy until each iteration
arrayCumulate <- c()
for(nIter in 1:length(arrayPercCorrect)){
  cumulateAccuracy <- mean(arrayPercCorrect[1:nIter])
  arrayCumulate <- c(arrayCumulate, cumulateAccuracy)
}
Using the same commands as before, we build a new chart. The only new argument is type='l' and it specifies that we display a line instead of points. In order to zoom into the area with the averages, we remove the ylim argument.
plot(
  x = arrayCumulate,
  type = 'l',
  xlab = 'Iteration', ylab = 'Cumulate accuracy',
  main = 'Average accuracy until each iteration'
)
abline(h = percCorrectMean, col = 'red', lty = 'dashed')


# OPTIMIZE K
# define the k to test
arrayK <- 1:50
nIterations <- 100

# validate the knn with different k
dtAccuracyK <- data.table()
for(k in arrayK)
{
  
  # run the KNN and compute the accuracies
  arrayAccuracy <- cvKnn(
    dtFeatures,
    nIterations=nIterations,
    arrayFeatures = arrayFeatures,
    k = k
  )
  # define the new data table rows
  rowsAccuracyK <- data.table(
    accuracy = arrayAccuracy,
    k = k
  )
  # add the new rows to the accuracy table
  dtAccuracyK <- rbind(
    dtAccuracyK,
    rowsAccuracyK
  )
}
head(dtAccuracyK)

# plot all the accuracies
plot(
  x = dtAccuracyK[, k],
  y = dtAccuracyK[, accuracy],
  xlab = 'K', ylab = 'Accuracy',
  main = 'KNN accuracy using different k',
  ylim = c(0, 1),
  col = 'grey'
)

# compute the average accuracy
dtCvK <- dtAccuracyK[
  , list(accuracy = mean(accuracy)),
  by='k'
  ]
View(dtCvK)

# add the average accuracy to the chart
help(points)
points(
  x = dtCvK[, k],
  y = dtCvK[, accuracy],
  pch = 16
)

# plot the average accuracy
plot(
  x = dtCvK[, k],
  y = dtCvK[, accuracy],
  xlab = 'k', ylab = 'accuracy',
  main = 'average knn accuracy using different k',
  type = 'o'
)

# identify the k performing best
kOpt <- dtCvK[accuracy == max(accuracy), k]
abline(v = kOpt, col = 'red')


# OPTIMIZE THE FEATURES
# rank the features
library('FSelector')
dfGains <- information.gain(
  language~., dtFeatures
)
dfGains$feature <- row.names(dfGains)
dtGains <- data.table(dfGains)
dtGains <- dtGains[order(attr_importance, decreasing = T)]
arrayFeatures <- dtGains[, feature]
arrayFeatures contains the features sorted by relevance. Now we can build the model choosing the top n features. The options for n are the numbers between 1 and the total number of features and we define arrayN containing them.

# define the number of features to test
arrayN <- 1:length(arrayFeatures)

# validate the KNN using different feature sets
dtAccuracyN = data.table()
for(n in arrayN)
{
  # 1 run the KNN and compute the accuracies
  arrayAccuracy <- cvKnn(
    dtFeatures,
    nIterations = nIterations,
    arrayFeatures = arrayFeatures[1:n],
    k = kOpt
  )
  
  # 2 define the new data table rows
  rowsAccuracyN <- data.table(
    accuracy = arrayAccuracy,
    n = n
  )
  
  # 3 add the new rows to the accuracy table
  dtAccuracyN <- rbind(
    dtAccuracyN,
    rowsAccuracyN
  )
}

# build a chart displaying the accuracies
plot(
  x = dtAccuracyN[, n],
  y = dtAccuracyN[, accuracy],
  xlab = 'N', ylab = 'Accuracy',
  main = 'KNN accuracy using different features',
  ylim = c(0, 1),
  col = 'grey'
)
dtCvN <- dtAccuracyN[
  , list(accuracy = mean(accuracy)),
  by='n'
  ]
points(
  x = dtCvN[, n],
  y = dtCvN[, accuracy],
  xlab = 'n', ylab = 'accuracy',
  pch = 16
)

# plot the average accuracy
plot(
  x = dtCvN[, n],
  y = dtCvN[, accuracy],
  xlab = 'N', ylab = 'Accuracy',
  main = 'Average knn accuracy using different features',
  type = 'o'
)

# identify the n performing best
nOpt <- dtCvN[accuracy == max(accuracy), n]
abline(v = nOpt, col = 'red')


# OPTIMIZE K AND THE FEATURES AT THE SAME TIME

# define the K and N (the number of features)
arrayK <- seq(from = 1, to =  49, by = 2)
arrayN <- seq(from = 11, to = 37, by = 2)

# define the combinations between K and N
dfParameters <- expand.grid(k=arrayK, n=arrayN)
dtParameters <- data.table(dfParameters)
head(dtParameters)

# validate the knn with different k and nFeatures
for(iConfig in 1:nrow(dtParameters)){  
  # compute the accuracy
  arrayAccuracy <- cvKnn(
    dtFeatures, nIterations = nIterations,
    arrayFeatures = arrayFeatures[1:dtParameters[iConfig, n]],
    k = dtParameters[iConfig, k]
  )
  # add the average accuracy to dtParameters
  dtParameters[iConfig, accuracy := mean(arrayAccuracy)]
}

# reshape dtParameters into an accuracy matrix
help(reshape)
dfAccuracy <- reshape(
  data = dtParameters,
  direction = "wide",
  v.names = "accuracy",
  idvar = "n",
  timevar = "k"
)
View(dfAccuracy)
dfAccuracy$n <- NULL
matrixAccuracy <- as.matrix(dfAccuracy)
rownames(matrixAccuracy) <- arrayN
colnames(matrixAccuracy) <- arrayK
View(matrixAccuracy)

# plot the performance depending on k and n
help(image)
image(
  x = arrayN, y = arrayK, z = matrixAccuracy,
  xlab = 'n', ylab = 'k',
  col = heat.colors(100)
)
