
# DATA EXPLORATION
# load the data
library(data.table)
setwd('~/Downloads/bank')
dtBank <- data.table(read.csv('bank.csv', sep=';'))

# remove the duration
dtBank[, duration := NULL]

# build a histogram about the target attribute
dtBank[, table(y)]
dtBank[, table(y) / .N]
defPercentage <- function(frequency)
{
  percentage = frequency / sum(frequency)
  percentage = round(percentage * 100)
  percentage = paste(percentage, '%')
  return(percentage)
}
defPercentage(dtBank[, table(y) / .N])
help(barplot)
tableOutput <- dtBank[, table(y)]
colPlot <- rainbow(length(tableOutput))
percOutput <- defPercentage(tableOutput)
barplot(
  height = tableOutput,
  names.arg = percOutput,
  col = colPlot,
  legend.text = names(tableOutput),
  xlab = 'Subscribing',
  ylab = 'Number of clients',
  main = 'Proportion of clients subscribing'
)

# explore the features
str(dtBank)
classFeatures <- lapply(dtBank, class)
classFeatures <- classFeatures[names(classFeatures) != 'y']
featCategoric <- names(classFeatures)[
  classFeatures == 'factor'
  ]
featNumeric <- names(classFeatures)[
  classFeatures == 'integer'
  ]

# build charts about the numeric features
par(mfcol = c(2, 3), mar = c(3, 4, 1, 2))
for(feature in featCategoric){
  tableFeature <- dtBank[, table(get(feature))]
  rainbCol <- rainbow(length(tableFeature))
  percFeature <- defPercentage(tableFeature)
  plot.new()
  legend(
    'top', names(tableFeature),
    col = rainbCol, pch = 16,
    title = feature
  )
  barplot(
    height = tableFeature,
    names.arg = percFeature,
    col = rainbCol,
    xlab = feature,
    ylab = 'Number of clients'
  )
}

# convert job into dummy variables
percJob <- dtBank[, table(job) / .N]
colRelevant <- names(percJob)[percJob > 0.08]
for(nameCol in colRelevant){
  newCol <- paste('job', nameCol, sep='_')
  dtBank[, eval(newCol) := ifelse(job == nameCol, 1, 0)]
}
dtBank[, job := NULL]

# convert marital into dummy variables
dtBank[, single := ifelse(marital == 'single', 1, 0)]
dtBank[, divorced := ifelse(marital == 'divorced', 1, 0)]
dtBank[, marital := NULL]

# convert education into dummy variables
dtBank[, edu_primary := ifelse(education == 'primary', 1, 0)]
dtBank[, edu_tertiary := ifelse(education == 'tertiary', 1, 0)]
dtBank[, education := NULL]

# convert housing, default and loan into numeric into dummy variables
dtBank[, housing := as.numeric(housing) - 1]
dtBank[, default := as.numeric(default) - 1]
dtBank[, loan := as.numeric(loan) - 1]

# convert contact into dummy variables
dtBank[, cellular := ifelse(contact == 'cellular', 1, 0)]
dtBank[, telephone := ifelse(contact == 'telephone', 1, 0)]
dtBank[, contact := NULL]

# convert month into numeric
months <- c(
  'jan', 'feb', 'mar', 'apr', 'may', 'jun',
  'jul', 'aug', 'sep', 'oct', 'nov', 'dec'
)
dtBank[
  , month := which(month == months),
  by=1:nrow(dtBank)
  ]
dtBank[, past_success := ifelse(poutcome == 'success', 1, 0)]
dtBank[, past_failure := ifelse(poutcome == 'failure', 1, 0)]
dtBank[, poutcome := NULL]

# explore the numeric features
par(mfrow=c(3, 2), mar=c(5, 4, 4, 2) + 0.1)
for(feature in featNumeric){
  dtBank[, hist(x = get(feature), main=feature, xlab = feature)]
}

# define a feature for not contacted clients
dtBank[, not_contacted := ifelse(pdays == -1, 1, 0)]
dtBank[pdays == -1, pdays := 0]

# define a feature for clients with negative balance
dtBank[, balance_negative := ifelse(balance < 0, 1, 0)]
dtBank[balance < 0, balance := 0]

# convert some numeric features into their logarithms
dtBank[, pdays := log(pdays + 1)]
dtBank[, balance := log(balance + 1)]
dtBank[, campaign := log(campaign + 1)]
dtBank[, previous := log(previous + 1)]

# explore the new features
str(dtBank)
View(dtBank)

# convert the output
dtBank[, output := as.numeric(y) - 1]
dtBank[, y := NULL]


# CLUSTERING
# define a table with the personal data features
featPers <- c(
  'age', 'default', 'balance', 'balance_negative',
  'housing', 'loan',
  'job_admin.', 'job_blue-collar',  'job_management',
  'job_services', 'job_technician',
  'single', 'divorced', 'edu_primary', 'edu_tertiary'
)
dtPers <- dtBank[, featPers, with=F]

# use Hierarchical Clustering on the anagraphic data
d <- dist(dtPers, method = 'euclidean')
hcOut <- hclust(d, method = 'average')
par(mfrow = c(1, 1))
plot(
  hcOut,
  labels = FALSE,
  hang = -1,
  main = 'Dendrogram',
  xlab = 'Client clusters',
  ylab = 'Agglomeration distance'
)

# define the cluster
We can identify 3 clusters cutting the dendrogram around the height of 40. There is also another option that is cutting the dendrogram at a lower level (around 18), identifying 7 clusters. We can explore both the options and visualize the two splits on the dendrogram using rect.hclust.
k1 <- 3
k2 <- 7
par(mfrow=c(1, 1))
rect.hclust(hcOut, k = k1)
rect.hclust(hcOut, k = k2)

# explore the output on the 3 clusters
dtClust <- dtBank[, 'output', with = F]
dtClust[, clusterHc1 := cutree(hcOut, k = k1)]
dtClust[, clusterHc2 := cutree(hcOut, k = k2)]
par(mfrow = c(1, 3), oma = c(0, 0, 10, 0))
for(iCluster in 1:k1){
  tableClust <- dtClust[
    clusterHc1 == iCluster,
    table(output)
    ]
  sizeCluster <- dtClust[, sum(clusterHc1 == iCluster)]
  titlePie <- paste(sizeCluster, 'clients')
  barplot(
    height = tableClust,
    names.arg = defPercentage(tableClust),
    legend.text = c('no', 'yes'),
    col = c('blue', 'red'),
    main = titlePie
  )
}
mtext(
  text = 'Hierarchic clustering, n = 3',
  outer = TRUE, line = 1, cex = 2
)

# explore the output on the 7 clusters
par(mfrow = c(2, 4), oma = c(0, 0, 10, 0))
for(iCluster in 1:k2){
  tableClust <- dtClust[
    clusterHc2 == iCluster,
    table(output)
    ]
  sizeCluster <- dtClust[, sum(clusterHc2 == iCluster)]
  titlePie <- paste(sizeCluster, 'clients')
  barplot(
    height = tableClust,
    names.arg = defPercentage(tableClust),
    col = c('blue', 'red'),
    main = titlePie
  )
}
mtext(
  text = 'Hierarchic clustering, n = 7',
  outer = TRUE, line = 1, cex = 2
)


# PREDICT THE OUTPUT
# prepare building the Random Forest and Logistic Regression
library('randomForest')
setnames(dtBank, 'job_blue-collar', 'job_bluecollar')
arrayFeatures <- names(dtBank)
arrayFeatures <- arrayFeatures[arrayFeatures != 'output']
formulaAll <- paste('output', '~')
formulaAll <- paste(formulaAll, arrayFeatures[1])
for(nameFeature in arrayFeatures[-1]){
  formulaAll <- paste(formulaAll, '+', nameFeature)
}
formulaAll <- formula(formulaAll)
dtTestBinded <- data.table()

# cross-validate the random forest
nIter <- 10
for(iIter in 1:nIter)
{
  indexTrain <- sample(
    x = c(TRUE, FALSE),
    size = nrow(dtBank),
    replace = T,
    prob = c(0.8, 0.2)
  )
  dtTrain <- dtBank[indexTrain]
  dtTest <- dtBank[!indexTrain]
  dtTest1 <- dtTest[output == 1]
  dtTest0 <- dtTest[output == 0]
  n0 <- nrow(dtTest0)
  n1 <- nrow(dtTest1)
  dtTest0 <- dtTest0[sample(x = 1:n0, size = n1)]
  dtTest <- rbind(dtTest0, dtTest1)
  modelRf <- randomForest(
    formula = formulaAll,
    data = dtTrain
  )
  modelLr <- glm(
    formula = formulaAll,
    data = dtTest,
    family = binomial(logit)
  )
  dtTest[, outputRf := predict(
    object = modelRf, newdata = dtTest, type='response'
  )]
  dtTest[, outputLr := predict(
    object = modelLr, newdata = dtTest, type='response'
  )]
  dtTestBinded <- rbind(dtTestBinded, dtTest)
}

# define a function plotting the distributions
plotDistributions <- function(dtTestBinded, colPred)
{
  densityLr0 <- dtTestBinded[
    output == 0,
    density(get(colPred), adjust = 0.5)
    ]
  densityLr1 <- dtTestBinded[
    output == 1,
    density(get(colPred), adjust = 0.5)
    ]
  col0 <- rgb(1, 0, 0, 0.3)
  col1 <- rgb(0, 0, 1, 0.3)
  plot(densityLr0, xlim = c(0, 1), main = 'density')
  polygon(densityLr0, col = col0, border = 'black')
  polygon(densityLr1, col = col1, border = 'black')
  legend(
    'top',
    c('0', '1'),
    pch = 16,
    col = c(col0, col1)
  )
  return()
}

# plot the distributions
par(mfrow = c(1, 1))
plotDistributions(dtTestBinded, 'outputRf')
plotDistributions(dtTestBinded, 'outputLr')



# define a function computing the AUC index
library('ROCR')
plotPerformance <- function(dtTestBinded, colPred)
{
  pred <- dtTestBinded[, prediction(get(colPred), output)]
  perfRates <- performance(pred, 'tpr', 'fpr')
  plot(perfRates)
  perfAuc <- performance(pred, 'auc')
  auc <- perfAuc@y.values[[1]]
  return(auc)
}

# compute and compare the AUC indices
aucRf <- plotPerformance(dtTestBinded, 'outputRf')
aucLr <- plotPerformance(dtTestBinded, 'outputLr')
aucRf
aucLr
