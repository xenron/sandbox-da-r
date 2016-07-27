
# load the flag features and build a data table
setwd('~/Downloads/flags')
dfFeatures <- read.table(file = 'dtFeatures.txt')
library("data.table")
dtFeatures <- data.table(dfFeatures)

# explore the feature tables
str(dtFeatures)


# K-MEANS
# define the function inputs
help(kmeans)
arrayFeatures <- names(dtFeatures)[-1]
dtFeaturesKm <- dtFeatures[, arrayFeatures, with=F]
dtFeaturesKm[, as.numeric(red)]
dtFeaturesKm[, as.numeric(red) – 1]
for(nameCol in arrayFeatures)
  dtFeaturesKm[
    , eval(nameCol) := as.numeric(get(nameCol)) - 1
    ]
View(dtFeaturesKm)
matrixFeatures <- as.matrix(dtFeaturesKm)

# cluster the data using the k-means
nCenters <- 8
modelKm <- kmeans(
  x = matrixFeatures,
  centers = nCenters
)

# explore the K-Means output
names(modelKm)
View(modelKm$centers)

# add the cluster to the data table
dtFeatures[, clusterKm := modelKm$cluster]

# explore the clusters
nameCluster <- 'clusterKm'
dtFeatures[, list(.N), by=nameCluster]
dtFeatures[, list(nCountries=.N), by=nameCluster]
dtFeatures[, table(language)]
dtFeatures[, as.list(table(language))]
dtFeatures[, as.list(table(language)), by=nameCluster]
dtFeatures[, as.list(table(language) / .N), by=nameCluster]

# aggregate the data by cluster
dtClusters <- dtFeatures[
  , c(list(nCountries=.N), as.list(table(language) / .N)),
  by=nameCluster
  ]

# define the barplot inputs
arrayLanguages <- dtFeatures[, unique(language)]
dtBarplot <- dtClusters[, arrayLanguages, with=F]
matrixBarplot <- t(as.matrix(dtBarplot))
nBarplot <- dtClusters[, nCountries]
namesLegend <- names(dtBarplot)
help(substring)
namesLegend <- substring(namesLegend, 1, 12)
arrayColors <- rainbow(length(namesLegend))
plotTitle <- paste('languages in each cluster of', nameCluster)

# build the histogram
barplot(
  height = matrixBarplot,
  names.arg = nBarplot,
  col = arrayColors,
  legend.text = namesLegend,
  xlim = c(0, ncol(matrixBarplot) * 2),
  main = plotTitle,
  xlab = 'cluster'
)

# define a function for building the histogram
plotCluster <- function(
  dtFeatures, # data table with the features
  nameCluster # name of the column defining the cluster
){
  # aggregate the data by cluster
  dtClusters <- dtFeatures[
    , c(list(nCountries=.N), as.list(table(language) / .N)),
    by=nameCluster]
  
  # prepare the histogram inputs
  arrayLanguages <- dtFeatures[, unique(language)]
  dtBarplot <- dtClusters[, arrayLanguages, with=F]
  matrixBarplot <- t(as.matrix(dtBarplot))
  nBarplot <- dtClusters[, nCountries]
  namesLegend <- names(dtBarplot)
  namesLegend <- substring(namesLegend, 1, 12)
  arrayColors <- rainbow(length(namesLegend))
  
  # build the histogram
  barplot(
    height = matrixBarplot,
    names.arg = nBarplot,
    col = arrayColors,
    legend.text = namesLegend,
    xlim=c(0, ncol(matrixBarplot) * 2),
    main = paste('languages in each cluster of', nameCluster),
    xlab = 'cluster'
  )
  
}

# visualize the histogram using the functions
plotCluster(dtFeatures, nameCluster)

# load the package for visualizing the world map
install.packages('rworldmap')
library(rworldmap)

# clean the country attribute
dtFeatures[, country := rownames(dfFeatures)]
dtFeatures[country == 'Germany-FRG', country := 'Germany']
dtFeatures[country == 'USSR', country := 'Russia']

# define a function for visualizing the world map
plotMap <- function(
  dtFeatures, # data table with the countries
  colPlot, # feature to visualize
  colourPalette = 'negpos8' # colors
){
  
  # function for visualizing a feature on the world map
  
  # define the column to plot
  dtFeatures[, colPlot := NULL]
  dtFeatures[, colPlot := substring(get(colPlot), 1, 12)]
  
  # prepare the data to plot
  mapFeatures <- joinCountryData2Map(
    dtFeatures[, c('country', 'colPlot'), with=F],
    joinCode = 'NAME',
    nameJoinColumn = 'country'
  )
  
  # build the chart
  mapCountryData(
    mapFeatures,
    nameColumnToPlot='colPlot',
    catMethod = 'categorical',
    colourPalette = colourPalette,
    missingCountryCol = 'gray',
    mapTitle = colPlot
  )
  
}

# build a map with the K-Means clusters
plotMap(dtFeatures, colPlot = 'clusterKm')


# HIERARCHIC CLUSTERING

# prepare the Hierarchical Clustering inputs
help(hclust)
help(dist)
matrixDistances <- dist(matrixFeatures, method = 'manhattan')

# build the hierarchic clustering model
modelHc <- hclust(d = matrixDistances, method = 'complete')
modelHc contains the clustering model and we can visualize the cluster using plot. You can see the help of hclust to understand the plot parameters.

# visualize the hierarchic clustering dendrogram
plot(modelHc, labels = FALSE, hang = -1)

# define the clusters
heightCut <- 17.5
abline(h=heightCut, col='red')
cutree(modelHc, h = heightCut)
dtFeatures[, clusterHc := cutree(modelHc, h = heightCut)]

# visualize the clusters
plotCluster(dtFeatures, nameCluster = 'clusterHc')
plotMap(dtFeatures, colPlot = 'clusterHc')


# PREDICT THE LANGUAGE USING THE KNN

# define the feature names
arrayFeatures <- names(dfFeatures)[-1]

# add the country to dtFeatures
dtFeatures[, country := rownames(dfFeatures)]
dtFeatures[country == 'Germany-FRG', country := 'Germany']
dtFeatures[country == 'USSR', country := 'Russia']

# explore the languages
dtFeatures[, table(language)]
plotMap(dtFeatures, colPlot = 'language', colourPalette = 'rainbow')

# build a pie chart
help(pie)
arrayTable <- dtFeatures[, table(language)]
pie(arrayTable)

# reduce the number of groups
listGroups <- list(
  english = 'English',
  spanish = 'Spanish',
  frger = c('French', 'German'),
  indoEu = c('Slavic', 'Other Indo-European'),
  arabic = 'Arabic',
  other = c(
    'Japanese/Turkish/Finnish/Magyar', 'Chinese', 'Others'
  )
)

# re-define the language column containing the language groups
for(nameGroup in names(listGroups)){
  dtFeatures[
    language %in% listGroups[[nameGroup]],
    language := nameGroup
    ]
}

# re-define the language column using factor
dtFeatures[, language]
dtFeatures[, language := factor(language)]
dtFeatures[, language]

# visuailze the language groups
plotMap(dtFeatures, colPlot = 'language')
pie(dtFeatures[, table(language)])

# install and load the package for KNN
install.packages("kknn")
library(kknn)

# define the kknn inputs
help(kknn)
formulaKnn <- 'language ~'
for(nameFeature in arrayFeatures){
  formulaKnn <- paste(formulaKnn, '+', nameFeature)
}
formulaKnn <- formula(formulaKnn)

# split the dataset into training and test set
help(sample)
indexTrain <- sample(
  x=c(TRUE, FALSE),
  size=nrow(dtFeatures),
  replace=TRUE,
  prob=c(0.8, 0.2)
)
dtTrain <- dtFeatures[indexTrain]
dtTest <- dtFeatures[!indexTrain]

# build the KNN model
modelKnn <- kknn(
  formula = formulaKnn,
  train = dtTrain,
  test = dtTest,
  k = 10,
  kernel = 'rectangular',
  distance = 1
)

# extract the fitted values
modelKnn$fitted.values

# add the estimated language to dtTest
dtTest[, languagePred := modelKnn$fitted.values]

# evaluate the model
percCorrect <- dtTest[, sum(language == languagePred) / .N]
percCorrect

# compute the information gain ratio
library('FSelector')
formulaFeat <- paste(arrayFeatures, collapse = ' + ')
formulaGain <- formula(paste('language', formulaFeat, sep = ' ~ '))
dfGains <- information.gain(language~., dtTrain)
dfGains$feature <- row.names(dfGains)
dtGains <- data.table(dfGains)
dtGains <- dtGains[order(attr_importance, decreasing = T)]
View(dtGains)

# re-define the feature vector sorted by information gain ratio
arrayFeatures <- dtGains[, feature]

# define a function for building the formula
buildFormula <- function(
  arrayFeatures, # feature vector
  nFeatures # number of features to include
){
  arrayFeaturesTop <- arrayFeatures[1:nFeatures]
  formulaKnn <- paste('language', '~')
  for(nameFeature in arrayFeaturesTop){
    formulaKnn <- paste(formulaKnn, '+', nameFeature)
  }
  formulaKnn <- formula(formulaKnn)
  return(formulaKnn)
}
formulaKnnTop <- buildFormula(arrayFeatures, nFeatures = 10)
formulaKnnTop

# build the model
modelKnn <- kknn(
  formula = formulaKnnTop,
  train = dtTrain,
  test = dtTest,
  k = 10,
  kernel = 'rectangular',
  distance = 1
)

# add the output to dtTest
dtTest[, languagePredTop := modelKnn$fitted.values]

# evaluate the model
percCorrectTop <- dtTest[, sum(language == languagePredTop) / .N]
percCorrectTop

# build the weighted knn model
modelKnn <- kknn(
  formula = formulaKnn,
  train = dtTrain,
  test = dtTest,
  k = 10,
  kernel = 'optimal',
  distance = 1
)

# add the estimated language to dtTest
dtTest[, languagePredWeighted := modelKnn$fitted.values]
percCorrectWeighted <- dtTest[
  , sum(language == languagePredWeighted) / .N
  ]
