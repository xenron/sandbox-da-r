
# DATA EXPLORATION
# load and explore the data
setwd('~/Downloads/flags')
dfFlag <- read.csv('flag.data', header=FALSE)
str(dfFlag)

# define the attribute names
nameCountry <- 'name'
namesGeography <- c('continent', 'zone', 'area')
namesDemography <- c('population', 'language', 'religion')
namesAttributes <- c(nameCountry, namesGeography, namesDemography)
namesNumbers <- c('bars', 'stripes', 'colors')
namesColors <- c('red', 'green', 'blue', 'gold', 'white', 'black', 'orange')
nameMainColor <- 'mainhue'
namesDrawings <- c(
  'circles', 'crosses', 'saltires', 'quarters',
  'sunstars', 'crescent', 'triangle', 'icon', 'animate', 'text'
)
namesAngles <- c('topleft', 'botright')
namesFlag <- c(namesNumbers, namesColors, nameMainColor, namesDrawings, namesAngles)

# select the column containing the attributes
names(dfFlag) <- c(namesAttributes, namesFlag)

# define a flag data table
library(data.table)
dtFlag <- data.table(dfFlag)

# convert the continent column into a factor
dtFlag[1:20, continent]
vectorContinents <- c('N.America', 'S.America', 'Europe', 'Africa', 'Asia', 'Oceania')
dtFlag[, continent := factor(continent, labels=vectorContinents)]

# convert the zone, language, religion columns into factors
vectorZones <- c('NE', 'SE', 'SW', 'NW')
dtFlag[, zone := factor(zone, labels=vectorZones)]
vectorLanguages <- c(
  'English', 'Spanish', 'French', 'German', 'Slavic',
  'Other Indo-European', 'Chinese', 'Arabic',
  'Japanese/Turkish/Finnish/Magyar', 'Others'
  )
dtFlag[, language := factor(language, labels=vectorLanguages)]
vectorReligions <- c(
  'Catholic', 'Other Christian', 'Muslim', 'Buddhist',
  'Hindu', 'Ethnic', 'Marxist', 'Others'
)
dtFlag[, religion := factor(religion, labels=vectorReligions)]
str(dtFlag)

# count the occurrences of the levels
table(dtFlag[, mainhue])
nameCol <- 'mainhue'
dtFlag[, table(get(nameCol))]
listTableCol = lapply(
namesAngles, function(nameCol){
dtFlag[, table(get(nameCol))]
})
listTableCol[[1]]

# build a histogram of the levels
nameCol <- 'language'
freqValues <- dtFlag[, table(get(nameCol))]
names(freqValues)
barplot(
  height = freqValues,
  names.arg = names(freqValues),
  main = nameCol,
  col = rainbow(length(freqValues)),
  ylab = 'number of flags'
)

# define a function building levels histograms
barplotAttribute <- function(dtData, nameCol)
{
  # define the frequency
  freqValues <- dtData[, table(get(nameCol))]
  # define the percentage
  percValues <- freqValues / sum(freqValues)
  percValues <- round(percValues * 100)
  percValues <- paste(percValues, '%')
  # generate the histogram
  barplot(
    height = freqValues,
    names.arg = names(freqValues),
    main = nameCol,
    col = rainbow(length(freqValues)),
    legend.text = percValues,
    ylab = 'number of flags'
  )
}
barplotAttribute(dtFlag, 'stripes')

# build a histogram for each feature
for(nameCol in namesFlag)
{
barplotAttribute(dtFlag, nameCol)
readline()
}

# explore the binary features
dtFlag[, sum(red)]
dtFlag[, sum(get('red'))]
namesColors
dtFlag[, sum(get(namesColors[1]))]
sapply(namesColors, function(nameColor){
  dtFlag[, sum(get(nameColor))]
})

# build a decision tree
library('rpart')
library('rpart.plot')
formulaRpart <- 'language ~ '
for(name in namesFlag){
formulaRpart <- paste(formulaRpart, '+', name)
}
formulaRpart <- formula(formulaRpart)
tree <- rpart(formula=formulaRpart, data=dtFlag)
prp(tree)

# define a function building the tree
dtFeatures <- dtFlag[, c('language', namesFlag), with=FALSE]
plotTree <- function(dtFeatures){
formulaRpart <- paste(names(dtFeatures)[1], '~')
for(name in names(dtFeatures)[-1]){
formulaRpart <- paste(formulaRpart, '+', name)
}
formulaRpart <- formula(formulaRpart)
tree <- rpart(formula=formulaRpart, data=dtFeatures)
prp(tree)
}
plotTree(dtFeatures)


# FEATURE TRANSFORMATION
# convert the binary features into factors
namesProcessed <- c()
nameFeat <- 'red'
length(unique(dtFeatures[, get(nameFeat)])) == 2
vectorFactor <- dtFeatures[
, factor(get(nameFeat), labels=c('no', 'yes'))
]
head(vectorFactor)
for(nameFeat in namesFlag){
  if(length(unique(dtFeatures[, get(nameFeat)])) == 2){
    vectorFactor <- dtFeatures[
      , factor(get(nameFeat), labels=c('no', 'yes'))]
    dtFeatures[, eval(nameFeat) := vectorFactor]
    namesProcessed <- c(namesProcessed, nameFeat)
  }
}
setdiff(namesFlag, namesProcessed)

# convert the bar feature into dummy variables
barplotAttribute(dtFeatures, 'bars')
dtFeatures[, nBars0 := bars == 0]
dtFeatures[, nBars1_2 := bars %in% c(1, 2)]
dtFeatures[, nBars3 := bars == 3]
dtFeatures[, bars := NULL]
namesProcessed <- c(namesProcessed, 'bars')

# convert the stripes feature into dummy variables
barplotAttribute(dtFeatures, 'stripes')
dtFeatures[, nStrp0 := stripes == 0]
dtFeatures[, nStrp2 := stripes == 2]
dtFeatures[, nStrp3 := stripes == 3]
dtFeatures[, nStrp5 := stripes == 5]
dtFeatures[, stripes := NULL]
namesProcessed <- c(namesProcessed, 'stripes')

# convert the colors feature into dummy variables
barplotAttribute(dtFeatures, 'colors')
dtFeatures[, nCol12 := colors %in% c(1, 2)]
dtFeatures[, nCol3 := colors == 3]
dtFeatures[, nCol4_5 := colors %in% c(4, 5)]
dtFeatures[, colors := NULL]
namesProcessed <- c(namesProcessed, 'colors')

# build a histogram about the non-processed features
for(nameCol in setdiff(namesDrawings, namesProcessed)){
barplotAttribute(dtFeatures, nameCol)
readline()
}

# convert some of the remaining features into binary
for(nameCol in setdiff(namesDrawings, namesProcessed)){
dtFeatures[, eval(nameCol) := ifelse(get(nameCol) > 0, 'yes', 'no')]
namesProcessed <- c(namesProcessed, nameCol)
}

# build a histogram about the non-processed features
for(nameCol in setdiff(namesFlag, namesProcessed)){
  barplotAttribute(dtFeatures, nameCol)
  readline()
}

# convert the remaining features into dummy variables
namesToDummy <- c("topleft", "botright", "mainhue")
for(nameCol in namesToDummy){
frequencyColors <- dtFeatures[, list(.N), by=nameCol]
for(color in frequencyColors[N > 20, get(nameCol)]){
nameFeatNew <- paste(nameCol, color, sep='')
dtFeatures[, eval(nameFeatNew) := get(nameCol) == color]
}
dtFeatures[, eval(nameCol) := NULL]
namesProcessed <- c(namesProcessed, nameCol)
}

# convert the logical column into factors
for(nameCol in names(dtFeatures)){
if(dtFeatures[, class(get(nameCol))] == 'logical'){
print(nameCol)
dtFeatures[, eval(nameCol) := ifelse(get(nameCol), 'yes', 'no')]
}
}

# plot the tree with the new features
plotTree(dtFeatures)


# FEATURE RANKING

# install and load the feature selection package
install.packages('FSelector')
library('FSelector')

# compute the information gain ratio
namesFeatures <- names(dtFeatures)[-1]
dfGains <- information.gain(language~., dtFeatures)
dfGains$feature <- row.names(dfGains)

# define a data table with the information gain ratio
dtGains <- data.table(dfGains)
dtGains <- dtGains[order(attr_importance, decreasing = T)]
head(dtGains)

# select the top features
dtGainsTop <- dtGains[1:12]
barplot(
  height = dtGainsTop[, attr_importance],
  names.arg = dtGainsTop[, feature],
  main = 'information gain',
  col = rainbow(nrow(dtGainsTop)),
  legend.text = dtGainsTop[, feature],
  xlim=c(0, 20)
)
