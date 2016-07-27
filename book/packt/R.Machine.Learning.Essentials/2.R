
# SOME R TOOLS
# define variables
n1 <- 2
n2 <- 3
n1

# use operators
n1 + n2
n1 * n2
nSum <- n1 + n2
nProd <- n1 * n2
nSum

# use functions
sum(2, 3)
sum(2, 3, 4)
s1 <- sum(2, 3)
s2 <- sum(2, 3, 4)
print(s1)

# define functions
funProd <- function(n1, n2)
{
  n <- n1 * n2
  return(n)
}

funProdPrint <- function(n1, n2){
  n <- n1 * n2
  print(n1)
  print(n2)
  print(n)
  return(n)
}
prod <- funProdPrint(n1 = 2, n2 = 3)

# visualize the documentation
help(sum)

# use vectors
a1 <- c(1, 2, 3)
a1
a1[1]
a1[c(1, 2)]
a1 + 1
a2 <- c(1, 2, 3)
a1 + a2
a3 <- 1:10
a3
a4 <- c(1, NA, 2)

# use booleans
bool1 <- TRUE
bool2 <- FALSE
bool3 <- bool1 & bool2
bool4 <- bool1 | bool2
bool5 <- !bool1
bool3
bool4
bool5
x1 <- 1
x2 <- 2
bool5 <- x1 == x2
bool6 <- x1 != x2
bool7 <- x1 <= x2
bool5
bool6
bool7

# use if statements
if(bool5){
  x <- 1
}else{
  x <- 2
}
x

# use for loops
vectorI <- c(1, 2, 5)
x <- 0
for(i in vectorI)
{
  if(i > 1)
  {
    x <- x + i
  }
}
x

nIter <- 10
vectorIter <- 1:nIter
total <- 0
for(i in vectorIter){
  total <- total + 1
}
total


# SOME R OBJECTS

# visualize the objects classes
class(n1)
class(funProd)
class(bool5)
class(a1)

# use strings
s1 <- 'string1'
s2 <- "string2"
sPaste <- paste(s1, s2, sep = '_')
sSub <- substring(sPaste, 2, 5)
sSub
vectorStrings <- c(s1, s2, sPaste, sSub)
vectorStrings
class(vectorStrings)

# homogeneity of vectors
vectorStringsNum <- c(s1, s2, 10, 1.3)
vectorStringsNum
class(vectorStringsNum)

# use factors
vectorString <- c('a', 'a', 'b', 'c')
vectorFactor <- factor(vectorString)
class(vectorFactor)
levels(vectorFactor)
table(vectorFactor)

# use dates
stringDate <- '2013-01-01'
formatDate <- '%Y-%m-%d'
date1 <- as.Date(stringDate, format = formatDate)
class(date1)
date1
date2 <- date1 + 10
date2
date1 > date2

# use lists
l1 <- list(1, a1, sPaste)
l1
l2 <- list(elNumber = 1, elvector = a1, elString = sPaste)
l2
l1[[1]]
l2[[1]]
l2$elNumber
[1] 1
names(l2)
names(l1) <- c('el1', 'el2', 'el3')
names(l1)
l3 <- l2[1]
l3
l4 <- l2[c(1, 2)]
l4

# use matrices
vectorMatrix <- c(1, 2, 3, 11, 12, 13)
matrix1 <- matrix(vectorMatrix, ncol = 2)
matrix1
matrix2 <- t(matrix1)
matrix2
vector3 <- c('a', 'b', 'c')
matrix3 <- cbind(matrix1, vector3)
matrix3
rownames(matrix3)
colnames(matrix3)
rownames(matrix3) <- c('row1', 'row2', 'row3')
colnames(matrix3) <- c('col1', 'col2', 'col3')
matrix3

# use data frames
df1 <- data.frame(matrix3)
df1
df1[1, ]
df1[, 2]
df1[1, 2]
colnames(df1)
rownames(df1)
df1[[1]]
names(df1)
df1$vector1
df1[c(1, 3)]
df1[c('col1', 'col3')]
df2 <- data.frame(
  numbers1 = c(1, 2, 3),
  numbers2 = c(1, 2, 3),
  characters1 = c('a', 'b', 'c')
)
df2$numbers3 <- df2$numbers1 * df2$numbers2
df2$numbers4 <- 1
df2
View(df2)

# use apply
x1 <- 1:10
func1 <- function(el){
  result <- el ^ 2
  return(result)
}
sapply(X = x1, FUN = func1)
l1 <- list(a = 1, b = 2, c = 3)
lapply(X = l1, FUN = func1)
matrix4 <- matrix(1:9, nrow = 3)
matrix4
apply(X = matrix4, MARGIN = 1, FUN = sum)
apply(X = matrix4, MARGIN = 2, FUN = sum)
apply(X = matrix4, MARGIN = c(1, 2), FUN = func1)


# R STANDARDS
x <- 1
sum(1, 2)


# PACKAGES

# install and load R packages
install.packages('data.table')
install.packages(
  pkgs = 'data.table',
  repos = 'http://cran.us.r-project.org'
)
library(data.table)

# use the R datasets
data()
help(iris)
class(iris)

# use data.table
dtIris <- data.table(iris)
class(dtIris)
str(dtIris)
print(dtIris)
View(dtIris)
dtIris[1]
dtIris[1:3]
dtIris[, Species]
dtIris[1:3, Species]
dtIris[1:3, 'Species', with = F]
dtIris[1:3, c(5, 1, 2), with = F]
dtIris[Sepal.Length > 7]
dtIris[, Sepal.Area := Sepal.Length * Sepal.Width]
dtIris[1:6]
dtIris[, mean(Sepal.Area)]
dtIris[, mean(Sepal.Area), by = 'Species']
dtIris[
  , list(areaMin = min(Sepal.Area), areaMax = max(Sepal.Area)),
  by = 'Species'
  ]

# use plyr
install.packages('plyr')
library('plyr')
funcDl <- function(dtChunk){
  result <- mean(dtIris$Sepal.Length)
  return(result)
}
listIris <- dlply(
  .data = iris,
  .variables = 'Species',
  .fun = funcDl
)
names(listIris)
listIris$setosa


