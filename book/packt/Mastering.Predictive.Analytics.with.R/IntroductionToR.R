####################
# Introduction to R
####################

71 - 54
37.2 * 1.02
6 / 7

2 ^ 4
13 %/% 2
13 %% 2

ceiling(7.8)
trunc(2.32)
round(1.234, 2)
signif(1.234, 2)

choose(5, 2)
factorial(4)
choose(5,2)*factorial(2)

# String Functions
toupper("Hello World")
tolower("Hello World")
substr("Hello World", 1, 5)
nchar("Hello World")
paste("Hello", "World", sep = " ")

x <- 2
(x <- 4 * 56)
ls()
rm(x)
rm(list = ls())
save.image("myWorkspace.R")
setwd ("/Users/ruimiguelforte/R Workspaces")
getwd()

# Primitive Types
false_value <- F
typeof(false_value)

a <- c(1,2,3,4)
b <- c("red", "white", "blue")
c <- c(T, F, T)

is.numeric(5)
is.logical(c(T,F))
x <- NA
x == NA
is.na(x)

as.numeric("42")
as.character(TRUE)
as.logical(1)
as.logical(54)

(my_range <- 2 : 5)

# R Functions

seq(from = 2, to = 5)
seq(2,5)
seq(2,5,by = 0.5)

help(t.test)
?t.test

# Indexing Vectors

x <- c(0,1,1,2)
x[2]
x[1:2]
x[c(1,4)]
x[-1]
x[c(-1,-4)]
logical_index = c(F,T,F,T)
x[logical_index]

x[x > 0]
x > 0
which(x > 0)
x <- c(0,1,1,2)
x < -2
x<-2
> x <- c(0,1,1,2)
x[(x > 0) & (x%%2==1)]

# Functions on Vectors

2 * x
x <- c(0,1,1,2)
y <- c(9,6,7,3)
x + y
x > y

x <- c(0, 1)
y <- 3 : 8
x + y

x <- c(0,1,1,2)
length(x)
max(x)
min(x)
sum(x)
prod(x)

v<-c(1,2,3,4,NA,6)
sum(v)
sum(v,na.rm=T)
x <- c(0,1,1,2)

any(x>0)
all(x>0)

# Modifying Vectors

z <- c("white","yellow","red")
z[2] <- "green"
z <- c("white","yellow","red")
z[-2] = "green"
z <- c("white","yellow","red")
z[4] <- "teal"
z <- c("white","yellow","red")
z[6] <- "lime"
z[4:5] <- "magenta"
z[4:5] <- c("magenta","magenta")

# Matrices and Arrays

v <- 1:6
m <- matrix(v, nrow = 2, ncol = 3)
m <- matrix(v,nrow = 2, ncol = 3, byrow=TRUE)

v1 = c(1,3,5)
v2 = c(2,4,6)
rbind(v1,v2)
cbind(v1,v2)

(m <- rbind(v1,v2))
m["v2",]

rownames(m)
colnames(m)
colnames(m) <- c("a","b","c")
m

m[2,3]
m[1,]
m[1:2,2:3]
m[1:2,c(1,3)]
m[m>1]
ncol(m)
nrow(m)
dim(m)
m <- rbind(1:2,2:3)
n <- rbind(c(2,2,2),c(1,1,1))
m%*%n
a <- array(1:24,c(2,3,4))

# Lists

title <- "OP list"
a <- c(12, 23, 5, 0, 9)
b <- matrix(1:10, nrow=5)
c <- c("Some text", "Some more text")
l <- list(title=title, numbers = a, b, c)
l[[4]]
l$numbers

# Importing Data with Files
mydata <- read.table("c:/mydata.csv", header = TRUE, sep = ",")

# Data Frames

data(iris)
head(iris)
head(iris, n = 3)
iris[,1]
iris$Sepal.Length
str(iris)
names(iris)
names(iris)[1:4]=c("SLength","SWidth","PLength","PWidth")
head(iris)

teamID <-c(1,2,3,4,5)
teamName <- c("Greece", "Romania", "Portugal", "Brazil", "England")
fifaRanking <- c(15,29, 14, 11, 10)
qualified <- c(T, F, T, T, T)
worldCup2014 <- data.frame(teamID, teamName, fifaRanking, qualified , stringsAsFactors=FALSE)

newWorldCup2014 <- worldCup2014[order(worldCup2014$fifaRanking, worldCup2014$teamName),]

worldCup2014[6,] <- c(6, "Belgium", 12, T)
worldCup2014 <- rbind(worldCup2014,c(7, "Scotland", 22, F))
worldCup2014$wonWCBefore = c(F,F,F,T,NA,F,F)
worldCup2014[complete.cases(worldCup2014),]
fix(worldCup2014)
worldCup2014[worldCup2014$teamName=="England","wonWCBefore"] = T

iris$Species<-NULL
head(iris)
iris <- iris[-7,]
subset(iris,Sepal.Length > (2.5*Sepal.Width),select=c("Sepal.Length","Sepal.Width","Species"))

# Saving Data from a Data Frame to a Text File

write.table(iris,"iris.csv",sep=",",row.names=F)

# Further Data Frame Manipulation

data(mtcars)
mtcars<-t(mrcars)
head(mtcars[,1:4])
aggdata <-aggregate(mtcars, by=list(mtcars$cyl,mtcars$gear), FUN=mean, na.rm=TRUE)
head(aggdata)

attach(mtcars)
aggdata <-aggregate(mtcars, by=list(cyl,gear), FUN=mean, na.rm=TRUE)
detach(mtcars)

with(mtcars,aggregate(mtcars, by = list(cyl, gear), FUN = mean,na.rm = TRUE))

patientID=c(16,21,78,201,299,303,304)
patientType=c("In","Out","Out","Emergency","In","Out","In")
patientDoctorID=c(23,9,1,12,54,12,9)
patientData = data.frame(patientID, patientType, doctorID=patientDoctorID)
doctorID=c(1,9,12,23,54)
doctorFirstName=c("Edward", "Nicky", "Emma", "Joseph", "Heinrich")
doctorLastName=c("Fulton", "Gold", "Jameson", "Lee", "Grumblemeyer")
doctorData = data.frame(doctorID, doctorFirstName,doctorLastName)
mergedData = merge(patientData, doctorData, by="doctorID")

# Sampling data frames

v<-1:100
sample(v,5)
sample(v,10,replace=T)
data(iris)
iris_size <- nrow(iris)
train_size <- ceiling(iris_size*0.85)
iris_sampling_vector<-sample(seq_len(train_size),size=train_size)
iris_training <- iris[iris_sampling_vector,]
iris_test <- iris[-iris_sampling_vector,]
nrow(iris_training)
nrow(iris_test)

# Tables

table(worldCup2014$qualified, worldCup2014$wonWCBefore)

# Factors

continent <- c("Europe", "Europe", "Europe", "South America", "Europe", "Europe", "Europe")
continent <- as.factor(continent)
continent[8] = "Asia"
levels(continent)
levels(continent) <- c(levels(continent), "Asia")
continent[8] = "Asia"
continent = continent[1:3]
as.factor(continent)
factor(continent)
continent <- c(continent, "Europe")

# Basic Statistical Functions

x <- c(1,1,2,3,4,5,6,5,23,1,60)
mean(x)
sd(x)
median(x)
x <- c(1,1,2,3,4,5,6,5,23,1,60)
y <- c(0.5, 0.9,1.2,1.5,8.7,9.9,15.1,13.6,48.0,0.3,209.1)
cor(x,y)
cov(x,y)
summary(iris)

# Distribution Functions

pnorm(0, mean = 1, sd = 2)
pnorm(0)
pbinom(1,size=2,p=0.5)
ppois(2,lambda=2)
pnorm(0, mean = 1, sd = 2, lower.tail=F)
qnorm(0.5)

qbinom(0.5, size = 2, p = 0.7)
pbinom(1, size = 2, p = 0.7)

qbinom(0.75,size=2,p=0.5)
dnorm(0)
dbinom(1,size=2,p=0.5)
rnorm(5,mean=2,sd=0.5)
set.seed(345)
rnorm(5,mean=2,sd=0.5)
set.seed(345)
rnorm(5,mean=2,sd=0.5)

# Writing Functions

myMean<-5
mySD<-1.5
n<-20
rvector<-rnorm(n,mean=myMean,sd=mySD)

sum_of_squares<-function(x) {
  y<-x*x
  return(sum(y))
}

source(“square_this.R”)
sum_of_squares(c(1,2,3,4,5))

sum_of_squares<-function(x=0) {
  y<-x*x
  return(sum(y))
}

sum_of_squares()

foo <- function(x,y) {
  if (missing(y)) {
    y<-5;
  }
  o<-x+3*y;
  return(o);
}

foo(3,5)
foo(3)

# Debugging

my_mode <- function(x) {
  vtype <- typeof(x)
  y <- table(x)
  names <- names(y)
  return(as(names[y==max(y)],vtype))
}

my_mode(c(1,2,3,4,5,1,3,5,3))
my_mode(c(2:4,2:4))
my_mode(c("red", "green", "yellow", "green"))
my_mode(c(T,F,T,T))


table(c(2:4,2:4,NA,NA,NA,NA))
my_mode(c(NA,NA))
my_mode(c(2:4,2:4,NA,NA,NA,NA))

debug(my_mode)
my_mode(c(2:4,2:4,NA,NA,NA,NA))

my_mode <- function(x) {
  vtype <- typeof(x)
  y <- table(x, useNA="always")
  names <- names(y)
  return(as(names[y==max(y)],vtype))
}

my_mode(c(2:4,2:4,NA,NA,NA,NA))
my_mode(c(NA))
my_mode(c(1,1,NA))
my_mode(list(1,1:3))
traceback()

# Conditional Statements

x<-3
y<--1
if (y < 0) y<-0
if (x < -5) {
x<-0
y<--5
}

x<-3
if (x<0) x<-0 else x<-x+1

x<-rnorm(20,mean=0, sd=2)
y<-ifelse(x<0,1,-1)

# Loops in R

for (i in 1:10) print ("Hello")
i = 1;
while (i<=10) {
 print("Hello")
 i<-i+1
}

x1<-10:19
x2<-2:11
v1<-2*x1*x2-30
v2=c()
for (i in 1:10) {
 v2[i]<-2*x1[i]*x2[i]-30
}

# Apply Functions

sapply(rnorm(20,mean=0, sd=2),sign)
v<-1:10
sapply(v,function(x) x^2)
sapply(v,function(x) {y<-x+1; y*2})

raise_power<-function(x,power) {
 return(x^power)
}
v<-1:10
sapply(v, raise_power, power=2)
sapply(v, raise_power, power=3)

lapply(1:3, function(x) x^3)
data(iris)
apply(iris[1:4],2,mean)
(m <- matrix(1:24,6,4))
apply(m, 2, function(x) sapply(x, function(y) y^2))

tapply(iris$Petal.Length, iris$Species, mean)
with(mtcars, tapply(mpg, list(gear, cyl), max))
with(mtcars, tapply(mpg, list(gear, cyl), max, na.rm=TRUE))
mapply(prod, iris$Petal.Length[1:10], iris$Petal.Width[1:10])










