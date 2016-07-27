###################################################################################################
#Codes used in Chapter 2 of book Learning Bayesian Models with R                                  #
#Author Hari M. Koduvely                                                                          #
###################################################################################################

#section - Your first R program
x <- 2
y <- 3
z <- x + y
print(z)

#section - Managing data in R

#1 Vector
v <- vector("integer",10)
v
v[5] <- 1
v

#2 list
l <- list(1L,2L,3,4,"a","b")
str(l)

#3 function c()
c(1,3,6,2,-1)

#4 matrix
m <- matrix(c(1:9),nrow=3,ncol=3)

#Section - Slicing and dicing datasets

#1 Using single bracket
x  <- c(10,20,30,40,50)
x[1:3]

x[x > 25]

f  <- x > 30
x[f]

#2 subsetting a matrix
m <- matrix(c(1:9),nrow=3,ncol=3)
m[1 ,] #select the entire first row
m[  ,2] #select the entire second column


#3 Using double bracket
y <- list("a","b","c","d","e")
y[1]
class(y[1])

y[[1]]
class(y[[1]])

#4 Using $ sign
z <- list(John = 12 ,Mary = 18,Alice = 24 ,Bob = 17 ,Tom = 21)
z$Bob 

#5 Filtering using negative index values
y <- z[c(-2,-4)]
y

#6 Vectorized operations
x <- c(1,2,3,4,5)
y <- c(10,20,30,40,50)
z <- x+y
z

w <- x*y
w

#Section - Writing R programs

#1 Functions
myMean <- function(x){
  s <- sum(x)
  l <- length(x)
  mean <- s/l
  mean
}

x <- c(10,20,30,40,50)
myMean(x)

#2 Scoping Rules
x <- 0.1
f <- function(y){
  x*y
}

g <- function(y){
  x <- 5
  x <- f(y)
}

g(10)


#3 Loop functions

#3.1 lapply
X <- list(HP=c(12.5,14.3,16.1,15.4),IBM=c(22,24.5,23.7,26.2),Dell=c(8.9,9.7,10.8,11.5),Oracle=c(20.5,22.7,21.8,24.4))
lapply(X,mean)

#3.2 sapply
sapply(X,mean,simplify="array")

#3.3 mapply
rep(x=10,times=5)
mapply(rep,x=c(10,20,30,40,50),times=1:5)

#3.4 apply
Y <- matrix(1:9,nrow=3,ncol=3)
apply(Y,1,sum) #sum along the row
apply(Y,2,sum) #sum along the column

#3.5 tapply
X <- c(HP=c(12.5,14.3,16.1,15.4),IBM=c(22,24.5,23.7,26.2),Dell=c(8.9,9.7,10.8,11.5),Oracle=c(20.5,22.7,21.8,24.4)  )
f <- factor(rep(c("Q1","Q2","Q3","Q4"),times=4))
tapply(X,f,mean,simplify=TRUE)


#Section - Data Visualization

#Import Iris data set available as sample data sets in R
data(iris)
str(iris)
plot(iris$Petal.Width, iris$Petal.Length, col = "blue", xlab = "X", ylab = "Y")
title(main = "Plot of Iris Data", sub = "Petal Length (Y) Vs Petal Width (X)")
fitlm <- lm(iris$Petal.Length ~ iris$Petal.Width)
abline(fitlm[1],fitlm[2], col = "red")

#Section - Sampling

#1 Uniform random numbers
runif(5,1,10)

sample(1:100,10,replace=T)

#2 Sampling from Normal distribution
rnorm(5,mean=0,sd=1)
  

#Section - Exercises
#1 loading dataset
df_auto <- read.table("C:/Users/Hari Koduvely/SkyDrive/Documents/Book - BMLR/Data/auto-mpg.csv", sep=",", header=T)

#2 producing box plot
plot(df_auto$name, df_auto$mpg)

#3 funtion to produce scaled value
myscale <- function(x){
 y <- df_auto[,colnames(df_auto) == x]
 yscal <- (y - mean(y))/sd(y)
 yscal
}

myscale("hp")

#4 computing scaled value of all variables
cnames <- colnames(df_auto)
df_auto_scaled <- sapply(cnames[-length(cnames)], myscale, simplify = numeric())

#5 scatter plot of mpg vs each attribute in a single graph.
coplot(df_auto$accel ~ df_auto$mpg | df_auto$name)


