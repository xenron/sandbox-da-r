# Chapter 1: Introduction to R Data Types and Basic Operations

#######################
#  Code block 1
#######################

#Writing commands in R
(44/55)*100
log(25)
log10(25)
exp(0.23)
453/365.25
1-5*0.2
1-0.2-0.2-0.2-0.2-0.2 # An interesting calculation

#######################
#  Code block 2
#######################

#Constant
2
"July"
NULL
NA
NaN
Inf

# Object can be created from existing object
# to make the result reproducible means every time we run the
# following code we will get the same results we need to set
# a seed value

set.seed(123)

# Random number generation from standard normal distribution
# and from  uniform distribution
rnorm(9)+runif(9)

#######################
#  Code block 3
#######################

# Storing R object into a variable and then viewing the mode
num.obj <- seq(from=1,to=10,by=2)
mode(num.obj)
logical.obj<-c(TRUE,TRUE,FALSE,TRUE,FALSE)
mode(logical.obj)
character.obj <- c("a","b","c")
mode(character.obj)

#######################
#  Code block 4
#######################

# R object containing both numeric and logical element
xz <- c(1, 3, TRUE, 5, FALSE, 9)
xz
mode(xz)

# R object containing character, numeric, and logical elements
xw <- c(1,2,TRUE,FALSE,"a")
xw
mode(xw)

#######################
#  Code block 5
#######################

num.obj <- seq(from=1,to=10,by=2)
logical.obj<-c(TRUE,TRUE,FALSE,TRUE,FALSE)
character.obj <- c("a","b","c")
is.numeric(num.obj)
is.logical(num.obj)
is.character(num.obj)
mode(mean)
# Also we can test whether "mean" is function or not as follows
is.function(mean)

#######################
#  Code block 6
#######################

num.obj <- seq(from=1,to=10,by=2)
logical.obj<-c(TRUE,TRUE,FALSE,TRUE,FALSE)
character.obj <- c("a","b","c")
class(num.obj)
class(logical.obj)
class(character.obj)

#######################
#  Code block 7
#######################

num.obj <- seq(from=1,to=10,by=2)
set.seed(1234) # To make the matrix reproducible
mat.obj <- matrix(runif(9),ncol=3,nrow=3)
mode(num.obj)
mode(mat.obj)
class(num.obj)
class(mat.obj)
# prints a numeric object
print(num.obj)
# prints a matrix object
print(mat.obj)


#######################
#  Code block 8
#######################

character.obj <- c("a","b","c")
character.obj
is.factor(character.obj)
# Converting character object into factor object using as.factor()
factor.obj <- as.factor(character.obj)
factor.obj
is.factor(factor.obj)
mode(factor.obj)
class(factor.obj)

#######################
#  Code block 9
#######################

#R object structure and mode conversion
# creating vector of numeric element with "c" function
num.vec <- c(1,3,5,7)
num.vec
mode(num.vec)
class(num.vec)
is.vector(num.vec)
# Vector with mixed elements 
num.char.vec <- c(1,3,"five",7)
num.char.vec
mode(num.char.vec)
class(num.char.vec)
is.vector(num.char.vec)


#######################
#  Code block 10
#######################

# combining multiple vectors
comb.vec <- c(num.vec,num.char.vec)
mode(comb.vec)
# creating named vector
named.num.vec <- c(x1=1,x2=3,x3=5)
named.num.vec
# vector of single element
unit.vec <- 9
is.vector(unit.vec)



#######################
#  Code block 11
#######################

# creating a vector of numbers and then converting it to logical 
# and character
numbers.vec <- c(-3,-2,-1,0,1,2,3)
numbers.vec
num2char <- as.character(numbers.vec)
num2char
num2logical <- as.logical(numbers.vec)
num2logical
# creating character vector and then convert it to numeric and logical
char.vec <- c("1","3","five","7")
char.vec
char2num <- as.numeric(char.vec)
char2num
char2logical <- as.logical(char.vec)
char2logical
# logical to character conversion
logical.vec <- c(TRUE, FALSE, FALSE,  TRUE,  TRUE)
logical.vec
logical2char <- as.character(logical.vec)
logical2char


#######################
#  Code block 12
#######################

#Vector
age <- c(10,20,30,40)
age[1]
height<- 175
height[2]<- 180
height[3] <- 165
height
name<- c("Rob", "Bob", "Jude","Monica")
logical<- c(TRUE, FALSE, TRUE, FALSE)
age[logical]


#######################
#  Code block 13
#######################

#Factor and its types
#creating factor variable with only one argument with factor() 
factor1 <- factor(c(1,2,3,4,5,6,7,8,9))
factor1
levels(factor1)
labels(factor)
labels(factor1)
#creating factor with user given levels to display
factor2 <- factor(c(1,2,3,4,5,6,7,8,9),labels=letters[1:9])
factor2
levels(factor2)
labels(factor2)


#######################
#  Code block 14
#######################

# creating numeric factor and trying to find out mean
num.factor <- factor(c(5,7,9,5,6,7,3,5,3,9,7))
num.factor
mean(num.factor)

#######################
#  Code block 15
#######################

num.factor <- factor(c(5,7,9,5,6,7,3,5,3,9,7))
num.factor
#as.numeric() function only returns internal values of the factor
as.numeric(num.factor)
# now see the levels of the factor
as.character(num.factor)
# now to convert the "num.factor" to numeric there are two method
# method-1: 
mean(as.numeric(as.character(num.factor)))
# method-2:
mean(as.numeric(levels(num.factor)[num.factor]))

#######################
#  Code block 16
#######################

#Data frame
#creating vector of different variables and then creating data frame
var1 <- c(101,102,103,104,105)
var2 <- c(25,22,29,34,33)
var3 <- c("Non-Diabetic", "Diabetic", "Non-Diabetic", "Non-Diabetic", "Diabetic")
var4 <- factor(c("male","male","female","female","male"))
# now we will create data frame using two numeric vectors one 
# character vector and one factor
diab.dat <- data.frame(var1,var2,var3,var4)
diab.dat

#######################
#  Code block 17
#######################

#class of each column before creating data frame 
class(var1)
class(var2)
class(var3)
class(var4)
# class of each column after creating data frame
class(diab.dat$var1)
class(diab.dat$var2)
class(diab.dat$var3)
class(diab.dat$var4)
# now create the data frame specifying as.is=TRUE
diab.dat.2 <- data.frame(var1,var2,var3,var4,stringsAsFactors=FALSE)
diab.dat.2
class(diab.dat.2$var3)

#######################
#  Code block 18
#######################

# To run the folloing code snipped, 
# the code block 16 need to run.
# Especially var1 var2 var3 and var4. 
# After that, from code clock 17 "diab.dat.2" object should run

# The following line will remove var1 to var4 
# object from the workspace
rm(var1);rm(var2);rm(var3);rm(var4)
# The following command will allow 
# us to access individual variables 
attach(diab.dat.2)
# Printing valuse of var1
var1
# checking calss of var3
class(var3)
# Now to detach the data frame from the workspace
detach(diab.dat.2)
# Now if we try to print individual varaiable it will give error
var1

#######################
#  Code block 19
#######################

#Matrices
# data frame to matrix conversion
mat.diab <- as.matrix(diab.dat)
mat.diab
class(mat.diab)
mode(mat.diab)
# matrix multiplication is not possible with this newly created matrix
t(mat.diab) %*% mat.diab
# creating a matrix with numeric elements only
# To produce the same matrix over time we set a seed value
set.seed(12345)
num.mat <- matrix(rnorm(9),nrow=3,ncol=3)
num.mat
class(num.mat)
mode(num.mat)
# matrix multiplication
t(num.mat) %*% num.mat

#######################
#  Code block 20
#######################

mat.array=array(dim=c(2,2,3))
# To produce the same results over time we set a seed value
set.seed(12345)
mat.array[,,1]<-rnorm(4)
mat.array[,,2]<-rnorm(4)
mat.array[,,3]<-rnorm(4)
mat.array

#######################
#  Code block 21
#######################

var1 <- c(101,102,103,104,105)
var2 <- c(25,22,29,34,33)
var3 <- c("Non-Diabetic", "Diabetic", "Non-Diabetic", "Non-Diabetic", "Diabetic")
var4 <- factor(c("male","male","female","female","male"))
diab.dat <- data.frame(var1,var2,var3,var4)
mat.array=array(dim=c(2,2,3))
set.seed(12345)
mat.array[,,1]<-rnorm(4)
mat.array[,,2]<-rnorm(4)
mat.array[,,3]<-rnorm(4)
# creating list
obj.list <- list(elem1=var1,elem2=var2,elem3=var3,elem4=var4,elem5=diab.dat,elem6=mat.array) 
obj.list


#######################
#  Code block 22
#######################

#Missing values in R
missing_dat <- data.frame(v1=c(1,NA,0,1),v2=c("M","F",NA,"M"))
missing_dat
is.na(missing_dat$v1)
is.na(missing_dat$v2)
any(is.na(missing_dat))


