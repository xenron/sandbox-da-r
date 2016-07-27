##################
# Code Snipped-1
##################

# Before running the following command we need to set the data
# location using setwd(). For example setwd("d:/chap2").

anscombe <- read.csv("CSVanscombe.csv",skip=2)

##################
# Code Snipped-2
##################

# import csv file that contains both numeric and character variable
# firstly using default and then using stringsAsFActors=FALSE

iris_a <- read.csv("iris.csv")
str(iris_a)

##################
# Code Snipped-3
##################

# Now using stringsAsFactors=FALSE
iris_b <- read.csv("iris.csv",stringsAsFactors=F)
str(iris_b)

##################
# Code Snipped-4
##################

iris_semicolon <- read.csv("iris_semicolon.csv",stringsAsFactors=FALSE,sep=";")
str(iris_semicolon)

##################
# Code Snipped-5
##################

anscombe_tab <- read.csv("anscombe.txt",sep="\t")
anscombe_tab_2 <- read.table("anscombe.txt",header=TRUE)

##################
# Code Snipped-6
##################
# Calling xlsx library
library(xlsx)
# importing xlsxanscombe.xlsx
anscombe_xlsx <- read.xlsx2("xlsxanscombe.xlsx",sheetIndex=1)

##################
# Code Snipped-7
##################

# loading robjects.RData file
load("robjects.RData")

# to see whether the objects are imported correctly
objects()

##################
# Code Snipped-8
##################

library(foreign)
iris_stata <- read.dta("iris_stata.dta")

##################
# Code Snipped-9
##################

# creating an R objects whose value is "datamanipulation"
char.obj <- "datamanipulation"

# creating a factor variable by extracting each single letter from the
# character string. To extract each single letter substring() function
# has been used. Note: nchar() function give number of character count
# in an character type R object
factor.obj <- factor(substring(char.obj,1:nchar(char.obj),1:nchar(char.obj)),levels=letters)

# Displaying levels of the factor variable
levels(factor.obj)

# Displaying the data using table() function
table(factor.obj)


##################
# Code Snipped-10
##################

# re-creating factor variable from existing factor variable
factor.obj1 <- factor(factor.obj)

# Displaying levels of the new factor variable
levels(factor.obj1)

# displaying data using table() function
table(factor.obj1)
factor.obj1


##################
# Code Snipped-11
##################

# creating a numeric variable by taking 100 random numbers
# from normal distribution
set.seed(1234) # setting seed to reproduce the example
numvar <- rnorm(100)

# creating factor variable with 5 distinct category

num2factor <- cut(numvar,breaks=5)
class(num2factor)
levels(num2factor)
table(num2factor)

##################
# Code Snipped-11
##################

# creating factor with given labels
num2factor <- cut(numvar,breaks=5,labels=c("lowest group","lower middle group", "middle group", "upper middle", "highest group"))

# displaying the data is tabular form
data.frame(table(num2factor))

# creating factor variable using conditional statement
num2factor <- factor(ifelse(numvar<=-1.37,1,ifelse(numvar<=-0.389,2,ifelse(numvar<=0.592,3,ifelse(numvar<=1.57,4,5)))),labels=c("(-2.35,-1.37]", "(-1.37,-0.389]", "(-0.389,0.592]", "(0.592,1.57]",   "(1.57,2.55]"))

# displaying data using table function
table(num2factor)

##################
# Code Snipped-12
##################

# creating date object using built in as.Date() function
as.Date("1970-01-01")

# looking at the internal value of date object
as.numeric(as.Date("1970-01-01"))

# Second January 1970 is showing number of elapsed day is 1.
as.Date("1970-01-02")
as.numeric(as.Date("1970-01-02"))

##################
# Code Snipped-13
##################

# creating date object specifying format of date
as.Date("Jan-01-1970",format="%b-%d-%Y")

##################
# Code Snipped-14
##################

# loading lubridate package
library(lubridate)

# creating date object using mdy() function
mdy("Jan-01-1970")

##################
# Code Snipped-15
##################

# creating heterogeneous date object
hetero_date <- c("second chapter due on 2013, august, 24", "first chapter submitted on 2013, 08, 18", "2013 aug 23")

# parsing the character date object and convert to valid date
ymd(hetero_date)

##################
# Code Snipped-16
##################

hetero_date <- c("second chapter due on 2013, august, 24", "first chapter submitted on 2013, 08, 18", "23 aug 2013")
ymd(hetero_date)

##################
# Code Snipped-17
##################

# Creating date object using based R functionality
date <- as.POSIXct("23-07-2013",format = "%d-%m-%Y", tz = "UTC")
date

# extracting month from the date object
as.numeric(format(date, "%m"))

# manipulating month by replacing month 7 to 8
date <- as.POSIXct(format(date,"%Y-8-%d"), tz = "UTC")
date

# The same operation is done using lubridate package
date <- dmy("23-07-2013")
date

month(date)

month(date) <- 8
date

##################
# Code Snipped-18
##################

# accessing system date and time
current_time <- now()
current_time

# changing time zone to "GMT"
current_time_gmt <- with_tz(current_time,"GMT")
current_time_gmt

# rounding the date to nearest day
round_date(current_time_gmt,"day")

# rounding the date to nearest month
round_date(current_time_gmt,"month")

# rounding date to nearest year
round_date(current_time_gmt,"year")

##################
# Code Snipped-19
##################

# creating a 10 element vector
num10 <- c(3,2,5,3,9,6,7,9,2,3)
# accessing 5th element
num10[5]

# checking whether there is any value of num10 object greater than 6
num10>6

# keeping only values greater than 6
num10[num10>6]

# use of negative subscript removes first element "3"
num10[-1]

##################
# Code Snipped-20
##################

# creating a data frame with 2 variables
data_2variable <- data.frame(x1=c(2,3,4,5,6),x2=c(5,6,7,8,1))

# accessing only first row
data_2variable[1,]

# accessing only first column
data_2variable[,1]

# accessing first row and first column
data_2variable[1,1]

##################
# Code Snipped-21
##################

 list_obj<- list(dat=data_2variable,vec.obj=c(1,2,3))
 list_obj
 
 # accessing second element of the list_obj objects
list_obj[[2]]
list_obj[[2]][1]

# accessing dataset from the list object
list_obj$dat
