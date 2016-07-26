##################
# Code block-1
##################

# Trying to create a vector of zero with length 2^32-1. Note that the RAM
# of the computer we are generating this example is 8GB with 64-bit Windows-7
# Professional edition.Processor core i5.

x <- rep(0, 2^31-1)
2^31

# If we try to assign a vector of length greater than maximum addressable
# length then that will produce NA

as.integer(2^31)

##################
# Code block-2
##################

# calling ODBC library into R
library(RODBC)

# creating connection with the database using odbc package and the connection
# we created earlier.

xldb<- odbcConnect("xlopen")

# In the odbcConnect() function the minimum argument required
# is the ODBC connection string.

# Now the connection created, using that connection we will import data

xldata<- sqlFetch(xldb, "CSVanscombe")

# Note here that "CSVanscombe"is the Excel worksheet name.

odbcClose(xldb) # closing the database connection

##################
# Code block-3
##################

# calling odbc library
library(RODBC)

# connecting with database
access_con<- odbcConnect("accessdata")

# import separate table as separate R data frame
coverage_page<- sqlFetch(access_con, "coverpage")
ques1 <- sqlFetch(access_con, "questionnaire1")
ques2 <- sqlFetch(access_con, "questionnaire2")

odbcClose(access_con) # closing the database connection
##################
# Code Snipped-4  for filehash package
##################

library(filehash)
dbCreate("exampledb")
filehash_db<- dbInit("exampledb")

dbInsert(filehash_db, "xx", rnorm(50))
value<- dbFetch(filehash_db, "xx")
summary(value)

dbInsert(filehash_db, "y", 4709)
dbDelete(filehash_db, "xx")
dbList(filehash_db)
dbExists(filehash_db, "xx")

filehash_db$x<- runif(100)
summary(filehash_db$x)
summary(filehash_db[["x"]])
filehash_db$y<- rnorm(100, 2)
dbList(filehash_db)

# To run the following line make sure the working directory is set properly.
# The working directory should be the folder where the file "anscombe.txt" is stored

dumpDF(read.table("anscombe.txt", header=T), dbName="massivedata")
massive_environment<- db2env(db="massivedata")

fit<- with(massive_environment, lm(Y1~X1))
with(massive_environment, summary(Y1))
with(massive_environment, Y1[1] <- 99)

##################
# Code block-5  for ff package
##################
library(ff)
file1 <- ff(filename="file1", length=10,vmode="double")
str(file1)

# calling rivers data
data(rivers)
file1[1:10] <- rivers[1:10]

# Note that here file1 is an ff object whereas
# file1[...] returns default R vector
str(file1)

# We can perform sampling if required on the ff objects:
# set seed to reproduce the example
set.seed(1337)
sample(file1,5,replace=FALSE)

gc()

##################
# Code block-6  for sqldf package
##################

# Selecting the rows from iris dataset where sepal length > 2.5
# and store that in subiris data frame

library(sqldf)
subiris<- sqldf("select * from iris where Sepal_Width> 3")
head(subiris)
nrow(subiris)

subiris2<- sqldf("select Sepal_Length,Petal_Length,Species from iris where Petal_Length> 1.4")
nrow(subiris2)

# Before running the following line, make sure the working directory is set properly
# import only Sepal width and Petal width along with species information where Petal width is greater than 0.4
iriscsv<-read.csv.sql("iris.csv",sql="select Sepal_Width,Petal_Width,Species from file where Petal_Width>0.4")
head(iriscsv)

# do not use underscore as within variable name it will give error, here is the example
iriscsv<-read.csv.sql("iris.csv",sql="select Sepal.Width,Petal.Width,Species from file where Petal.Width>0.4")

# we can draw a random sample of size 10 from iris data that are stored in iris.csv file.
iris_sample<- read.csv.sql("iris.csv",sql="select * from file order by random(*) limit 10")
iris_sample

# Calculate group wise mean from iris data
iris_avg<-sqldf("select Species, avg(Sepal_Length),avg(Sepal_Width),avg(Petal_Length),avg(Petal_Width) from iris group by Species")

colnames(iris_avg) <- c("Species","Sepal_L","Sepal_W","Petal_L","Petal_W")
iris_avg

# The base R counterpart to perform same operation is
aggregate(iris[,-5],list(iris$Species),mean)

