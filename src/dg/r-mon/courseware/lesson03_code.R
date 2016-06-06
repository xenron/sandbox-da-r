#####---lesson 3---#####

###-Data Structure 2
##-data.frame
patientID<-c(1,2,3,4)
age<-c(25,34,28,52)
diabetes<-c("Type1","Type2","Type1","Type1")
status<-c("Poor","Improved","Excellent","Poor")
patientdata<-data.frame(patientID, age, diabetes, status)
patientdata

patientdata[1:2]
patientdata[c("diabetes","status")]
patientdata$age
table(patientdata$diabetes, patientdata$status)

#attach(), detach() and with()
summary(mtcars$mpg)
plot(mtcars$mpg,mtcars$disp)
plot(mtcars$mpg,mtcars$wt)

attach(mtcars)
summary(mpg)
plot(mpg,disp)
plot(mpg,wt)
detach(mtcars)

mpg<-c(23,2,5,345)
attach(mtcars)
plot(mpg, wt)    #an error appears

with(mtcars,{
  summary(mpg, disp, wt)
  plot(mpg, disp)
  plot(mpg, wt)
})

with(mtcars,{
  stats<-summary(mpg)
  stats
})
stats

with(mtcars,{
  stats<<-summary(mpg)
  stats
})
stats

#NA
pat2<-edit(patientdata)
pat2
complete.cases(pat2)
pat3<-pat2[complete.cases(pat2),];pat3
na.omit(pat2)     #or we can use na.omit

#rbind(), cbind()
patientdata
rbind(patientdata, list(1,35,"Type2","Excellent"))

math<-c(89,98,59,76)
chinese<-c(67,79,86,46)
english<-c(100,65,86,99)
grade<-data.frame(math, chinese,english);grade
class(grade)
grade<-cbind(grade, grade$math-grade$chinese);grade
class(grade)

#apply()
apply(grade,1,max)
apply(grade,1,mean)

#merge()
math2<-c(89,56)
chinese2<-c(69,78)
english2<-c(99,86)
grade2<-data.frame(math2, chinese2, english2);grade2
merge(grade, grade2)
physical<-c(100,89,96,90)
grade2<-data.frame(physical,math)
merge(grade, grade2)

##-factors
patientID<-c(1,2,3,4)
age<-c(25,34,28,52)
diabetes<-c("Type1","Type2","Type1","Type1")
status<-c("Poor","Improved","Excellent","Poor")
diabetes<-factor(diabetes)
status<-factor(status, order=T)
status<-factor(status, order=T,levels=c("Poor","Improved","Excellent"))
patientdata<-data.frame(patientID, age, diabetes, status)
str(patientdata)
summary(patientdata)

##-list
j<-list(name="Joe",salary=55000, union=T)
j <-list("Joe", 55000, T)

z<-vector(mode="list")
z[["abc"]]<-3

#index
j$salary
j[["salary"]]      #注意使用双重括号
j[[2]]
j[1:2]
j2<-j[2];j2
class(j2)
str(j2)

j[1:2]
j3<-j[1:2];j3
class(j3)

#增加删除列表元素
j
j$sex<-"M"    #增加性别
j

j[[5]]<-36
j[6:8]<-c(T,F,T);j

j$union<-NULL;j
c(list("Grace",5000,T),list(33))

#apply()
lapply(list(1:3,25:29),median)
sapply(list(1:3,25:29),median)


###-Input and Output
##-Workspace
getwd()
setwd("d:/Rworkspace")
ls()
savehistory("myhistory")
loadhistory("myhistory")
save.image("myfile")
load("myfile")
#q()

##-source() and sink()

source("script1.R")

sink("myputout",append=T,split=T)
pdf("mygraphs.pdf")
source("script1.R")

##-Data input and output
##input
#edit()
mydata<-data.frame(age=numeric(0),gender=character(0),weight=numeric(0))
mydata<-edit(mydata)

#csv and txt
abort<-read.csv("abort.csv",header=T)
abort

#Excel
install.packages("xlsx")
library(xlsx)
a<-read.xlsx2("mtcars.xlsx",1)
head(a)

install.packages("RODBC")
library(RODBC)
mydata<-odbcConnectExcel2007("mtcars.xlsx")
mydataframe<-sqlFetch(mydata,"Sheet1")
odbcClose(mydata)

#spss
install.packages("Hmisc")
library(Hmisc)
mydatafram<-spss.get("behavior.sav")
#sas
#foreign::read.ssd()
#Hmisc::sas.get()

#Stata
#foregin::read.dta()

#matlab
#R.matlab::readMat()
#R.matlab::writeMat()

#Web
#API
#WDI
install.packages("WDI")
library(WDI)
wdi_datasets <- WDIsearch()    #列出可用数据集
head(wdi_datasets)
wdi_trade_in_services <- WDI(               
  indicator = "BG.GSR.NFSV.GD.ZS"
)                             #取其中一个
str(wdi_trade_in_services)

#quantmod
install.packages("quantmod")
library(quantmod)
# 如果你正在使用0.5.0 之前的版本，那么请设置以下选项，
# 或者把参数auto.assign = FALSE 传给getSymobols。
options(getSymbols.auto.assign = FALSE)
microsoft <- getSymbols("MSFT")
head(microsoft)

#R自带url1
salary_url <- "http://www.justinmrao.com/salary_data.csv"
salary_data <- read.csv(salary_url)
str(salary_data)

#R自带url2
salary_url <- "http://www.justinmrao.com/salary_data.csv"
local_copy <- "my local copy.csv"
download.file(salary_url, local_copy)
salary_data <- read.csv(local_copy)



#RCurl
install.packages("RCurl")
library(RCurl)
time_url <- "http://tycho.usno.navy.mil/cgi-bin/timer.pl"
time_page <- getURL(time_url)
cat(time_page)

#XML
install.packages("XML")
library(XML)
time_doc <- htmlParse(time_page)
pre <- xpathSApply(time_doc, "//pre")[[1]]
values <- strsplit(xmlValue(pre), "\n")[[1]][-1]
strsplit(values, "\t+")

#httr
install.packages("httr")
library(httr)
time_page <- GET(time_url)
time_doc <- content(page, useInternalNodes = TRUE)

#ODBC
#DBI-SQLite
install.packages("DBI")
install.packages("RSQLite")
library(DBI)
library(RSQLite)
driver <- dbDriver("SQLite")
db_file <- system.file("extdata", "crabtag.sqlite", package = "learningr")
conn <- dbConnect(driver, db_file)

#DBI-MySQL
driver <- dbDriver("MySQL")
db_file <- "path/to/MySQL/database"
conn <- dbConnect(driver, db_file)

#DBI-SQLsearch
query <- "SELECT * FROM IdBlock"
(id_block <- dbGetQuery(conn, query))

dbDisconnect(conn)
dbUnloadDriver(driver)

#safe rewrite
query_crab_tag_db <- function(query)
{
  driver <- dbDriver("SQLite")
  db_file <- system.file(
    "extdata",
    "crabtag.sqlite",
    package = "learningr"
  )
  conn <- dbConnect(driver, db_file)
  on.exit(
{   # 此代码在函数之后运行，即使抛出一个错误
  dbDisconnect(conn)
  dbUnloadDriver(driver)
}
  )
dbGetQuery(conn, query)
}                    

query_crab_tag_db("SELECT * FROM IdBlock")

dbReadTable(conn, "idblock")

#RODBC
install.packages("RODBC")
library(RODBC)
conn <- odbcConnect("my data source name")
id_block <- sqlQuery(conn, "SELECT * FROM IdBlock")
odbcClose(conn)

##output
write.table(mtcars,"mtcars.txt",sep="\t")

library(xlsx)
write.xlsx(mtcars, "mtcars.xlsx")

write.csv(mtcars,"mtcars.csv")

install.packages("foreign")
library("foreign")
write.foreign(mtcars,"mtcars.txt","mtcars_code.sps",package="SPSS")
