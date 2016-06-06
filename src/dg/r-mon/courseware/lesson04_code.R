#####---lesson 3---#####

###-look up our data
##-Labels
#-variable labels
patientID<-c(1,2,3,4)
age<-c(25,34,28,52)
gender<-c(1,2,1,1)
diabetes<-c("Type1","Type2","Type1","Type1")
status<-c("Poor","Improved","Excellent","Poor")
diabetes<-factor(diabetes)
status<-factor(status, order=T)
patientdata<-data.frame(patientID, age, gender, diabetes, status);patientdata
names(patientdata)[2]<-"Age at hospitalization (in years)"
patientdata

#-value labels
patientdata$gender<-factor(
  patientdata$gender,
  levels=c(1,2),
  labels=c("male","female"))
patientdata

##-summary of other functions
length(age)
dim(patientdata)
str(gender)
class(gender)
status
mode(status)
names(patientdata)
c(age,diabetes,status)
cbind(age, diabetes, status)
rbind(patientdata,c(5,34,"female","Type2","Poor"))
head(mtcars)
tail(mtcars)
ls(mtcars)
rm(gender);gender
newdata<-edit(patientdata)
fix(patientdata) #edit(patientdata)

###-data management
AWines<-read.csv("d:/data/AWines.csv",header=T)
AWines<-AWines[1:6,-8:-9]

##-Create variables
sumR<-AWines$Red+AWines$Fortified
meanR<-(AWines$Red+AWines$Fortified)/2
#1
AWines$sumR<-AWines$Red+AWines$Fortified
AWines$meanR<-(AWines$Red+AWines$Fortified)/2
#2
attach(AWines)
AWines$sumR<-Red+Fortified
AWines$meanR<-(Red+Fortified)/2
detach(AWines)
#3
AWines<-transform(AWines,sumR=Red+Fortified,meanR=(Red+Fortified)/2)

##-variable recodification
AWines$Red[AWines$Red >= 9999] <-NA
AWines$Redcat[AWines$Red<500]<-"Low sale"
AWines$Redcat[AWines$Red>=500 & AWines$Red <=1000]<-"middle sale"
AWines$Redcat[AWines$Red>1000]<-"High sale"

#or we can
AWines<-within(AWines,{
  Redcat<-NA
  Redcat[Red<500] <- "Low sale"
  Redcat[Red>=500 & Red<=1000] <- "Middle sale"
  Redcat[Red>1000] <- "High sale"
})

#car-recode
library(car)
x<-rep(1:3,3)
x
recode(x, "c(1,2)='A'; else='B'")
Recode(x, "1:2='A'; 3='B'")

recode(AWines$Red,"0:499='Low sale';500:1000='Middle sale';1001:9999='High sale'")

#doBy-recodeVar
library(doBy)
x <- c("dec","jan","feb","mar","apr","may")
src1 <- list(c("dec","jan","feb"), c("mar","apr","may"))
tgt1 <- list("winter","spring")
recodeVar(x,src=src1,tgt=tgt1)

recodeVar(AWines$Red,src=list(0:499,500:1000,1000:9999),tgt=list("Low sale","Middle sale","High sale"))

#cut
cut(AWines$Red,breaks=c(0,500,1000,9999),labels=c("Low sale","Middle sale","High sale"))

##-rename our variables
fix(AWines)
#rename()
install.packages("reshape")
library(reshape)
AWines<-rename(AWines, c(Month="Month-Year",Red="Red Wine"));AWines
#names()
names(AWines)[2]<-"Fortified Wine"
names(AWines)[4:5]<-c("Rose Wine","Sparkling Wine")

##-rank datas
AWines<-read.csv("D:/data/AWines.csv",header=T)
AWines<-AWines[1:6,-8:-9]
newAW<-AWines[order(AWines$Red),]

attach(AWines)
newAW<-AWines[order(-Fortified, Red),]
detach(AWines)

##-merge datas
merge()
cbind()
rbind()

##-subsets
newdata<-AWines[,c(5:7)];newdata

myvars<-c("sparkling","Sweet.white", "Dry.white")
newdata<-AWines[myvars]

myvars2<-names(AWines) %in% c("sparkling","Red")
newdata2<-AWines[!myvars2];newdata2

newdata3<-AWines[c(-3, -5)]

AWines$Red<-AWines$sparkling<-NULL

newdata<-AWines[1:3,]
newdata<-AWines[which(AWines$Fortified>=3100 & AWines$Red > 600),]

subset(AWines, Red>=500 & Red<=1000, select=Month:Rose)

mysample<- AWines[sample(1:nrow(AWines),3,replace=FALSE),];mysample

##-how to use SQL in R
install.packages("sqldf")
library(sqldf)
newdf<-sqldf("select * from mtcars where carb=1 order by mpg", row.names=T)
newdf
sqldf("select avg(mpg) as avg_mpg, avg(disp) as avg_disp,
      gear from mtcars where cyl in (4,6) group by gear")

