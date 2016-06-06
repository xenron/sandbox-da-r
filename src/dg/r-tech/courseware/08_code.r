#################################################################################
# 案例1：测试“第1课 向量化编程思维”案例3的代码，测试数据文件为Data1~Data5，
#+       撰写代码测试报告。

# 第一课向量化编程思维案例3代码

#Data3<- read.csv("d:/data/lesson1/Data3.csv")
Data3<- read.csv("d:/tmp/01_Data3.csv")
str(Data3)

start_end<-function(x,minSize){
  x<-na.omit(x)
  Index<-x$num
  
  Judger1<-x$v[2:(nrow(x)-1)]!=x$v[1:(nrow(x)-2)]
  Judger2<-x$v[2:(nrow(x)-1)] ==x$v[3:(nrow(x))]
  
  start<-which(Judger1&Judger2)+1
  end<-which((!Judger1)&(!Judger2))+1
  
  ret<-data.frame(start=Index[start],end=Index[end],length=Index[end]-Index[start]+1)
  ret<-ret[ret$length>=minSize,]
  return(ret)
}

start_end(x=Data3,minSize=50)

# fileNames<-list.files("d:/data/lesson8/Data1_5",full.names = T)
fileNames<-list.files("D:/tmp/08_Data1_5",full.names = T)

# 对一个文件结果进行标识
i<-1
Data<-read.csv(fileNames[i])
retTemp<-start_end(x=Data,minSize=50)
fileName<-rep(paste0("Data",i),nrow(retTemp))
retTemp<-cbind(fileName,retTemp)
row.names(retTemp)<-1:nrow(retTemp)
retTemp

# 测试函数Test
Test<-function(fileNames){
  allRet<-data.frame()
  for(i in 1:length(fileNames)){
    Data<-read.csv(fileNames[i])
    retTemp<-start_end(x=Data,minSize=50)
    fileName<-rep(paste0("Data",i),nrow(retTemp))
    retTemp<-cbind(fileName,retTemp)
    allRet<-rbind(allRet,retTemp)
    row.names(allRet)<-1:nrow(allRet)
  }
  write.csv(allRet,"d:/data/lesson8/Result.csv")
  return(allRet)
}

Test(fileNames[1:5])


# 测试函数Test2，tryCatch函数
Test2<-function(fileNames){
  allRet<-data.frame()
  for(i in 1:length(fileNames)){
    Data<-read.csv(fileNames[i])
    tryCatch(retTemp<-start_end(x=Data,minSize=50),error=function(e) e)
    tryCatch(fileName<-rep(paste0("Data",i),nrow(retTemp)),error=function(e) e)
    tryCatch(retTemp<-cbind(fileName,retTemp),error=function(e) e)
    tryCatch(allRet<-rbind(allRet,retTemp),error=function(e) e)
    tryCatch(row.names(allRet)<-1:nrow(allRet),error=function(e) e)
  }
  write.csv(allRet,"d:/data/lesson8/Result.csv")
  return(allRet)
}

Test2(fileNames[1:5])












