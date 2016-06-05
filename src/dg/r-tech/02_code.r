
## read.table 函数常用的重要参数讲解

##########################################################
# 参数：file（文件名，字符串）
read.table(file = "d:/data/lesson2/Data1.csv")
read.table(file = "d:\\data\\lesson2\\Data1.csv")
read.table(file = "d:/data\\lesson2\\Data1.csv")

read.table(file = "d:\data\lesson2\Data1.csv")

read.table("http://....txt")


##########################################################
# 参数：header (TRUE 默认FALSE)
read.table(file = "d:/data/lesson2/Data2.csv",header = FALSE)
read.table(file = "d:/data/lesson2/Data2.csv",header = TRUE)

read.table(file = "d:/data/lesson2/Data2.csv")


##########################################################
# 参数：sep (分隔符,默认空格键“ ”，“,”, "\t"等)
read.table(file = "d:/data/lesson2/Data1.csv",sep=",")



##########################################################
# 参数：dec
read.table(file = "d:/data/lesson2/Data3.csv",sep=",",dec=',',header = TRUE)


##########################################################
# 参数：quote

read.table(file = "d:/data/lesson2/Data4.csv",sep=" ",quote= " ")


##########################################################
# 参数：row.names

read.table(file = "d:/data/lesson2/Data5.csv",sep=",",dec=',',header = TRUE,
           row.names=c("a1","a2","a3","a4","a5"))

read.table(file = "d:/data/lesson2/Data5.csv",sep=",",dec=',',header = TRUE,
           row.names="F")



##########################################################
# 参数:col.names

read.table(file = "d:/data/lesson2/Data5.csv",sep=",",dec=',',header = TRUE,
           col.names=c("a1","a2","a3","a4","a5","a6"))







#####################################################################################################
# 案例1:有300个的csv格式的数据，每个csv文件代表每1天的数据，不同csv文件包含全部相同或部分相同的字段，
#+      现在需要在这300个文件中提取全部特定的字段数据，返回数据框。

# 观察数据
oneData<- read.csv("d:/data/lesson2/Data50/Data_201.csv")
oneData<- read.csv("d:/data/lesson2/Data50/Data_202.csv")

oneDataNames<-names(oneData)
dataNames<-c("V3","V5","V56","V90","V180")
bothNames<-intersect(dataNames,oneDataNames)
bothNames

newData<-matrix(NA,nrow=nrow(oneData),ncol = length(dataNames))
colnames(newData)<-dataNames

for(i in 1:length(bothNames)){
  newData[,bothNames[i]]<-oneData[,bothNames[i]]
}

head(oneData)
head(newData)



# getDatas子函数

getDatas<-function(dataNames,oneData){
  oneDataNames<-names(oneData)
  bothNames<-intersect(dataNames,oneDataNames)
  newData<-matrix(NA,nrow=nrow(oneData),ncol = length(dataNames))
  colnames(newData)<-dataNames
  
  for(i in 1:length(bothNames)){
    newData[,bothNames[i]]<-oneData[,bothNames[i]]
  }
  return(newData)
}

# getAllDatas主函数

getAllDatas<- function(dataNames,DataDir){
  FileNames<-list.files(DataDir,full.names = T)
  dtlist<-lapply(FileNames,function(filename){
    oneData<-read.csv(filename)
    newData<-getDatas(dataNames=dataNames,oneData=oneData)
    return(newData)
  })
  dtcombined<-do.call(rbind,dtlist)
  return(dtcombined)
}



system.time(allDatas<-getAllDatas(dataNames=c("V3","V5","V56","V90","V180"),
                        DataDir="d:/data/lesson2/Data50/"))


system.time(allDatas<-getAllDatas(dataNames=c("V3","V5","V56","V90","V180"),
                        DataDir="d:/data/lesson2/Data1000/"))





































