###########################################################################
# 案例1：Data1.csv数据文件有20个变量属性，请采用基于粗糙集的属性约简方法，
#+       对数据的属性进行约简，可以选择不同的决策属性。

# 查看数据
Data<-read.table("d:/data/lesson5/Data1.csv",sep=",", header = T)

# 数据离散化
Discretization<- function(data,method="distance",group){
  if(method=="distance"){
  grouplist<-lapply(data, function(x){
    if(length(unique(x))<=group) return(x)
    if(class(x)=='character'){
      x<-NA
      return(x)
    }
    
    groups<-cut(x,breaks=group)
    return(groups)
    
  })
    
  }else if(method=="frequency"){
    grouplist<-lapply(data,function(x){
      if(length(unique(x))<=group) return(x)
      
      if(class(x)=="character"){
        x<-NA
        return(x)
      }
      
      breaks<-quantile(x,seq(0,1,1/group))
      groups<-cut(x,breaks=breaks)
      return(groups)
    })
    
  }
  
  groupdf<-do.call(cbind,grouplist)
  return(groupdf)
}


# 调用RoughSetKnowledgeReduction包中函数进行属性约简
RoughSets<-function(Data,decisionColName="D",method="distance",group=5){
  
  library(RoughSetKnowledgeReduction)
  groupdf<-Discretization(data=Data,method=method,group=group)
  
  allName<-colnames(groupdf)
  conditionName<-allName[allName!=decisionColName]
  rankName<-c(conditionName,decisionColName)
  groupdf<-groupdf[,rankName]
  
  df<-new(Class = "DecisionTable",decisionTable=groupdf)
  cr<-findFirstConditionReduct(df)
  Id<-getColumnIds(cr)
  
  return(colnames(groupdf[,Id]))
  
}


Data<-read.table("d:/data/lesson5/Data1.csv",sep=",", header = T)
system.time(RoughSets(Data=Data,decisionColName="D",method="distance",group=3))
system.time(RoughSets(Data=Data,decisionColName="D",method="distance",group=6))





































