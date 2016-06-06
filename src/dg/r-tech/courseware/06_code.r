#############################################################################################
# 案例1：k个随机正态分布~（μ，σ）数据集，从中抽取一个长度为n样本，抽取方法是
#+       每次从1~k个数据集中抽一个数，循环n次，得到总样本n。


# 编程思路：
# - 根据信息表Mat随机产生6个正态分布数据集；
# - 将6个数据集合并成一个大数据集allData（n=1000行，k=6列），每一列代表一个数据集；
# - 每次随机抽取1个列号colID，然后从该列中随机抽取1个数字，并赋值到总样本Data中；
# - 循环n次，即得到总样本Data。

Mat<-read.csv("d:/data/lesson6/Mat.csv")
k<-6

allData<-numeric()
for(i in 1:k){
  tempData<-rnorm(1000,Mat[i,"Mean"],Mat[i,"SD"])
  allData<-cbind(allData,tempData)
}

n<-1000
Data<-numeric()
for(i in 1:n){
  colID<-sample(1:k,1,prob = Mat[,"Prob"])
  Data[i]<-sample(allData[,colID],1)
}

#### Method1 ####
Method1<-function(Mat,n){
  k<-nrow(Mat)
  
  allData<-numeric()
  for(i in 1:k){
    tempData<-rnorm(n,Mat[i,"Mean"],Mat[i,"SD"])
    allData<-cbind(allData,tempData)
  }
  
  Data<-numeric()
  for(i in 1:n){
    colID<-sample(1:k,1,prob = Mat[,"Prob"])
    Data[i]<-sample(allData[,colID],1)
  }
  return(Data)
}

Mat<-read.csv("d:/data/lesson6/Mat.csv")
Data<-Method1(Mat,n=1000)

# 验证：查看多次抽样后样本的重复率
set.seed(3)
Data1<-Method1(Mat,n=1000)
Data2<-Method1(Mat,n=1000)
Data3<-Method1(Mat,n=1000)
All<-c(Data1,Data2,Data3)
length(unique(All))/3000



# 编程思路2：
# - 在1~k中随机抽取一个数据集编号ID；
# - 根据数据集编号ID随机产生一个只有1个数的正态分布数据集，并赋值到总样本Data中；
# - 循环n次，即得到总样本Data。

Mat<-read.csv("d:/data/lesson6/Mat.csv")
n<-1000

Data<-numeric()
for(i in 1:n){
  ID<-sample(1:k,1,prob=Mat[,"Prob"])
  Data[i]<-rnorm(1,Mat[ID,"Mean"],Mat[ID,"SD"])
}

##### Method2 #####
Method2<-function(Mat,n){
  Data<-numeric()
  for(i in 1:n){
    ID<-sample(1:k,1,prob=Mat[,"Prob"])
    Data[i]<-rnorm(1,Mat[ID,"Mean"],Mat[ID,"SD"])
  }
  return(Data)
}

set.seed(3)
Data1<-Method2(Mat,n=1000)
Data2<-Method2(Mat,n=1000)
Data3<-Method2(Mat,n=1000)
All<-c(Data1,Data2,Data3)
length(unique(All))/3000






#############################################################################################
# 案例2：k个随机正态分布~（μ，σ）数据集，从中抽取一个长度为n样本，抽取方法是
#+       先随机把n个总样本数分到k个组，然后统一在该组中抽取特定的数。


# 编程思路：
# - 将1~k数据集号分到每一个要抽取的总样本数据编号(1~n)中；
# - 统计每个数据集需要抽取多少个数据；
# - 在每个数据集中抽取相应个数的数据；
# - 合并得到总样本Data。


Mat<-read.csv("d:/data/lesson6/Mat.csv")
n<-1000

Groups<-sample(1:k,n,prob = Mat[,"Prob"],replace = T)
SummaryGroups<-table(Groups)
ID<-sample(1:k,1,prob = Mat[,"Prob"])
Data<-sapply(1:k,function(num){
  rnorm(SummaryGroups[num],Mat[ID,"Mean"],Mat[ID,"SD"])
})

Data<-unlist(Data)


##### Method3 #####
Method3<-function(Mat,n){
  k<-nrow(Mat)
  
  Groups<-sample(1:k,n,prob = Mat[,"Prob"],replace = T)
  SummaryGroups<-table(Groups)
  ID<-sample(1:k,1,prob = Mat[,"Prob"])
  Data<-sapply(1:k,function(num){
    rnorm(SummaryGroups[num],Mat[ID,"Mean"],Mat[ID,"SD"])
  })
  
  Data<-unlist(Data)
  
  return(Data)
}

Mat<-read.csv("d:/data/lesson6/Mat.csv")
Data<-Method3(Mat,n=1000)


set.seed(3)
Data1<-Method3(Mat,n=1000)
Data2<-Method3(Mat,n=1000)
Data3<-Method3(Mat,n=1000)
All<-c(Data1,Data2,Data3)
length(unique(All))/3000


system.time(Method2(Mat,n=100000))
system.time(Method3(Mat,n=100000))











