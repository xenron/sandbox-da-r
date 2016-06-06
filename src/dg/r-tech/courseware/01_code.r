####################################################################
# 例1: + vs for

ADD<-function(x){
  return(x+1)
}

FOR<-function(x){
  xTemp<- rep(NA,length(x))
  for(i in 1:length(x)){
    xTemp[i]<-x[i]+1
  }
  return(xTemp)
}
x<-rnorm(10000000)
system.time(a<-ADD(x))
system.time(a<-FOR(x))



####################################################################
# 例2: for vs apply

FOR<-function(m){
  mTemp<- matrix(NA,nrow(m),ncol(m))
  for(i in 1:ncol(m)){
    mTemp[,i]<-sort(m[,i])
  }
  return(mTemp)
}

APPLY<- function(m){
  mTemp<- apply(m, 2, function(x){
    sort(x)
  })
  return(mTemp)
}

m<- matrix(rnorm(10000000),10000,1000)
system.time(m1 <- FOR(m))
system.time(m2 <- APPLY(m))


####################################################################
# 例3：apply vs snowfall

mysort<- function(x){
  replicate(5,sort(x))
  return(sort(x))
}

APPLY<- function(m){
  mTemp <- apply(m, 2, mysort)
  return(mTemp)
}

SNOWFALL<-function(m,ncl){
  library(snowfall)
  sfInit(parallel = TRUE, cpus = ncl)
  mTemp<- sfApply(m,2,mysort)
  sfStop()
  return(mTemp)
}

m <- matrix(rnorm(10000000), 100000, 100) 
system.time(m1 <- APPLY(m))              
system.time(m2 <- SNOWFALL(m, 2))        





####################################################################
# 案例1：在Data1.csv数据中，某些变量存在一些缺失的数据（NA），计算该
#+       数据的缺失数据的比例。

Data1<- read.csv("d:/data/Data1.csv")
str(Data1)

# 一般的编程思路代码
NANum<- length(Data1[is.na(Data1)])
totalNum<- nrow(Data1)*ncol(Data1)
perNA<- NANum/totalNum

# 向量化编程思路代码
perNA<- mean(is.na(Data1))





####################################################################
# 案例2：在数据Data2中，如果x<=3或x>=8,则将Data中的这些x值替换成NA

Data2<- c(rep(4,3),1:11,rep(5,3),5:10)
Data2

# 一般的编程思路代码
for(i in 1:length(Data2)){
  if(Data2[i]<=3 | Data2[i]>=8){
    Data2[i]<-NA
  }
}

# 向量化编程思路代码
NAIDX<- which(Data2<=3 | Data2>=8)
Data2[NAIDX]<- NA
Data2






#######################################################################
# 案例3：Data3.csv数据中只有两个变量，第一列（num）记录行号，
#+      第二列（v）记录数值，其中第二列有部分数值片段是连续不变的，找
#+      到连续不变且长度超过50的数值片段的起、始行号和片段的总长度。

Data3<- read.csv("d:/data/lesson1/Data3.csv")
str(Data3)

# 一般编程思路代码
x<-Data3
startNum<-1
endNum<-1
minSize<-50
count<-1
vTemp<-x$v[1]

for(i in 2:nrow(x)){
  Judger<-vTemp==x$v[i]
  if(Judger & (!is.na(Judger))){
    endNum<-x$num[i]
    count<-count+1
  }else if((!Judger) | is.na(Judger)){
    if(count>=minSize){
      print(paste("start=",startNum,"end=",endNum,"length=",count))
    }
    startNum<-x$num[i]
    endNum<-x$num[i]
    vTemp<-x$v[i]
    count<-1
  }
}




# 向量化编程思路代码
minSize<-50
x<-na.omit(x)
Index<-x$num

Judger1<-x$v[2:(nrow(x)-1)]!=x$v[1:(nrow(x)-2)]
Judger2<-x$v[2:(nrow(x)-1)] ==x$v[3:(nrow(x))]

start<-which(Judger1&Judger2)+1
end<-which((!Judger1)&(!Judger2))+1

ret<-data.frame(start=Index[start],end=Index[end],length=Index[end]-Index[start]+1)
print(ret[ret$length>=minSize,])

































