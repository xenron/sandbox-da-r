#####################################################################################################
## 案例1：有a,b两个向量，它们中的每个元素逐一相减，并求相减结果的最小值。

a<-c(1:6)
b<-c(5,16,7,28,12,64,34,2,26)
mat<-matrix(NA,nrow = length(a),ncol = length(b))

for(i in 1:length(a)){
  mat[i,]<-a[i]-b
}

MIN<-apply(mat,1,min)


mat2<-outer(a,b,FUN="+")
MIN2<-apply(mat,1,min)



#####################################################################################################
## 案例2：随机生成一个矩阵，求每行的p.value值。

set.seed(2)
dat<-matrix(rnorm(120000*100),120000)

system.time(
p1 <- apply(dat, 1, function(x){
  t.test(x)$p.value
}))

t.test(dat[1,])$p.value

system.time({
  p2<- 2*pt(apply(dat, 1, function(x,mu=0){
    -abs((mean(x) - mu)/sqrt(var(x)/ncol(dat)))
  }),ncol(dat)-1)
})


identical(p1,p2)
which(p1!=p2)

p1[96952]
p2[96952]


#####################################################################################################
## 案例3：随机生成一个矩阵，采用更快的方法，求每行的p.value值。

getwd()

writeLines("#include <math.h>
           void calc_tstat(double *x, int *nr, int *nc, double *mu, double *tstat)
           {
           int i, j;
           double sum = 0.0, sum2 = 0.0, mean, var;
           for (i = 0; i < *nr; i++) {
           for (j = 0; j < *nc; j++) {
           sum += x[i + j * *nr];
           }
           mean = sum / (double) *nc;
           sum = 0.0;
           for (j = 0; j < *nc; j++) {
           sum2 += (x[i + j * *nr] - mean) * (x[i + j * *nr] - mean);
           }
           var = sum2 / (double) (*nc - 1);
           sum2 = 0.0;
           tstat[i] = (mean - *mu) / sqrt(var / (*nc - 1));
           }
           }", "calc_tstat.c")

system("R CMD SHLIB calc_tstat.c")

set.seed(2)
dat<-matrix(rnorm(120000*100),120000)
nr<-nrow(dat)
nc<-ncol(dat)

dyn.load(sprintf("calc_tstat%s",.Platform$dynlib.ext))

system.time({
 p3<-   2 * pt(-abs(.C("calc_tstat", as.double(dat), nr, nc,
                           0, double(nrow(dat)))[[5]]), nc - 1)
 
})

identical(p2,p3)
which(p2!=p3)

p2[9878]
p3[9878]





#####################################################################################################
# 案例4:有2000个的csv格式的数据，每个csv文件代表每1天的数据，不同csv文件包含全部相同或部分相同的字段，
#+      现在需要在这2000个文件中提取全部特定的字段数据，返回数据框。

## 上一课code
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



system.time(allDatas<-getAllDatas(dataNames=c("V3","V15","V56","V90","V180"),
                                  DataDir="d:/data/lesson2/Data50/"))




## 本周code：借用data.table包中的fread函数读取数据
library(data.table)

# 完全包含
filename<-"d:/data/lesson2/Data50/Data_201.csv"
dataNames=c("V3","V15","V56","V90")
oneDataNames<-names(fread(filename,nrow=1))
colIn<- dataNames %in% oneDataNames

dt1<- fread(filename,select = dataNames)


# 部分包含
filename<-"d:/data/lesson2/Data50/Data_201.csv"
dataNames=c("V3","V15","V56","V180")
oneDataNames<-names(fread(filename,nrow=1))
colIn<- dataNames %in% oneDataNames

dt1<- fread(filename,select = dataNames[colIn])
mat1<- matrix(NA,nrow = nrow(dt1),ncol = sum(!colIn))
dtleft<-as.data.table(mat1)
setnames(dtleft,names(dtleft),dataNames[!colIn])
dtleft<-cbind(dt1,dtleft)


# 完全不包含
return(NULL)


# 整合代码如下：
# 子函数getDatas2
getDatas2<- function(dataNames,filename){
  oneDataNames<-names(fread(filename,nrow=1))
  colIn<- dataNames %in% oneDataNames
  
  if(all(colIn)){
    dt1<- fread(filename,select = dataNames) 
    return(dt1)
  }
   
  if(any(colIn)){
    dt1<- fread(filename,select = dataNames[colIn])
    mat1<- matrix(NA,nrow = nrow(dt1),ncol = sum(!colIn))
    dtleft<-as.data.table(mat1)
    setnames(dtleft,names(dtleft),dataNames[!colIn])
    dtleft<-cbind(dt1,dtleft) 
    return(dtleft)
  }else return(NULL)
}

# 主函数getAllDatas2
getAllDatas2<-function(dataNames,DataDir){
  library(data.table)
  FileNames<-list.files(DataDir,full.names = T)
  dtlist<-lapply(FileNames,function(filename){
    newData<-getDatas2(dataNames=dataNames,filename=filename)
    return(newData)
  })
  
  dtcombined<-rbindlist(dtlist)
  return(as.data.frame(dtcombined))
}



system.time(allDatas2<-getAllDatas2(dataNames=c("V3","V15","V56","V90","V180"),
                                  DataDir="d:/data/lesson3/Data2000/"))




which(allDatas!=allDatas2)
class(allDatas)
class(allDatas2)






#####################################################################################################
## 案例5:比较使用OpenBLAS前后矩阵运算速度快慢。

mat<- matrix(1:(4000*4000),4000)
system.time(temp<- mat%*%mat)
























