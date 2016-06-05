#####---lesson 6---#####

###-Vectorization
a<-1:10;a
b<-c(2,5,7,8,9,3,2,1,63,45);b
a+b
a-b
a*b
a/b
a[1:5]
a[1:10]  #get("a")
c<-c()
c[1:10]<-seq(1,10);c
D<-matrix(c(1:12),nrow=4);D
E<-matrix(c(22:33),nrow=4);E
D+E
D%*%t(E)

##-compare the time use
#-add & for
Add<-function(x,a){
  return(x+a)
}

For<-function(x,a){
  Temp<-rep(NA, length(x))  #定义临时变量
  for(i in 1:length(x)){
    Temp[i]<-x[i]+a    
  }
  return(Temp)
}

x<-rnorm(10000000)
a<-100
b<-Add(x,a)
#b
c<-For(x,a)
#c

system.time(b<-Add(x,a))
system.time(c<-For(x,a))

#-for & apply
For2<-function(m){
  Temp<-matrix(NA,nrow(m),ncol(m))
  for(i in 1:nrow(m)){
    Temp[i,]<-mean(m[i,])
  }
  return(Temp)
}

Apply<-function(m){
  Temp<-apply(m,1,mean)
  return(Temp)
}

m<-matrix(rnorm(10000000),nrow=1000,ncol=10000)
system.time(m1<-For2(m))
system.time(m2<-Apply(m))

#apply &snowfall
mysort<-function(x){
  replicate(10,sort(x))
  return(sort(x))
}  #定义一个多次排序函数，默认五次

Apply2<-function(m){
  Temp<-apply(m,2,mysort)
  return(Temp)
}

Snowfall<-function(m, ncl){
  library(snowfall)
  sfInit(parallel=T, cpus=ncl)   #初始集成环境
  Temp<-sfApply(m, 2, mysort)    #用sfApply函数对m矩阵的每一列进行多次排序
  sfStop()
  return(Temp)
}

m<-matrix(rnorm(10000000),nrow=10000,ncol=1000)
system.time(m3<-Apply2(m))
system.time(m4<-Snowfall(m,2))

##-the use of vectorization
##1
#计算缺失数据比例
Data1<- read.csv("D:/data/Data1.csv")
str(Data1)

# 一般的编程思路代码
NANum<- length(Data1[is.na(Data1)])
totalNum<- nrow(Data1)*ncol(Data1)
perNA<- NANum/totalNum
perNA

# 向量化编程思路代码
perNA<- mean(is.na(Data1))
perNA

##2
#反转过大或过小的数据
Data2<- c(rep(4,3),1:11,rep(5,3),5:10)
Data2

# 一般的编程思路代码
for(i in 1:length(Data2)){
  if(Data2[i]<=3 | Data2[i]>=8){
    Data2[i]<--Data2[i]
  }
}
Data2
# 向量化编程思路代码
Data2<- c(rep(4,3),1:11,rep(5,3),5:10)
Data2

NAIDX<- which(Data2<=3 | Data2>=8)
Data2[NAIDX]<- -Data2[NAIDX]
Data2

##3
#计算连续出现40次以上的数据编号始终及总长
Data3<- read.csv("D:/data/Data3 (2).csv")
str(Data3)

# 一般编程思路代码
x<-Data3
startNum<-1
endNum<-1
minSize<-40
count<-1
vTemp<-x$v[1]    #初始化

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
minSize<-40
x<-na.omit(x)
Index<-x$num

Judger1<-x$v[2:(nrow(x)-1)]!=x$v[1:(nrow(x)-2)]
Judger2<-x$v[2:(nrow(x)-1)] ==x$v[3:(nrow(x))]

start<-which(Judger1&Judger2)+1
end<-which((!Judger1)&(!Judger2))+1

ret<-data.frame(start=Index[start],end=Index[end],length=Index[end]-Index[start]+1)
print(ret[ret$length>=minSize,])

###-Advanced circulation

##-replication
rep(runif(1),5)
replicate(5,runif(1))

#traffic
time_for_commute <- function()
{
  # 选择当时所用的交通工具
  mode_of_transport <- sample(
    c("car", "bus", "train", "bike"),
    size = 1,
    prob = c(0.1, 0.2, 0.3, 0.4)
  )
  # 根据交通工具，找到出行的时间
  time <- switch(
    mode_of_transport,
    car = rlnorm(1, log(30), 0.5),
    bus = rlnorm(1, log(40), 0.5),
    train = rnorm(1, 30, 10),
    bike = rnorm(1, 60, 5)
  )
  names(time) <- mode_of_transport
  time
}
replicate(5,time_for_commute())


##-遍历列表
#prime_factor
prime_factors <- list(
  two = 2,
  three = 3,
  four = c(2, 2),
  five = 5,
  six = c(2, 3),
  seven = 7,
  eight = c(2, 2, 2),
  nine = c(3, 3),
  ten = c(2, 5)
)
head(prime_factors)

#for
unique_primes <- vector("list", length(prime_factors))
for(i in seq_along(prime_factors))
{
  unique_primes[[i]] <- unique(prime_factors[[i]])
}
names(unique_primes) <- names(prime_factors)
unique_primes

#lapply
lapply(prime_factors, unique)

#vapply
vapply(prime_factors, length, numeric(1))

#sapply
sapply(prime_factors, unique) # 返回一个列表
sapply(prime_factors, length) # 返回一个向量
sapply(prime_factors, summary) # 返回一个数组

sapply(list(),length)
#vapply(list(), length, numeric(1))

#complemented
complemented <- c(2, 3, 6, 18) 
lapply(complemented, rep.int, times = 4)

#rep4x
rep4x <- function(x) rep.int(4, times = x)
lapply(complemented, rep4x)

#lapply2
lapply(complemented, function(x) rep.int(4, times = x))

#eapply
env <- new.env()
env$molien <- c(1, 0, 1, 0, 1, 1, 2, 1, 3) 
env$larry <- c("Really", "leery", "rarely", "Larry")
eapply(env, length)
#or
lapply(env, length)

##-遍历数组
#matlab
install.packages("matlab")
library(matlab)   #detach("package:matlab")

(Magic<-magic(4))
rowSums(Magic)

#matlab_apply
apply(Magic,1,sum)  #rowSums(Magic)
apply(Magic，1，toString)
apply(Magic,2, toString)


(baldwins <- data.frame(
  name = c("Alec", "Daniel", "Billy", "Stephen"),
  date_of_birth = c(
    "1958-Apr-03", "1960-Oct-05", "1963-Feb-21", "1966-May-12"
  ),
  n_spouses = c(2, 3, 1, 1),
  n_children = c(1, 5, 3, 2),
  stringsAsFactors = FALSE
))
apply(baldwins, 1, toString)
apply(baldwins, 2, toString)

#matlab_sapply
sapply(baldwins, toString)

sapply(baldwins, range)


##-多个输入的应用函数
#mapply
msg <- function(name, factors)
{
  ifelse(
    length(factors) == 1,
    paste(name, "is prime"),
    paste(name, "has factors", toString(factors))
  )
}
mapply(msg, names(prime_factors), prime_factors)


##-Split-Apply-Combine
#Frogger
(frogger_scores <- data.frame(
  player = rep(c("Tom", "Dick", "Harry"), times = c(2, 5, 3)),
  score = round(rlnorm(10, 8), -1)
))

(scores_by_player <- with(
  frogger_scores,
  split(score, player)
))

(list_of_means_by_player <- lapply(scores_by_player, mean))

(mean_by_player <- unlist(list_of_means_by_player))

#better one
with(frogger_scores, tapply(score, player, mean))


##-plyr package
library(plyr)
llply(prime_factors, unique)

laply(prime_factors, length)
laply(list(), length)

raply(5, runif(1)) # 数组输出
rlply(5, runif(1)) # 列表给出
rdply(5, runif(1)) # 数据框输出
r_ply(5, runif(1)) # 丢弃输出

frogger_scores$level <- floor(log(frogger_scores$score))

ddply(
  frogger_scores,
  .(player),
  colwise(mean) # 除了player 之外，对每个列调用mean 函数
)

ddply(
  frogger_scores,
  .(player),
  summarize,
  mean_score = mean(score), # 对score 调用mean
  max_level = max(level) #... 然后求level 的max 值
)

###-recursion
##-quicksort
qs<-function(x){
  if(length(x)<=1) return(x)
  pivot<-x[1]
  therest<-x[-1]
  sv1<-therest[therest<pivot]
  sv2<-therest[therest>=pivot]
  sv1<-qs(sv1)
  sv2<-qs(sv2)
  return(c(sv1,pivot,sv2))
}

x<-c(5,4,12,13,3,8,88)
qs(x)
y<-c(56,5,7,78,96,32,16,57)
qs(y)