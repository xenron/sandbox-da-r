####R语言魔鬼训练营####
###第十周课程###
install.packages("snow")

###游程问题
source("D:/data/findruns.R")
findruns(c(1,0,0,1,1,0,1,1,1),2)
debug(findruns)
findruns(c(1,0,0,1,1,0,1,1,1),2)
###城市寻对
source("D:/data/cities.R")
m<-rbind(c(0,12,5),c(12,0,8),c(5,8,0))
m
debug(mind)
mind(m)
debug(imin)
mind(m)

###并行计算
sum=0
for (i in 0:(n-1))
  for(j in (i+1):(n-1))
    for(k in 0:(n-1))
      sum=sum+a[i,k]*a[j][k]
mean=sum/(n*(n-1)/2)


source("D:/data/SnowMutlinks.R")
library(snow)
cl<-makeCluster(type="SOCK",c("localhost","localhost"))
testm<-matrix(sample(0:1,16,replace=T),nrow=4)
mutlinks(cl,testm)



split(1:4,1:2)