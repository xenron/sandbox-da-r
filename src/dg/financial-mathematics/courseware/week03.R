library(zoo)                    # for time series
library(timeSeries)             # for portfolio
library(timeDate)

##读取数据
data <- read.csv("E:/金融数学/idx000001.csv",header=T)
head(data,5)
clpr <- data$Clpr   #提取收盘价
date <- as.Date(data[,3])   #提取日期
clpr.ts <- zoo(clpr, date)                   # 改变变量的类型



#####单个股票收益计算#####
#提取年份数据
year<-format(date,"%Y")
mark=rep(0,length(date))
for(i in 1:(length(date)-1))
{
  if(year[i]!=year[i+1])
  {
    mark[i]=1
  } 
}
cls.year<-clpr[which(mark==1)]

#提取季度数据
qtr<-format(as.yearqtr(date),"%q")
mark=rep(0,length(date))
for(i in 1:(length(date)-1))
{
  if(qtr[i]!=qtr[i+1])
  {
    mark[i]=1
  } 
}
cls.qtr<-clpr.ts[which(mark==1)]

#提取月份数据
month<-format(date,"%m")
mark=rep(0,length(date))
for(i in 1:(length(date)-1))
{
  if(month[i]!=month[i+1])
  {
    mark[i]=1
  } 
}
cls.month<-clpr[which(mark==1)]

##提取周数据
week<-dayOfWeek(as.timeDate(date))
mark=rep(0,length(week))
for(i in 1:(length(week)))
{
  if(week[i]=="Mon")
  {
    mark[i]=1
  } 
}
cls.week<-clpr[which(mark==1)]


##简单收益率
SRet<-function(data)
{ 
 re<-100*diff(data)/data[-length(data)]
 return(re)
 
}
#returns(data,method="simple",percentage=T)

##对数收益率
LRet<-function(data)
{
  re <- 100 * log(1+SRet(data)/100)                      # use formula
 # re.1 <- 100 * diff(log(data))                                  # use function
 return(re)
}
#returns(data, method='continuous', percentage=TRUE)

##收益率计算
returns(cls.year,method="simple",percentage=T)
returns(cls.year,method="continuous",percentage=T)
returns(cls.qtr,method="simple",percentage=T)
returns(cls.qtr,method="continuous",percentage=T)
returns(cls.month,method="simple",percentage=T)
returns(cls.month,method="continuous",percentage=T)
returns(cls.week,method="simple",percentage=T)
returns(cls.week,method="continuous",percentage=T)
re<-returns(clpr.ts,method="simple",percentage=T)
re2<-returns(clpr.ts,method="continuous",percentage=T)

re<-re[-1]
re2<-re2[-1]
plot(re,type="l")


##多期平均收益率

#算术平均
mean(re)
mean(re2)
#几何平均
gm <- 100*(cumprod(re/100+1)[length(cumprod(re/100+1))]^(1/length(re))-1)
gm2<- 100*(cumprod(re2/100+1)[length(cumprod(re2/100+1))]^(1/length(re2))-1)
gm
gm2


#####多个股票收益计算#####
da1 <- read.table("E:/金融数学/R with application to financial quantitive analysis/CH-05/CAQC.txt",head=T,colClasses="character")
da2 <- read.table("E:/金融数学/R with application to financial quantitive analysis/CH-05/ZXTX.txt",head=T,colClasses="character")
wcls1 <- as.numeric(da1[,3])
wcls2 <- as.numeric(da2[,3])
dates1 <- as.yearmon(da1[,2])
dates2 <- as.yearmon(da2[,2])
cls1 <- zoo(wcls1,dates1)
cls2 <- zoo(wcls2,dates2)
dat.merge <- merge(cls1, cls2)

r.merge <- returns(dat.merge, method='continuous', percentage=TRUE)
print(r.merge)

#####投资组合收益计算####
# (1) 读取数据
da <- read.table("E:/金融数学/R with application to financial quantitive analysis/CH-05/5-3.txt",head=T)
sum(is.na(da))                                           # check NA code
p.app <- na.approx(da)                                   # approximate NA code
r.stocks <- diff(log(p.app))*100

# (2) 随机分配权重
set.seed(12345)
w <- runif(ncol(da), 0, 1)
(w <- w/sum(w))

# (3) 资产组合收益率计算
rp <- r.stocks %*% w
rp

#####内部收益率#####
#定义损失函数
f <- function(r, p, Cs){
  n <- length(Cs)
  tt <- 1:n
  loss <- p - sum(Cs/((1+r)^tt))
  loss
}

#计算
#1
Cs <- c(800, 1600)
p <- 2000
uniroot(f, c(0,1), p=p, Cs=Cs)
#2
Cs <- rep(120,5)
p <- 500
uniroot(f, c(0,1), p=p, Cs=Cs)
#3
Cs <- c(20,20,rep(0,3),80)
p <- 100
irr<-uniroot(f, c(0,1), p=p, Cs=Cs)$root
irr<-(1+irr)^3-1
irr

#####到期收益率#####
#定义损失函数
f_exp <- function(r, p, Cs, Cp){
  n <- length(Cs)
  tt <- 1:n
  loss <- p - sum(Cs/((1+r)^tt)) - Cp/(1+r)^n
  loss
}

# 计算

#1
Cs <- rep(2000000, times=40)
Cp <- 25000000
p <- 23640000
uniroot(f_exp, c(0,1), p=p, Cs=Cs, Cp=Cp)  

#2
Cs <- rep(950000, times=15)
Cp <- 15000000
p <- 22710000
r.expected <- 0.067
r.half <- uniroot(f_exp, c(0,1), p=p, Cs=Cs, Cp=Cp)$root          # find the zero root of f function
r.annulized <- (r.half+1)^2-1
#判断是否有投资价值
if (r.annulized>=r.expected){
  cat('Do it, and the endougeneous return is', r.annulized, '\n')
}else{
  cat('Not do it, and the endougeneous return is', r.annulized, '\n')
}


#####有效年利率#####
# 定义函数
eff_rts <- function(pr,m){
  er<-(1+pr)^m-1  
}

# 计算
(da1 <- eff_rts(0.05,2))
(da2 <- eff_rts(0.025,4))


#####投资组合收益率#####
p=57259000
Cs=c(rep(2300000,5),32300000,rep(1400000,3),11400000,rep(1050000,3),21050000)
irr<-uniroot(f, c(0,1), p=p, Cs=Cs)

#####浮动利率#####
p=99.3098
Cs=rep(5.4,12)
Cp=100
a<-uniroot(f_exp, c(0,1), p=p, Cs=Cs, Cp=Cp)  
(a$root)*2-0.1