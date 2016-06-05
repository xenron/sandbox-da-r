#######平滑法########
#滑动平滑法
sma.cal <- function( ts ) { 
  n <- length(ts)
  sma <- c()
  for( t in 1 : n ) sma[t]= mean(ts[1:t])
  return(sma)
}

#加权平滑法
wma.cal <- function( ts, weight ){
  n <- length(ts)
  k <- length(weight)
  
  wma <- c()
  for( t in (k+1):n ) wma[t]= sum( ts[(t-k+1):t] * weight )
  
  return(wma)
}

#k期移动平滑法
kwma.cal <- function( ts, k=20 ) { 
  n <- length(ts)
  kwma <- c()
  for( t in (k+1): n ) kwma[t]= mean(ts[(t-k+1):t])
  return(kwma)
}

#指数平滑法
ewma.cal <- function( ts, a=0.8 ){
  n <- length(ts)
  
  ewma <- c(ts[1]) 
  for( t in 2:n ) ewma[t]= a*ts[t] + (1-a)*ewma[t-1]
  
  return(ewma)
}

#计算
DAX <-  as.numeric( EuStockMarkets[, 1] )
DAX.sma<- sma.cal(DAX)
DAX.kwma <- kwma.cal(DAX, k=20)
DAX.wma <- wma.cal(DAX, weight=c(0.1, 0.2, 0.3, 0.4))
DAX.ewma <- ewma.cal(DAX, a=0.3)
tail(DAX.sma)
tail(DAX.wma)
tail(DAX.kwma)
tail(DAX.ewma)


#上证综指的日收益滑动法计算
index <- read.table("D:/R Quant/Data/Index/TRD_Index.txt",header=TRUE)

index.SH <- index[index$Indexcd == 1, ]  

plot(index.SH$Retindex,type="l")
lines(sma.cal(index.SH$Retindex),col="red")
plot(index.SH$Retindex,type="l")
lines(wma.cal(index.SH$Retindex,c(0.4,0.3,0.2,01)),col="blue")
plot(index.SH$Retindex,type="l")
lines(kwma.cal(index.SH$Retindex),col="green")
plot(index.SH$Retindex,type="l")
lines(ewma.cal(index.SH$Retindex),col="yellow")

#######趋势拟合法########
#线性趋势
library("TSA")
data(rwalk)
t=time(rwalk)
model1=lm(rwalk~t)
summary(model1)

plot(rwalk,type='o',ylab='y')
abline(model1) # add the fitted least squares line


##曲线趋势
#二次曲线
t=time(rwalk)
model1=lm(rwalk~t+I(t^2))
summary(model1)



########季节变动#########
#季节均值法
# season(tempdub) creates a vector of the month index of the data as a factor 
#season()这个函数创建一个月份向量的数据作为一个因子
data(tempdub)
month.=season(tempdub) # the period sign is included to make the printout from
# the commands two line below clearer; ditto below.
model2=lm(tempdub~month.-1) # -1 removes the intercept term 
summary(model2)

#有截距的情形，自动将1月的数据丢掉
model3=lm(tempdub~month.) # intercept is automatically included so one month (Jan) is dropped
summary(model3)

#余弦趋势法
har.=harmonic(tempdub,1)
model4=lm(tempdub~har.)
summary(model4)
plot(ts(fitted(model4),freq=12,start=c(1964,1)),ylab='Temperature',type='l',
     ylim=range(c(fitted(model4),tempdub))) 
#参数ylim的设定确保了图象y轴的范围
points(tempdub)









