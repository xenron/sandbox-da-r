library(xts)
library(xtsExtra)
library(quantmod)
library(forecast)
library(urca)
library(FinTS)
library(rugarch)


########xts包简介#######
#读取数据
index.dt <- read.table("D:/R Quant/data/Index/TRD_Index.txt", header=TRUE)
head(index.dt)
SH.index <- index.dt[index.dt$Indexcd == 1, -1]
SH.index <- xts(SH.index[, -1], order.by = as.Date(SH.index$Trddt)) #将交易日期去除
head(SH.index)

SH.index.2012 <- SH.index["2012-01-01/2013-01-01"]  #选取2012年的数据
head(SH.index.2012, 3)
tail(SH.index.2012, 3)


# xts数据子集的其他提取方式（begin）
SH.index.2012 <- SH.index["2012"]
head(SH.index.2012,3)
tail(SH.index.2012,3)

SH.index.after2010 <- SH.index["2010-01-01/"]
head(SH.index.after2010, 3)
tail(SH.index.after2010, 3)

SH.index.2010MartoEnd <- SH.index["2010-03/2010"]
head(SH.index.2010MartoEnd,3)
tail(SH.index.2010MartoEnd,3)
# xts数据子集的其他提取方式（end）



# OHLC数据格式
SH.ohlc <- SH.index[, c("Trddt", "Opnindex", "Hiindex", "Loindex", "Clsindex")]
SH.ohlc.daily <- to.daily(SH.ohlc)  #变成标准的ohlc格式数据，每日交易数据
head(SH.ohlc.daily)
SH.ohlc.weekly <- to.weekly(SH.ohlc)  #每月交易数据
head(SH.ohlc.weekly)



#自动算周收益率 日收益率dailyRuturn()
SH.ret <- weeklyReturn(SH.ohlc)
barChart(SH.ret, main = "Time Series of SH Index Weekly Return", 
         theme = chartTheme("white"), col = 'darkblue', border = FALSE)



# period.apply函数（begin）
ep <- endpoints(SH.ohlc, 'weeks') 
#对数据按照每一周分组
#自动计算周的最高值，最低值等

ep
max <- period.apply(SH.ohlc[, 4], ep, max)
head(max)
# period.apply函数（end）


#画出上证指数收益率的直方图
hist(SH.ret, breaks = 50, col = 'darkgreen', border = FALSE, 
     main = "Histogram of SH Index Weekly Return")
summary(coredata(SH.ret))
quantile(coredata(SH.ret), probs = seq(0, 1, 0.1))
mean(coredata(SH.ret))
sd(coredata(SH.ret))



########构建AR models##########
da=read.table("q-gnp4710.txt",header=T)
head(da)
G=da$VALUE #美国的实际国民生产总值的季度增长率
LG=log(G)  #求对数
gnp=diff(LG) #LG数列的后一项减去前一项得到的差数列，即gnp[i]=LG[i+1]-LG[i]
dim(da) 
tdx=c(1:dim(da)[1])/4+1947 # create the time index
par(mfcol=c(2,1))
plot(tdx,LG,xlab='year',ylab='log(GNP)',type='l')   #画出GNP值对数值随时间的变化情况
plot(tdx[2:253],gnp,type='l',xlab='year',ylab='growth') #画出增长率的对数值随时间的变化情况
 
#定阶
acf(gnp,lag=12)  # compute ACF
pacf(gnp,lag=12) # compute PACF
mm1=ar(gnp,method='mle')
mm1$order # Find the identified order 
names(mm1)
print(mm1$aic,digits=3)
aic=mm1$aic  # For plotting below.
length(aic)
plot(c(0:12),aic,type='h',xlab='order',ylab='aic')
lines(0:12,aic,lty=2)

#构建方程
m1=arima(gnp,order=c(3,0,0))  #构建AR(3)模型
m1
p1=c(1,-m1$coef[1:3]) # 建立特征方程
r1=polyroot(p1) # solve the polynomial equation求解特征方程
r1
Mod(r1)
k=2*pi/acos(1.616116/1.832674) # compute length of the period计算周期长度
k


#模型检验
tsdiag(m1,gof=12)  # 检验残差是否白噪声
Box.test(m1$residuals,lag=12,type='Ljung')
pv=1-pchisq(0.7493,9) # Compute p value using 1 degrees of freedom
pv

#预测
predict(m1,1)
predict(m1,2)
library("forecast")
forecast(m1,1)
forecast(m1,2)





##############构建MA模型############
#数据读入
da=read.table("D:/data/m-ibm3dx2608.txt",header=T)
head(da)
ew=da$ewrtn
plot(da$date,ew,type="l")

#定阶
acf(ew,lag=20)

#模型拟合
m1=arima(ew,order=c(0,0,9)) # unrestricted model
m1
m2=arima(ew,order=c(0,0,9),fixed=c(NA,0,NA,0,0,0,0,0,NA,NA)) 
m2
sqrt(0.005097)  #计算at标准差

#模型检验
tsdiag(m1,gof=12)   #检查残差是否白噪声
Box.test(m1$residuals,lag=12,type='Ljung')  # model checking
pv=1-pchisq(17.6,9)  # compute p-value after adjusting the d.f.
pv

#预测
m1=arima(ew[1:986],order=c(0,0,9),fixed=c(NA,0,NA,0,0,0,0,0,NA,NA))
m1
predict(m1,10) # prediction 
pre=c(predict(m1,10)$pred)
data.frame(ew[987:996],pre,ew[987:996]-pre)  #预测值与真实值的比较


###########构建ARMA模型#################
acf(ew)
pacf(ew)
#根据信息准则定阶
library("forecast")
auto.arima(ew)
#根据eacf定阶
eacf(ew)

#模型拟合
m2=arima(ew,order=c(2,0,2))
m2
m3=arima(ew,order=c(0,0,1))
m3


#模型检验
tsdiag(m2,gof=12)   #检查残差是否白噪声
Box.test(m2$residuals,lag=12,type='Ljung')
tsdiag(m3,gof=12)   #检查残差是否白噪声
Box.test(m3$residuals,lag=12,type='Ljung')


#预测
predict(m2,1)
predict(m3,1)


######自相关初步判断#########
acf(SH.ret)
pacf(SH.ret)
######自相关性检验，判断是否为白噪声#########
Box.test(SH.ret, lag = 12)   #lag表示检验当前与过去lag期的相关系数是否为0
######平稳性检验，单位根检验#####
summary(ur.df(SH.ret, type = 'none'))
#Value of test-statistic比Critical values for test statistics的值都小
#就判断为没有单位根，即为平稳时间序列



#综合例子：上证指数收益率序列的平稳性时间序列建模
# ARMA for CPI (begin)
CPI <- read.csv("D:/R Quant/data/CPI/CPI.csv", header=TRUE, fill=FALSE)
CPI <- na.omit(CPI)
CPI.xts <-xts(CPI[,-1],order.by = as.Date(CPI$time))
head(CPI.xts)
tail(CPI.xts)
CPI.xts.treat <- CPI.xts[1:(nrow(CPI.xts)-3),]
tail(CPI.xts.treat)
plot.xts(CPI.xts, main = "CPI 2001-2014")

acf(CPI.xts.treat)
pacf(CPI.xts.treat)
Box.test(CPI.xts.treat, lag=12)
summary(ur.df(CPI.xts.treat, type = 'drift'))   #ADF单位根检验#

CPI.arma <- auto.arima(CPI.xts.treat, stationary = TRUE, seasonal = FALSE, ic = 'aic')
summary(CPI.arma)
confint(CPI.arma)   #计算系数的置信区间

#####残差检验######
tsdiag(CPI.arma) 

#拟合值与原数据比较
plot(CPI.arma$x, lwd=2, col='darkgreen',
     main="CPI: Raw Data vs Fitted Values",
     ylab="CPI", xlab="time")
lines(fitted(CPI.arma),lty=1, lwd=2, col='red')

accuracy(CPI.arma)

####预测####
pred.last3 <- t(predict(CPI.arma, n.ahead = 3)$pred[1:3])
pred.whole <- cbind(t(fitted(CPI.arma)), pred.last3)
CPI.xts.pred <- xts(t(pred.whole), order.by=as.Date(CPI$time))
#将预测值转化为时间序列

plot(CPI.xts,main="CPI: Raw Data vs Predicted Values",
     ylab="CPI", xlab="time")
lines(CPI.xts.pred,lty=1, lwd=2, col='red')
# ARMA for CPI (end)

