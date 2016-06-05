######读入上证指数######
index.dt <- read.table("D:/R Quant/data/Index/TRD_Index.txt", header=TRUE)
head(index.dt)
tail(index.dt)
SH.index <- index.dt[index.dt$Indexcd == 1, -1]
head(SH.index)
tail(SH.index)

######xts包介绍######
library(xts)
#转变为xts格式
SH.index <- xts(SH.index[, -1], order.by = as.Date(SH.index$Trddt))
head(SH.index)
class(SH.index)
plot.xts(SH.index$Retindex)

#解除xts格式
head(coredata(SH.index))
class(coredata(SH.index))

data(sample_matrix)
xtsible(sample_matrix)
sample.xts <- as.xts(sample_matrix)
head(sample.xts)
class(sample.xts)

#xts数据子集
SH.index.2012 <- SH.index["2012-01-01/2013-01-01"]
head(SH.index.2012, 3)
tail(SH.index.2012, 3)

SH.index.2012 <- SH.index["2012"]
head(SH.index.2012,3)
tail(SH.index.2012,3)

SH.index.after2010 <- SH.index["2010-01-01/"]
head(SH.index.after2010, 3)
tail(SH.index.after2010, 3)

SH.index.2010MartoEnd <- SH.index["2010-03/2010"]
head(SH.index.2010MartoEnd,3)
tail(SH.index.2010MartoEnd,3)

# OHLC数据格式
SH.ohlc <- SH.index[, c("Trddt", "Opnindex", "Hiindex", "Loindex", "Clsindex")]

SH.ohlc.daily <- to.daily(SH.ohlc)  #变成标准的ohlc格式数据，每日交易数据
head(SH.ohlc.daily)
SH.ohlc.weekly <- to.weekly(SH.ohlc)  #每周交易数据
head(SH.ohlc.weekly)



######quantmod包介绍######
# 使用quantmod包和中国移动数据制图
library(quantmod)
getSymbols("CHL",src="yahoo")
barChart(CHL,theme="white")
set1=CHL[500:700]
candleChart(set1,theme="white")
chartSeries(CHL,theme="white")
chartSeries(CHL[,1],name="Open price for CHL",theme="white")
require(TTR)
chartSeries(CHL[600:764,],theme="white")
addMACD()  
addBBands()


#自动算周收益率 日收益率dailyRuturn()
SH.ret <- weeklyReturn(SH.ohlc)
barChart(SH.ret, main = "Time Series of SH Index Weekly Return", 
         theme = chartTheme("white"), col = 'darkblue', border = FALSE)


# period.apply函数
ep <- endpoints(SH.ohlc, 'weeks')
ep
max <- period.apply(SH.ohlc[, 4], ep, max)
head(max)

######时间序列的描述性统计######
hist(SH.ret, breaks = 50, col = 'darkgreen', border = FALSE, 
     main = "Histogram of SH Index Weekly Return")
summary(coredata(SH.ret))
quantile(coredata(SH.ret), probs = seq(0, 1, 0.1))
mean(coredata(SH.ret))
sd(coredata(SH.ret))

library(fBasics)
basicStats(coredata(SH.ret))



######时间序列的自相关性######
acf(coredata(SH.ret))
pacf(coredata(SH.ret))
Box.test(coredata(SH.ret))

######CPI序列建模######
library(forecast)
#读取数据
CPI <- na.omit(CPI)
CPI.xts <-xts(CPI[,-1],order.by = as.Date(CPI$time))
head(CPI.xts)
tail(CPI.xts)

CPI.xts.treat <- CPI.xts[1:(nrow(CPI.xts)-3),]#构建测试集
tail(CPI.xts.treat)
plot.xts(CPI.xts, main = "CPI 2001-2014")
#平稳性检验
library(urca)
summary(ur.df(CPI.xts.treat,lags=5,type = 'drift'))

library(tseries)
adf.test(CPI.xts.treat)

#白噪声检验
acf(CPI.xts.treat)
pacf(CPI.xts.treat)
Box.test(CPI.xts.treat, lag=12)

#模型拟合
CPI.arma <- auto.arima(CPI.xts.treat, stationary = TRUE, seasonal = FALSE, ic = 'aic')
summary(CPI.arma)
confint(CPI.arma) #计算系数的置信区间

#残差检验
accuracy(CPI.arma)
tsdiag(CPI.arma) 

#拟合值与原数据比较
plot(CPI.arma$x, lwd=2, col='darkgreen',
     main="CPI: Raw Data vs Fitted Values",
     ylab="CPI", xlab="time")
lines(fitted(CPI.arma),lty=1, lwd=2, col='red')

#预测并比较
pred.last3 <- t(predict(CPI.arma, n.ahead = 3)$pred[1:3])
pred.whole <- cbind(t(fitted(CPI.arma)), pred.last3)
CPI.xts.pred <- xts(t(pred.whole), order.by=as.Date(CPI$time))

plot(CPI.xts,main="CPI: Raw Data vs Predicted Values",
     ylab="CPI", xlab="time")
lines(CPI.xts.pred,lty=1, lwd=2, col='red')



