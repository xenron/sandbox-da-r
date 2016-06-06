######带时间序列误差的回归模型######
#数据读入
r1=read.table("D:/data/w-gs1yr.txt",header=T)[,4]
r3=read.table("D:/data/w-gs3yr.txt",header=T)[,4]
# 绘制走势图
plot(r1,type="l",col="red")
lines(r3,col="blue")
# 散点图，呈现强烈的正相关性
# 相关系数较大
plot(r1,r3)
# 构造lm线性模型
m1=lm(r3~r1)
# 显著相关
summary(m1)
# 残差图，分布不均匀，仍然包含未提取的信息
plot(m1$residuals,type='l')
# 显示残差高度自相关
# 假设两个利率都是非平稳时间序列，这样的残差表明这两个利率不是协整的
acf(m1$residuals,lag=36)
#
c1=diff(r1)
c3=diff(r3)
plot(c1,c3)
m2=lm(c3~-1+c1)
summary(m2)
plot(m2$residuals,type='l')
acf(m2$residuals,lag=36)
#
m3=arima(c3,order=c(0,0,1),xreg=c1,include.mean=F)
m3
plot(m3$residuals)
acf(m3$residuals,lag=36)
rsq=(sum(c3^2)-sum(m3$residuals^2))/sum(c3^2)
rsq

######长记忆模型######
library(fracdiff)
da=read.table("D:/data/d-ibm3dx7008.txt",header=T)
head(da)
ew=abs(da$vwretd)
acf(ew,lag=100)
# obtain Geweke-Port-Hudak estimate using command fdGPH
m3=fdGPH(ew)
m3
m2=fracdiff(ew,nar=1,nma=1)
summary(m2)


######模型比较和平均######

da=read.table("D:/data/q-gdpc96.txt",header=T)
head(da)
gdp=log(da$gdp)
plot(gdp,type="l")
dgdp=diff(gdp)
plot(dgdp,type="l")
acf(dgdp)
pacf(dgdp)
m1=ar(dgdp,method='mle')
m1$order
#样本内比较
m2=arima(dgdp,order=c(3,0,0))
m2
tsdiag(m2)
m3=arima(dgdp,order=c(3,0,0),season=list(order=c(1,0,1),period=4))
m3
tsdiag(m3)
m4=arima(dgdp,order=c(0,0,2),season=list(order=c(1,0,1),period=4))
m4
#样本外比较
source("D:/data/backtest.R")    # Perform backtest
mm2=backtest(m2,dgdp,215,1)
mm3=backtest(m3,dgdp,215,1)
#模型平均
predict(m2,1)$pred
predict(m3,1)$pred
(predict(m2,1)$pred+predict(m3,1)$pred)/2


######每周普通汽油价格######
da=read.table("D:/data/w-petroprice.txt",header=T)
da1=read.table("D:/data/w-gasoline.txt")
pgs=log(da1[,1])
pus=log(da$US)
#时间序列图
tdx=c(1:717)/52+1997  # calendar time
par(mfcol=c(2,1))
plot(tdx,pgs,xlab='year',ylab='ln(price)',type='l')
title(main='(a) Gasoline')
plot(tdx,pus,xlab='year',ylab='ln(price)',type='l')
title(main='(b) Crude oil')
#纯时间序列模型
dpgs=diff(pgs)
acf(dpgs,lag=20)
pacf(dpgs,lag=20)
m1=ar(diff(pgs),method='mle')
m1$order
t.test(dpgs)
m1=arima(dpgs,order=c(5,0,0),include.mean=F)
m1
m1=arima(dpgs,order=c(5,0,0),include.mean=F,fixed=c(NA,NA,NA,0,NA))
m1
tsdiag(m1,gof=20)
#原油价格的使用
dpus=diff(pus)
m3=lm(dpgs~-1+dpus)
summary(m3)
acf(m3$residuals,lag=20)
pacf(m3$residuals,lag=20)

m4=ar(m3$residuals,method='mle')
m4$order
m4=arima(dpgs,order=c(6,0,0),include.mean=F,xreg=dpus)
m4
m4=arima(dpgs,order=c(5,0,0),include.mean=F,xreg=dpus)
m4
m4=arima(dpgs,order=c(5,0,0),include.mean=F,xreg=dpus,fixed=c(NA,NA,NA,0,NA,NA))
m4
tsdiag(m4,gof=20)
#模型比较
c1=c(NA,NA,NA,0,NA)
pm1=backtest(m1,dpgs,316,1,fixed=c1,inc.mean=F)
c4=c(NA,NA,NA,0,NA,NA)
pm4=backtest(m4,dpgs,316,1,xre=dpus,inc.mean=F,fixed=c4)
tdx=tdx[2:717]
pm4fit=dpgs[317:716]-pm4$error
pm1fit=dpgs[317:716]-pm1$error
plot(tdx[317:716],dpgs[317:716],xlab='year',ylab='growth',type='l')
points(tdx[317:716],pm1fit,pch='*')
plot(tdx[317:716],dpgs[317:716],xlab='year',ylab='growth',type='l')
points(tdx[317:716],pm4fit,pch='*')
#样本外预测
m6=lm(dpgs[2:716]~-1+dpus[1:715])
summary(m6)
acf(m6$residuals,lag=20)
pacf(m6$residuals,lag=20)
m7=ar(m6$residuals,method='mle')
m7$order
m7=arima(dpgs[2:716],order=c(9,0,0),include.mean=F,xreg=dpus[1:715])
m7
m7=arima(dpgs[2:716],order=c(9,0,0),include.mean=F,xreg=dpus[1:715],fixed=c(NA,NA,NA,0,NA,0,0,0,NA,NA))
m7
tsdiag(m7,gof=20)
c7=c(NA,NA,NA,0,NA,0,0,0,NA,NA)
pm7=backtest(m7,dpgs[2:716],315,1,xre=dpus[1:715],inc.mean=F,fixed=c7)


