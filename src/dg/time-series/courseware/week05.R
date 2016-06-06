install.packages("fGarch")

######波动率模型######
da=read.table("D:/data/m-intcsp7309.txt",header=T)
head(da)
# 计算对数收益率
intc=log(da$intc+1)
#转化为时间序列
rtn=ts(intc,frequency=12,start=c(1973,1))
# 画时间序列图
plot(rtn,type='l',xlab='year',ylab='ln-rtn')
# 检验收益率的均值是否为0
# 0假设，对数收益率的均值为0
# p-value小于0.05，拒绝0假设，对数收益率的均值不为0
# ?????????????????????????????????????????????????????????????
t.test(intc)
Box.test(intc,lag=12,type='Ljung') 

par(mfcol=c(2,1))
# ACF plots
# 无明显的相关性
acf(intc,lag=24)
# 数收益率的绝对值
# 存在一定的相关性
acf(abs(intc),lag=24) 

# p值接近0
# 结论，股票对数收益率，前后不相关，但不是独立的
# 使用一元波动模型，来刻画不相关也不独立的性质
# 因为随机扰动不是相互独立的，所以不属于白噪声
Box.test(abs(intc),lag=12,type='Ljung')
par(mfcol=c(1,1))


######ARCH效应检验######
#Intel股票对数收益率
# 计算at
y=intc-mean(intc)
# p值小于0.05，拒绝0假设，前后项之间存在相关性
Box.test(y^2,lag=12,type='Ljung')
source("D:/data/archTest.R")
# p值<0.05，拒绝零假设，存在相关性（如12），存在ARCH效应
archTest(y,12)

#美元对欧元汇率的日对数收益率
fx=read.table("D:/data/d-useu9910.txt",header=T)
fxeu=log(fx$rate)
eu=diff(fxeu)
# 反映出波动率不同
plot(eu,type="l")
# 相关性不强
acf(eu)
# p值>0.05，不能拒绝零假设
Box.test(eu,lag=20,type='Ljung')
# 检验均值是否为0
# 不能拒绝0假设，均值应该为零
t.test(eu)
acf(eu^2)
pacf(eu^2)
# p值<0.05，拒绝0假设，at之间不相互独立，有较强的ARCH效应
Box.test(eu^2,lag=20,type='Ljung')
archTest(eu,20)


######ARCHM模型的建立######
# 加载程序包
library(fGarch)

da=read.table("D:/data/m-intcsp7309.txt",header=T)
head(da)
#计算对数收益率
intc=log(da$intc+1)
rtn=ts(intc,frequency=12,start=c(1973,1))
y=intc-mean(intc)

# 通过pacf定阶
pacf(y^2)
# 拟合ARCH（3）模型
m1=garchFit(~1+garch(1,0),data=intc,trace=F,cond.dist="std")
summary(m1)
# 简化模型
m2=garchFit(~1+garch(1,0),data=intc,trace=F)
summary(m2)
#提取残差
resi=residuals(m2,standardize=T)
tdx=c(1:444)/12+1973

plot(tdx,resi,xlab='year',ylab='stand-resi',type='l')
acf(resi,lag=20)
pacf(resi^2,lag=20) 
plot(m2)



######GARCH模型######
library(fGarch)
m4=garchFit(~1+garch(1,1),data=intc,trace=F) 
summary(m4)
v1=volatility(m4)  # Obtain volatility
resi=residuals(m4,standardize=T) # Standardized residuals
vol=ts(v1,frequency=12,start=c(1973,1))
res=ts(resi,frequency=12,start=c(1973,1))
par(mfcol=c(2,1))  # Show volatility and residuals
plot(vol,xlab='year',ylab='volatility',type='l')
plot(res,xlab='year',ylab='st. resi',type='l') 
par(mfcol=c(2,2)) # Obtain ACF & PACF
acf(resi,lag=24)
pacf(resi,lag=24)
acf(resi^2,lag=24)
pacf(resi^2,lag=24) 
# Obtain plot of predictive intervals
par(mfcol=c(1,1))
upp=0.0113+2*v1
low=0.0113-2*v1
tdx=c(1:444)/12+1973
plot(tdx,intc,xlab='year',ylab='series',type='l',ylim=c(-0.6,0.6))
lines(tdx,upp,lty=2,col='red')
lines(tdx,low,lty=2,col='red')
abline(h=c(0.0113))
# Student-t innovations
m5=garchFit(~1+garch(1,1),data=intc,trace=F,cond.dist="std")
summary(m5)
v2=volatility(m5)
m6=garchFit(~1+garch(1,1),data=intc,trace=F,cond.dist='sstd')
summary(m6)
v3=volatility(m6)
par(mfcol=c(3,1))
plot(tdx,v1,xlab='year',ylab='volatility',type='l',ylim=c(0.06,0.3))
title(main='(a) Gaussian')
plot(tdx,v2,xlab='year',ylab='volatility',type='l',ylim=c(0.06,0.3))
title(main='(b) Student-t')
plot(tdx,v3,xlab='year',ylab='volatility',type='l',ylim=c(0.06,0.3))
title(main='(c) Skew Student-t') 
cor(cbind(v1,v2,v3))
library(fBasics)
basicStats(intc)
# Testing skewness of the data
tt=-0.5526/sqrt(6/444)
tt
# Testing skewness of the model.
tt=(0.8717-1)/0.0629
tt
# Compute p-value
pv=2*pnorm(tt)
pv
plot(m6)

#预测
yt=intc-mean(intc)
m1=arima(yt^2,order=c(1,0,1))
m1
mean(intc)
fit=yt^2-m1$residuals
# m6 is GARCH(1,1) with skew-t innovations.
v3=volatility(m6)
cor(v3,sqrt(fit))
