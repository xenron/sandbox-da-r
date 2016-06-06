######波动率模型######
da=read.table("D:/data/m-intcsp7309.txt",header=T)
head(da)
intc=log(da$intc+1) #计算对数收益率
rtn=ts(intc,frequency=12,start=c(1973,1))   #转化为时间序列
plot(rtn,type='l',xlab='year',ylab='ln-rtn') # 画时间序列图
t.test(intc)  # 检验收益率的均值是否为0
Box.test(intc,lag=12,type='Ljung') 
par(mfcol=c(2,1))
acf(intc,lag=24) # ACF plots
acf(abs(intc),lag=24) 
Box.test(abs(intc),lag=12,type='Ljung')
par(mfcol=c(1,1))


######ARCH效应检验######
#Intel股票对数收益率
y=intc-mean(intc)   #计算at
Box.test(y^2,lag=12,type='Ljung')
source("D:/data/archTest.R")  
archTest(y,12)   

#美元对欧元汇率的日对数收益率
fx=read.table("D:/data/d-useu9910.txt",header=T)
fxeu=log(fx$rate)
eu=diff(fxeu)
plot(eu,type="l")
acf(eu)
Box.test(eu,lag=20,type='Ljung')
t.test(eu)
acf(eu^2)
pacf(eu^2)
Box.test(eu^2,lag=20,type='Ljung')
archTest(eu,20)


######ARCHM模型的建立######
library(fGarch) # 加载程序包 

da=read.table("D:/data/m-intcsp7309.txt",header=T)
head(da)
intc=log(da$intc+1) #计算对数收益率
rtn=ts(intc,frequency=12,start=c(1973,1))
y=intc-mean(intc)

pacf(y^2)  #通过pacf定阶
m1=garchFit(~1+garch(1,0),data=intc,trace=F,cond.dist="std") # 拟合ARCH（3）模型
summary(m1)
m2=garchFit(~1+garch(1,0),data=intc,trace=F) #简化模型
summary(m2)
resi=residuals(m2,standardize=T) #提取残差
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
tt=-0.5526/sqrt(6/444) # Testing skewness of the data
tt
tt=(0.8717-1)/0.0629 # Testing skewness of the model.
tt
pv=2*pnorm(tt)  # Compute p-value 
pv
plot(m6)
#预测
yt=intc-mean(intc)
m1=arima(yt^2,order=c(1,0,1))
m1
mean(intc)
fit=yt^2-m1$residuals
v3=volatility(m6)  # m6 is GARCH(1,1) with skew-t innovations.
cor(v3,sqrt(fit))
