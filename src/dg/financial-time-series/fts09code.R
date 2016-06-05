library(TSA)
library(fGarch)
######双线性模型######
da=read.table("D:/data/m-ibm3dx2608.txt",header=T)
ew<-da$ewrtn
acf(ew)
pacf(ew)
m1<-arima(ew,order=c(3,0,0))
re<-residuals(m1)
acf(re^2)
pacf(re^2)
source("D:/data/archTest.R") 
y=ew-mean(ew)
archTest(y,12)   #check the ARCH effect
m1=garchFit(~1+garch(3,0),data=y,trace=F) # 拟合ARCH（3）模型
summary(m1)

library(rugarch)
garch11.spec <- ugarchspec(variance.model = list(garchOrder = c(3, 1)), 
                           mean.model = list(armaOrder = c(3, 0)))
mod.garch <- ugarchfit(spec = garch11.spec, data=ew)
mod.garch




######门限自回归模型######
###失业率###
da=read.table("D:/Data/m-unrate.txt",header=T)
dim(da)
head(da)
unemp=da$rate
unrate=ts(unemp,frequency=12,start=c(1948,1))
plot(unrate,xlab='year',ylab='unrate',type='l')
par(mfcol=c(2,2))
acf(unemp,lag=36)
pacf(unemp,lag=36)
acf(diff(unemp),lag=36)
pacf(diff(unemp),lag=36)


tar(unrate,p1=12,p2=12,d=1,a=0.1,print=T,method="MAIC")

#另一本书的例子
require(tsDyn)       # load package - install it if you don't have it
require(astsa)


lag1.plot(log10(lynx), 4, corr=FALSE)
(u = setar(log10(lynx), m=2, thDelay=1))  # fit model and view results
plot(u)  # graphics -  ?plot.setar for information



######平滑转移AR模型######
da=read.table("D:/data/m-3m4608.txt",header=T)
rtn=da[,2]
  
star<-function(par){
  f=0
  T1=length(rtn)
  h=c(1,1)
  at=c(0,0)
  for(t in 3:T1){
    resi=rtn[t]-par[1]
    at=c(at,resi)
    sig=par[2]+par[3]*at[t-1]^2+par[4]*at[t-2]^2
    sig1=par[5]+par[6]*at[t-1]^2
    tt=sqrt(sig+sig1/(1+exp(-1000*at[t-1])))
    h=c(h,tt)
    x=resi/tt
    f=f+log(tt)+0.5*x*x
  }
  f
}

par=c(0.001,0.002,0.256,0.141,0.002,-0.314)
m2=optim(par,star,method="BFGS",hessian=T)


######马尔科夫转移模型######

######非参数方法######
z1=read.table("D:/data/w-tb3ms7097.txt",header=T)
x=z1[1:1460,4]/100
y=(z1[2:1461,4]-z1[1:1460,4])/100
par(mfcol=c(2,2))
plot(x,y,pch="*",xlab="x(t-1)",ylab="x(t)")
lines(lowess(x,y))
title(main="(a)y(t) v.s x(t-1)")
fit=lowess(x,y)
plot(fit$x,fit$y,xlab="x(t-1)",ylab="mu",type="l",ylim=c(-0.002,0.002))
title(main="(b)Estimate of mu")
plot(x,abs(y),pch="*",xlab="x(t-1)",ylab="abs(y)")
lines(lowess(x,abs(y)))
title(main="(c) abs(y(t)) v.s x(t-1)")

fit2=lowess(x,abs(y))  
plot(fit2$x,fit2$y,xlab="x(t-1)",ylab="sigma",type="l",ylim=c(0,0.01))
title(main="(b)Estimate of sigma")


######非线性检验######
r1=rnorm(500,0,1)
ar(r1)$order
Tsay.test(r1)
Keenan.test(r1)
Tsay.test(ew)


library(tseries)
bds.test(ew,m=5)

