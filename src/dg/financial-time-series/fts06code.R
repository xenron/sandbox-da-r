######随机季节模型######
#CO2含量的时间序列图
library(TSA)
data(co2)
plot(co2,ylab='CO2',main='Monthly Carbon Dioxide Levels at Alert, NWT, Canada')
#带有月份标识的时间序列图
plot(window(co2,start=c(2000,1)),main='Carbon Dioxide Levels with Monthly Symbols', ylab='CO2')
Month=c("J","F","M","A","M","J","J","A","S","O","N","D")
points(window(co2,start=c(2000,1)),pch=Month)

#残差滞后高度自相关
month.=season(co2)
model=lm(co2~month.-1)
summary(model)
res=residuals(model)
acf(res)

######乘法ARMA模型######
par(mfrow=c(1,2))
#ARMA(0,1)*(0,1)模型模拟
plot(y=ARMAacf(ma=c(0.5,rep(0,10),0.8,0.4),lag.max=13)[-1],x=1:13,type='h',
     xlab='Lag k',ylab=expression(rho[k]),axes=F,ylim=c(0,0.6))
points(y=ARMAacf(ma=c(0.5,rep(0,10),0.8,0.4),lag.max=13)[-1],x=1:13,pch=20)
abline(h=0)
axis(1,at=1:13, labels=c(1,NA,3,NA,5,NA,7,NA,9,NA,11,NA,13))
axis(2)
text(x=7,y=.5,labels=expression(list(theta==-0.5,Theta==-0.8)))

plot(y=ARMAacf(ma=c(-0.5,rep(0,10),0.8,-0.4),lag.max=13)[-1],x=1:13,type='h',
     xlab='Lag k',ylab=expression(rho[k]),axes=F)
points(y=ARMAacf(ma=c(-0.5,rep(0,10),0.8,-0.4),lag.max=13)[-1],x=1:13,pch=20)
abline(h=0)
axis(1,at=1:13, labels=c(1,NA,3,NA,5,NA,7,NA,9,NA,11,NA,13))
axis(2)
text(x=7,y=.35,labels=expression(list(theta==0.5,Theta==-0.8)))



# ARMA(1,0)*(1,0)模拟
plot(y=ARMAacf(ar=c(rep(0,11), 0.75), ma=0.4,lag.max=61)[-1],x=1:61,type='h',
     xlab='Lag k',ylab=expression(rho[k]),axes=F,ylim=c(-0.4,.8),xlim=c(0,61))
points(y=ARMAacf(ar=c(rep(0,11), 0.75), ma=0.4,lag.max=61)[c(1,11,12,13,
                                                             23,24,25,35,36,37,47,48,49,59,60,61)+1],
       x=c(1,11,12,13,23,24,25,35,36,37,47,48,49,59,60,61),pch=20)
abline(h=0)
axis(1,at=c(0,1,12,24,36,48,60,61),labels=c(NA,1,12,24,36,48,60,NA))
axis(2, at=c(-0.4,0.0,0.4,0.8))
text(x=40,y=.8,labels=expression(list(Phi==0.75,theta==-0.4)))

plot(y=ARMAacf(ar=c(rep(0,11), 0.75), ma=-0.4,lag.max=61)[-1],x=1:61,type='h',
     xlab='Lag k',ylab=expression(rho[k]),axes=F,ylim=c(-0.4,.8))
points(y=ARMAacf(ar=c(rep(0,11), 0.75), ma=-0.4,lag.max=61)[c(1,11,12,13,
                                                              23,24,25,35,36,37,47,48,49,59,60,61)+1],
       x=c(1,11,12,13,23,24,25,35,36,37,47,48,49,59,60,61),pch=20)
abline(h=0)
axis(1,at=c(0,1,12,24,36,48,60,61),labels=c(NA,1,12,24,36,48,60,NA))
axis(2, at=c(-0.4,0.0,0.4,0.8))
text(x=40,y=.8,labels=expression(list(Phi==0.75,theta==0.4)))


######模型识别、拟合与检验######
par(mfrow=c(1,1))
#CO2含量建模
#原数据acf图
acf(as.vector(co2),lag.max=36,
    main=expression(Sample~~ACF~~of~~CO[2]~~Levels))

#一次差分序列图
plot(diff(co2),main=expression(Time~~Series~~Plot~~of~~the~~First~~Differences~~of~~
                                 CO[2]~~Levels), ylab=expression(First~~Difference~~of~~CO[2]))


# 一次差分的ACf
acf(as.vector(diff(co2)),lag.max=36,
    main=expression(Sample~~ACF~~of~~the~~First~~Differences~~of~~
                      CO[2]~~Levels))


# 一次差分和季节差分的序列图
plot(diff(diff(co2),lag=12),main=expression(Time~~Series~~Plot~~of~~the~~First~~and~~
                                              Seasonal~~Differences~~of~~CO[2]~~Levels), 
     ylab=expression(First~~and~~Seasonal~~Difference~~of~~C~O[2])) 


# 一次差分与季节差分的ACF
acf(as.vector(diff(diff(co2),lag=12)),lag.max=36,ci.type='ma',
    main=expression(Sample~~ACF~~of~~the~~First~~and~~Seasonal~~Differences~~of~~
                      CO[2]~~Levels))
# 模型拟合
m1.co2=arima(co2,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
m1.co2

# 残差检验
plot(window(rstandard(m1.co2),start=c(1995,2)),ylab='Standardized Residuals', type='o',
     main=expression(Residuals~~from~~the~~ARIMA(list(0,1,1))%*%(list(0,1,1))[12]~~Model))
abline(h=0)

acf(as.vector(window(rstandard(m1.co2),start=c(1995,2))),lag.max=36,
    main=expression(ACF~~of~~Residuals~~from~~the~~ARIMA(list(0,1,1))%*%(list(0,1,1))[12]~~Model))

tsdiag(m1.co2, gof.lag=36)

hist(window(rstandard(m1.co2),start=c(1995,2)),xlab='Standardized Residuals',
     main=expression(Residuals~~from~~the~~ARIMA(list(0,1,1))%*%(list(0,1,1))[12]~~Model))

qqnorm(window(rstandard(m1.co2),start=c(1995,2)),main=expression(Normal~~Q-Q~~Plot))
title(main=expression(Residuals~~from~~the~~ARIMA(list(0,1,1))%*%(list(0,1,1))[12]~~Model),
      line=3)
qqline(window(rstandard(m1.co2),start=c(1995,2)))

#过度拟合模型
m2.co2=arima(co2,order=c(0,1,2),seasonal=list(order=c(0,1,1),period=12))
m2.co2

# 预测
plot(m1.co2,n1=c(2003,1),n.ahead=24,col='red',xlab='Year',type='o',
     ylab=expression(CO[2]~~Levels),
     main=expression(Forecasts~~and~~Forecast~~Limits~~'for'~~the~~CO[2]~~Model))

plot(m1.co2,n1=c(2004,1),n.ahead=48,col='red',xlab='Year',type='o',
     ylab=expression(CO[2]~~Levels),
     main=expression(Long~~Term~~Forecasts~~'for'~~the~~CO[2]~~Model))




######可口可乐公司股票对数盈利######
da=read.table("D:/data/q-ko-earns8309.txt",header=T)
head(da)
eps=log(da$value)
koeps=ts(eps,frequency=4,start=c(1983,1))
c1=c("1","2","3","4")
plot(koeps,type='l')
points(koeps,pch=c1,cex=0.6)   #添加季节标记

par(mfcol=c(2,2))
koeps=log(da$value)
deps=diff(koeps)
sdeps=diff(koeps,4)
ddeps=diff(sdeps)
acf(koeps,lag=20)
acf(deps,lag=20)
acf(sdeps,lag=20)
acf(ddeps,lag=20)


c1=c("2","3","4","1")
c2=c("1","2","3","4")
par(mfcol=c(3,1))
plot(deps,xlab='year',ylab='diff',type='l')
points(deps,pch=c1,cex=0.7)
plot(sdeps,xlab='year',ylab='sea-diff',type='l')
points(sdeps,pch=c2,cex=0.7)
plot(ddeps,xlab='year',ylab='dd',type='l')
points(ddeps,pch=c1,cex=0.7) 

# 模型构建
m1=arima(koeps,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=4))
m1
tsdiag(m1,gof=20)  # model checking
Box.test(m1$residuals,lag=12,type='Ljung')
pp=1-pchisq(13.30,10)
pp
koeps=log(da$value)
length(koeps)
y=koeps[1:100]
m1=arima(y,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=4))
m1
pm1=predict(m1,7)
names(pm1)
pred=pm1$pred
se=pm1$se
ko=da$value
fore=exp(pred+se^2/2)
v1=exp(2*pred+se^2)*(exp(se^2)-1)
s1=sqrt(v1)
eps=ko[80:107]
length(eps)
tdx=(c(1:28)+3)/4+2002
upp=c(ko[100],fore+2*s1)
low=c(ko[100],fore-2*s1)
min(low,eps)
max(upp,eps)
plot(tdx,eps,xlab='year',ylab='earnings',type='l',ylim=c(0.35,1.3))
points(tdx[22:28],fore,pch='*')
lines(tdx[21:28],upp,lty=2)
lines(tdx[21:28],low,lty=2)
points(tdx[22:28],ko[101:107],pch='o',cex=0.7)
