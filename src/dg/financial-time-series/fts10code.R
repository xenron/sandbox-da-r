library(MTS)
######IBM与标准普尔500指数######
da <- read.table("D:/data/m-ibmsp2608.txt", header=TRUE, quote="\"")

ibm=log(da$ibm+1)*100
sp5=log(da$sp+1)*100
y=data.frame(cbind(ibm,sp5))
par(mfcol=c(2,1))
plot(ibm,type="l")
plot(sp5,type="l")

#
MTSplot(y)

par(mfcol=c(2,2))
plot(sp5,ibm)
plot(ibm[-length(ibm)],sp5[-1])
plot(sp5[-length(sp)],ibm[-1])
plot(sp5[-length(sp)],sp5[-1])
par(mfcol=c(1,1))

cor(y)
ccm(y,lags=5,level=T)

par(mfcol=c(2,2))
acf(ibm)
ccf(ibm,sp5)
ccf(sp5,ibm)
acf(sp5)
par(mfcol=c(1,1))


######不同期限的债券指数月收益率######
bnd <- read.table("D:/data/m-bnd.txt", header=TRUE, quote="\"")
ccm(bnd,lag=2,level=T)

######多元混成检验######
mq(y,lag=12)
mq(bnd,lag=12)



######构建VAR模型######
da <- read.table("D:/data/m-ibmsp2608.txt", header=TRUE, quote="\"")
ibm=log(da$ibm+1)*100
sp5=log(da$sp+1)*100
y=data.frame(cbind(ibm,sp5))

ord.choice=VARorder(y,maxp=10)

var1.fit=VAR(y,p=1)
var5.fit=VAR(y,p=5)
MTSdiag(var1.fit)
MTSdiag(var5.fit)

var1.pred=VARpred(var1.fit,h=6)

######构建VMA模型######
vma5.fit=VMAs(y,malags=c(1,2,3,5))

######构建VARMA模型######
data <- read.table("D:/data/m-gs1n3-5301.txt", quote="\"")
y=data[,-3]
VARMA(y,p=2,q=1)


######协整VAR模型######
#数据整理
x <- read.table("D:/data/w-tb3n6ms.txt", header=TRUE, quote="\"")
tb3m=log(x[,1]+1)
y=x[,1:2]
ord.choice=VARorder(y,maxp=12)

cointst.rc=ca.jo(y,ecdet="const",type="trace",K=3,spec="transitory")
summary(cointst.rc)

library(tsDyn)
vecm.fit=VECM(y,2)
summary(vecm.fit)

vecm.fst=predict(vecm.fit,n.ahead=10)
vecm.fst
summary(vecm.fst)


######配对交易######
library(urca)
help(ca.jo)
da=read.table("D:/data/d-bhp0206.txt",head=T)
da1=read.table("D:/data/d-vale0206.txt",head=T)
bnp=log(da[,9])
vale=log(da1[,9])
m1=lm(bnp~vale)
summary(m1)
wt=m1$residuals
m3=arima(wt,order=c(2,0,0),include.mean=F)
m3
p1=c(1,-m3$coef)
x=polyroot(p1)
x

xt=cbind(bnp,vale)
mm=ar(xt)
mm$order
cot=ca.jo(xt,ecdet="const",type="trace",K=2,spec="transitory")
summary(cot)

co1=ca.jo(xt,ecdet="const",type="eigen",K=2,spec="transitory")
summary(co1)







