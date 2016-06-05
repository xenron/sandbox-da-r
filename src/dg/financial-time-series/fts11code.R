######单因子模型######
x <- read.table("D:/data/m-fac9003.txt", header=TRUE, quote="\"")
xmtx=cbind(rep(1,168),x[,14])
rtn=as.matrix(x[,1:13])

#系数估计
xit.hat=solve(t(xmtx)%*%xmtx)%*%(t(xmtx)%*%rtn)
beta.hat=t(xit.hat[2,])
E.hat=rtn-xmtx%*%xit.hat
D.hat=diag(t(E.hat)%*%E.hat/(168-1-1))
r.square=1-diag(t(E.hat)%*%E.hat)/diag(t(rtn)%*%rtn)
t(rbind(beta.hat,sqrt(D.hat),r.square))

#协方差矩阵和相关矩阵
cov.r=var(x[,14])*(t(beta.hat)%*%beta.hat)+diag(D.hat)
sd.r=sqrt(diag(cov.r))
corr.r=cov.r/outer(sd.r,sd.r)
print(corr.r,digits=1,width=2)

#样本相关矩阵
print(cor(rtn),digits=1,width=2)

#GMVP
w.gmin.model=solve(cov.r)%*%rep(1,nrow(cov.r))
w.gmin.model=w.gmin.model/sum(w.gmin.model)
t(w.gmin.model)
w.gmin.data=solve(var(rtn))%*%rep(1,nrow(cov.r))
w.gmin.data=w.gmin.data/sum(w.gmin.data)
t(w.gmin.data)

resi.cov=t(E.hat)%*%E.hat/(168-2)
resi.sd=sqrt(diag(resi.cov))
resi.cor=resi.cov/outer(resi.sd,resi.sd)
print(resi.cor,digits=1,width=2)


######多因子模型######
da=read.table("D:/data/m-cpice16-dp7503.txt",header=T)
cpi=da[,1]
cen=da[,2]
x1=cbind(cpi,cen)
y1=data.frame(x1)
VARorder(y1,maxp=13)
var3.fit=VAR(y1,p=3)
res=var3.fit$residuals[166:333,1:2]
da=matrix(read.table(file="m-fac9003.txt"),14)
xmtx=cbind(rep(1,168),res)
da=t(da)
rtn=as.matrix(x[,1:13])
xit.hat=solve(t(xmtx)%*%xmtx)%*%(t(xmtx)%*%rtn)
beta.hat=t(xit.hat[2:3,])
E.hat=rtn-xmtx%*%xit.hat
D.hat=diag(t(E.hat)%*%E.hat/(168-2-1))
r.square=1-diag(t(E.hat)%*%E.hat)/diag(t(rtn)%*%rtn)

#协方差矩阵与相关矩阵
cov.rtn=beta.hat%*%var(res)%*%t(beta.hat)+diag(D.hat)
sd.rtn=sqrt(diag(cov.rtn))
cor.rtn=cov.rtn/outer(sd.rtn,sd.rtn)
print(cor.rtn,diits=1,width=2)

cov.resi=t(E.hat)%*%E.hat/(168-3)
sd.resi=sqrt(diag(cov.resi))
cor.resi=cov.resi/outer(sd.resi,sd.resi)
print(cor.resi,digits=1,width=2)


######BARRA因子模型######
da=read.table("D:/data/m-barra-9003.txt",header=T)
rm=matrix(apply(da,2,mean),1)
rtn=da-matrix(1,168,1)%*%rm
rtn=t(rtn)
fin=c(rep(1,4),rep(0,6))
tech=c(rep(0,4),rep(1,3),rep(0,3))
oth=c(rep(0,7),rep(1,3))
ind.dum=cbind(fin,tech,oth)
ind.dum

#样本相关矩阵
cov.rtn=var(rtn)
sd.rtn=sqrt(diag(cov.rtn))
corr.rtn=cov.rtn/outer(sd.rtn,sd.rtn)
print(corr.rtn,digits=1,width=2)

#参数估计
#OLS估计
F.hat.o=solve(t(ind.dum)%*%ind.dum)%*%t(ind.dum)%*%rtn
E.hat.o=rtn-ind.dum%*%F.hat.o
diagD.hat.o=rowVars(E.hat.o)
#广义最小二乘估计
Dinv.hat = diag(diagD.hat.o^(-1))
Hmtx=solve(t(ind.dum)%*%Dinv.hat%*%ind.dum)%*%t(ind.dum)%*%Dinv.hat
F.hat.g=Hmtx%*%rtn
F.hat.gt=t(F.hat.g)
E.hat.g=rtn-ind.dum%*%F.hat.g
diagD.hat.g=rowVars(E.hat.g)
t(Hmtx)
cov.ind=ind.dum%*%var(F.hat.gt)%*%t(ind.dum)+diag(diagD.hat.g)
sd.ind=sqrt(diag(cov.ind))
corr.ind=cov.ind/outer(sd.ind,sd.ind)
print(corr.ind,digits=1,width=2)

######主成分分析######
rtn=read.table("D:/data/m-5clog-9008.txt",header=T)
pca.cov=princomp(rtn)
names(pca.cov)
summary(pca.cov)
pca.cov$loadings
screeplot(pca.cov)
pca.corr=princomp(rtn,cor=T)
summary(pca.corr)


######统计因子分析######
rtn=read.table("D:/data/m-barra-9003.txt",header=T)
stat.fac=factanal(rtn,factors=2,method='mle')
stat.fac

stat.fac=factanal(rtn,factors=3,method='mle',scores = "Bartlett")
stat.fac
summary(stat.fac)
plot(loadings(stat.fac))

######渐近主成分回归######
da <- read.table("D:/data/m-apca0103.txt", quote="\"")
rtn=matrix(nrow=36,ncol=40)
for(i in 1:40)
{
  rtn[,i]=da[(36*(i-1)+1):(36*i),3]
}
dim(rtn)
library(FinTS)
apca=apca(rtn,6)



