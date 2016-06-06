library(xts)
library(xtsExtra)
library(quantmod)
library(RODBC)
library(FinTS)

######GARCH模型VaR的实现与回测检验######
index.dt <- read.table("D:/R Quant/data/Index/TRD_Index.txt", header=TRUE)
head(index.dt)
SH.index <- index.dt[index.dt$Indexcd == 1, -1]
SH.index <- xts(SH.index[, -1], order.by = as.Date(SH.index$Trddt))
SH.ohlc <- SH.index[, c("Trddt", "Opnindex", "Hiindex", "Loindex", "Clsindex")]
SH.ret <- weeklyReturn(SH.ohlc)

plot(SH.ret^2, type = 'l', main = "Squared Return of SH Index")
plot(abs(SH.ret), type = 'l', main = "Absolute Return of SH Index")


#自相关
acf(SH.ret^2, lty=1, lwd=5, col='darkblue')
pacf(SH.ret^2, lty=1, lwd=5, col='darkred')

#白噪声
Box.test(SH.ret^2, lag = 12)  
ArchTest(SH.ret) 

#GARCH模型构建
library(fGarch)
garch11.spec2=garchFit(~1+garch(1,1),data=SH.ret,trace=F) 
plot(garch11.spec2)

library(rugarch)
garch11.spec <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), 
                           mean.model = list(armaOrder = c(0, 0)))
mod.garch <- ugarchfit(spec = garch11.spec, data=SH.ret)
mod.garch
plot(mod.garch)

#利用GARCH模型计算VaR
garch11.roll <- ugarchroll(garch11.spec, SH.ret, n.start = 120, refit.every = 1, 
                           refit.window = "moving", calculate.VaR = TRUE, 
                           solver = "hybrid", VaR.alpha = 0.05)
report(garch11.roll, type = "VaR", VaR.alpha = 0.05, conf.level = 0.99)

garch11.fcst <- ugarchforecast(mod.garch, n.ahead = 6)
garch11.fcst
ret.fcst <- - qnorm(0.95) * garch11.fcst@forecast$sigmaFor
ret.fcst



######常用金融分析函数######
#读取SAS、SPSS文件
library(foreign)
mydata<-spss.get()

#连接数据库
library(RODBC)
conn <- odbcConnectAccess2007(access.file="D:/R Quant/Data/Stock/Stock.accdb",
                              uid="test",pwd="test")
ZGSH <- sqlQuery(conn , "SELECT Stkcd ,Trddt ,Opnprc ,Hiprc ,Loprc ,Clsprc ,Adjprcwd ,Dretwd
                 FROM Stock WHERE Stkcd = 600028")
ZGSH

stk.query <- "SELECT Stock.Stkcd , Stock.Trddt , Stock.Adjprcwd , Stock.Dsmvosd
FROM Stock
INNER JOIN Company ON Stock.Stkcd = Company.Stkcd
WHERE Company.Listdt <= #1/1/2009#"

data.list.09 <- sqlQuery(conn , stk.query)

stk.query <- "SELECT Stock.Stkcd, Stock.Trddt, Stock.Adjprcwd, Stock.Dsmvosd
FROM Stock
INNER JOIN Company
ON Stock.Stkcd = Company.Stkcd
WHERE Company.Nnindcd = 'J66'"

data.ind.J66 <- sqlQuery(conn, stk.query)

close(conn)

#数据编辑
mydata <- data.frame(age=numeric (0),gender=character (0),weight=numeric (0))
mydata <- edit(mydata)

omit <- read.csv("D:/R Quant/Data/Other/omit.csv",header=T)
omit
omit1 <- na.omit(omit)
omit1

stock <- read.csv("D:/R Quant/Data/Other/price.csv",header=T)
stock
transform(stock ,Opnprc = -Opnprc ,difference = Hipri - Loprc)

names(ZGSH)


#*apply函数族
# apply
d <- read.csv("D:/R Quant/Data/Other/grade.csv",header=T) 
d 
e <- d[,c(2,3)]
apply(e,2,mean)
apply(e,1,sum)


#lapply
x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
lapply(x, quantile)

#sapply
sapply(x, quantile,simplify=FALSE,use.names=FALSE)
sapply(x, quantile, simplify=TRUE )

sapply(1:4,FUN=function(i) {
  sqr <- i^2
  return(sqr)
})


#tapply
e <- d$chinese
f <- ifelse(e>=70,"good","bad")
f 
tapply(e,f,mean)

#mapply
r1 <- rep(1:5, each=2)
r2 <- rep(1:5, times=2)
r1 
r2
mapply(r1, r2, FUN=function(x1, x2) x1+x2)


#并行计算

count <- function(x){
  return (x+1);
}
system.time({
  res <- lapply(1:500000, count)
})

library(parallel)
cl.cores <- detectCores()
cl <- makeCluster(cl.cores)
system.time({
  res <- parLapply(cl, 1:500000,  count)
})
stopCluster(cl)


######财务指标选股######

# 股票选取(begin)

fin <- read.csv("D:/R Quant/data/Finance/Finance Report 2012.csv", 
                header=TRUE)

sort(table(fin$Nnindcd))

fin.car <- fin[fin$Nnindcd == "CSRC汽车制造业", ]

stk.car.list <- fin.car[, "Stkcd"]
stk.car.num <- length(stk.car.list)
stk.car.num

rank.car <- apply(fin.car, MARGIN=2, FUN=function(var) rev(rank(var)))
rank.car <- data.frame(rank.car)

k <- 15
win.select <- ifelse(rank.car$EPS <= k & rank.car$ROE <= k & rank.car$NPM <= k,
                     1, 0)
win.select

fin.win.car <- fin.car[ win.select == 1, ]
fin.lose.car <- fin.car[ win.select == 0, ]

win.list <- fin.win.car$Stkcd
lose.list <- fin.lose.car$Stkcd

win.nme <- fin.win.car$Stknme
lose.nme <- fin.lose.car$Stknme

win.nme
lose.nme

# 股票选取(end)

# 表现实测(begin)

idx.data  <- read.table("D:/R Quant/data/Index/TRD_Index.txt",header=TRUE)

idx.HS300 <- idx.data[idx.data$Indexcd == 902 & as.Date(idx.data$Trddt) >= as.Date("2013-01-01"), 
                      c("Trddt","Clsindex")]
idx.HS300 <- xts(idx.HS300$Clsindex, order.by=as.Date(idx.HS300$Trddt))
names(idx.HS300) <- "HS300"

win.prc.car <- xts( order.by=index(idx.HS300) )

conn <- odbcConnectAccess2007(access.file="D:/R Quant/data/Stock/Stock.accdb",
                              uid="test", pwd="test")

for( stk.cd.i in win.list ){
  
  prc.query <- paste("SELECT Trddt, Adjprcwd
                     FROM Stock 
                     WHERE Trddt >= #1/1/2013# 
                     AND Stkcd = ", stk.cd.i)
  
  stk.prc.i <- sqlQuery(conn, prc.query)
  stk.prc.xts.i <- xts(stk.prc.i$Adjprcwd, order.by=as.Date(stk.prc.i$Trddt))
  
  win.prc.car <- merge(win.prc.car, stk.prc.xts.i, all=TRUE)
  
}

for( stk.cd.i in lose.list ){
  
  prc.query <- paste("SELECT Trddt, Adjprcwd
                     FROM Stock 
                     WHERE Trddt >= #1/1/2013# 
                     AND Stkcd = ", stk.cd.i)
  
  stk.prc.i <- sqlQuery(conn, prc.query)
  stk.prc.xts.i <- xts(stk.prc.i$Adjprcwd, order.by=as.Date(stk.prc.i$Trddt))
  
  lose.prc.car <- merge(lose.prc.car, stk.prc.xts.i, all=TRUE)
  
}

lose.prc.car <- xts( order.by=index(idx.HS300) )

close(conn)

head(lose.prc.car)
head(win.prc.car)

plot(win.prc.car, screens=1)
plot(lose.prc.car, screens=1)

plot(na.approx(lose.prc.car), screens=1)

win.mean <- xts(rowMeans(na.locf(win.prc.car[, -3])), order.by=index(win.prc.car) )
lose.mean <- xts(rowMeans(na.locf(lose.prc.car[, -18])), order.by=index(lose.prc.car) )

rep(coredata(win.mean[1]), length(win.mean))
coredata(lose.mean[1])

win.ret <- win.mean / rep(coredata(win.mean[1]), length(win.mean))
lose.ret <- lose.mean / rep(coredata(lose.mean[1]), length(win.mean))


plot(win.mean)
plot(lose.mean)

plot(merge(win.ret, lose.ret)[1:30], screens=1, 
     col=c('darkred', 'darkgreen'), 
     main="Cumulative Return of Win and Lose Groups")

# 表现实测(end)

## 量化选股(end)




######单资产CAPM计算实例######

library(RODBC)
library(xts)
library(xtsExtra)

mkt.all.data <- read.table("D:/R Quant/data/Index/TRD_Index.txt",header=TRUE)
head(mkt.all.data)
ret.mkt <- mkt.all.data[mkt.all.data$Indexcd == 902, c("Trddt","Retindex")]
ret.mkt.xts <- xts(ret.mkt$Retindex, order.by=as.Date(ret.mkt$Trddt))
names(ret.mkt.xts) <- "ret.mkt"
head(ret.mkt.xts)
rf <- (1.0325)^(1/360) - 1
conn <- odbcConnectAccess2007(access.file="D:/R Quant/data/Stock/Stock.accdb",
                              uid="test",pwd="test")

ret.stk <- sqlQuery(channel=conn, "SELECT Trddt,Dretwd 
                    FROM Stock 
                    WHERE Stkcd = 600000")
ret.stk.xts <- xts(ret.stk$Dretwd, order.by=as.Date(ret.stk$Trddt))
names(ret.stk.xts) <- "ret.stk"
head(ret.stk.xts)
ret.stk.mkt.xts <- merge(ret.stk.xts, ret.mkt.xts, all=TRUE) - rf
head(ret.stk.mkt.xts)
plot( ret.stk.mkt.xts, main="Stock Return and Market Return - TS Plot" )
plot( coredata(ret.stk.mkt.xts), col='darkblue', pch=20,
      main="Stock Return vs Market Return")
reg.CAPM <- lm(data=ret.stk.mkt.xts, ret.stk~ ret.mkt)
summary(reg.CAPM)
plot( coredata(ret.stk.mkt.xts), col='lightgreen', pch=20,
      main="Stock Return, Market Return and CAPM Line")
abline(reg.CAPM, col='darkred', lty=2, lwd=2)
coef(reg.CAPM)
alpha <- coef(reg.CAPM)[1]

## 单资产CAPM计算实例(end)


###### 单资产月度Alpha的计算 ######

ret.split <- split(ret.stk.mkt.xts, f='months')
ret.split[[5]]
num.month <- length(ret.split)
num.month
alpha.monthly <- c( 600000 )

for( t in 1:num.month ){
  
  ret.stk.mkt.t <- ret.split[[t]]
  
  alpha.t <- NA
  
  if( nrow(ret.stk.mkt.t) > 10 ) {
    reg.CAPM.t <- lm(data=ret.stk.mkt.t, ret.stk ~ ret.mkt)
    alpha.t <- coef(reg.CAPM.t)[1]
  }
  
  alpha.monthly <- c(alpha.monthly, alpha.t)
}

month.label <- paste("Alpha", rep(2009:2013, each=12), rep(1:12, times=5), sep="/")
rep(2009:2013, each=12)
rep(1:12, times=5)
month.label

names(alpha.monthly) <- c("Stkcd",  month.label)
alpha.monthly

## 单资产月度Alpha的计算(end)



###### CAPM模型Alpha批量化计算 ######

library(parallel)

stk.list <- sqlQuery(channel=conn, "SELECT DISTINCT Stkcd FROM Stock")[[1]]
cl.cores <- detectCores()
cl <- makeCluster(cl.cores)

library(RODBC)
library(xts)

mkt.all.data <- read.table("D:/R Quant/data/Index/TRD_Index.txt",header=TRUE)
head(mkt.all.data)

ret.mkt <- mkt.all.data[mkt.all.data$Indexcd == 902, c("Trddt","Retindex")]
ret.mkt.xts <- xts(ret.mkt$Retindex, order.by=as.Date(ret.mkt$Trddt))
names(ret.mkt.xts) <- "ret.mkt"
head(ret.mkt.xts)

rf <- (1.0325)^(1/360) - 1

conn <- odbcConnectAccess2007(access.file="D:/R Quant/data/Stock/Stock.accdb",
                              uid="test",pwd="test")
clusterEvalQ(cl,source("CAPMsource.R"))

alpha.CAPM <- parSapply(cl=cl, stk.list, FUN=function(stk.i){ 
  
  ret.query <- paste("SELECT Trddt,Dretwd FROM Stock WHERE Stkcd = ", stk.i)
  ret.i <- sqlQuery(conn, ret.query)  
  ret.i.xts <- xts(ret.i$Dretwd, order.by=as.Date(ret.i$Trddt))  
  ret.i.mkt.xts <- merge(ret.i.xts, ret.mkt.xts, all=TRUE)
  ret.i.mkt.xts <- ret.i.mkt.xts - rf
  names(ret.i.mkt.xts) <- c("ret.i", "ret.mkt")
  
  ret.split <- split(ret.i.mkt.xts, f="months")
  num.month <- length(ret.split)
  
  alpha.CAPM.i <- c(stk.i)
  for(t in 1:num.month) {
    ret.i.mkt.t <- na.omit(ret.split[[t]])
    num.day <- length(index(ret.i.mkt.t))
    
    alpha.CAPM.i.t <- NA
    
    if( num.day >10 ){     
      reg.i.t <- lm(data=ret.i.mkt.t, ret.i~ret.mkt)     
      alpha.CAPM.i.t <- coef(reg.i.t)[1]
    }
    alpha.CAPM.i <- c(alpha.CAPM.i, alpha.CAPM.i.t)
  } 
  
  return(alpha.CAPM.i)  
})

alpha.CAPM.df <- data.frame(t(alpha.CAPM))
label <- paste("Alpha_", rep(2009:2013, each=12), "/", 1:12, sep="")
names(alpha.CAPM.df) <- c("Stkcd", label)
label
head(alpha.CAPM.df, 3)

write.csv(alpha.CAPM.df, "CAPM.csv", row.names=FALSE)

## CAPM模型Alpha批量化计算(end)



###### 三因子模型 ######

# 单资产(begin)

library(xts)
library(xtsExtra)
library(RODBC)
library(parallel)

thr.factor.all <- read.csv("D:/R Quant/Data/Thr Factors/THR_Factor.csv", header=TRUE)
head(thr.factor.all)

thr.factor <- thr.factor.all[thr.factor.all$Excgflg == 0 & thr.factor.all$Mktflg == 'A', 
                             c("Date", "Rmrf", "Smb", "Hml")]
head(thr.factor)

thr.factor.xts <- xts(thr.factor[, -1], order.by=as.Date(thr.factor[, 1]))

conn <- odbcConnectAccess2007(access.file="D:/R Quant/data/Stock/Stock.accdb",
                              uid="test",pwd="test")

stk.ret <- sqlQuery(channel=conn, "SELECT Trddt,Dretwd 
                    FROM Stock 
                    WHERE Stkcd = 600000")
head(stk.ret)

stk.ret.xts <- xts(stk.ret$Dretwd, order.by=as.Date(stk.ret$Trddt))
names(stk.ret.xts) <- "stk.ret"
head(stk.ret.xts)

plot(stk.ret.xts, col='darkblue', main="Daily Return of PU FA YIN HANG")

reg.data.xts <- merge( stk.ret.xts, thr.factor.xts, all=TRUE)
head(reg.data.xts)

pairs( coredata(reg.data.xts), pch=20, col='darkgreen', 
       main="Pairs Plot : Return of PFYH and Three Factors")

reg.res <- lm(data=reg.data.xts, stk.ret~ Rmrf + Smb + Hml)
summary(reg.res)

coef(reg.res)

alpha <- coef(reg.res)[1]
alpha

# 单资产(end)

# 批量生成(begin)

stk.list <- sqlQuery(channel=conn, "SELECT DISTINCT Stock.Stkcd 
                     FROM Stock
                     INNER JOIN Company
                     ON Stock.Stkcd = Company.Stkcd
                     WHERE Company.Listdt <= #1/1/2009#")
cl.cores <- detectCores()
cl <- makeCluster(cl.cores)

library(RODBC)
library(xts)

conn <- odbcConnectAccess2007(access.file="D:/R Quant/data/Stock/Stock.accdb",
                              uid="test",pwd="test")

stk.list <- sqlQuery(channel=conn, "SELECT DISTINCT Stock.Stkcd 
                     FROM Stock
                     INNER JOIN Company
                     ON Stock.Stkcd = Company.Stkcd
                     WHERE Company.Listdt <= #1/1/2009#")[[1]]

thr.factor.all <- read.csv("D:/R Quant/Data/Thr Factors/THR_Factor.csv", header=TRUE)

thr.factor <- thr.factor.all[thr.factor.all$Excgflg == 0 & thr.factor.all$Mktflg == 'A', 
                             c("Date", "Rmrf", "Smb", "Hml")]

thr.factor.xts <- xts(thr.factor[, -1], order.by=as.Date(thr.factor[, 1]))

clusterEvalQ(cl, source(file="D:/R Quant/Rcode/Chap 5/Thr Factor/Thr Factor Source.R"))

alpha.thr.factor <- t(parSapply(cl=cl, stk.list, FUN=function(stk.i){ 
  
  ret.query <- paste("SELECT Trddt,Dretwd FROM Stock WHERE Stkcd = ", stk.i)
  ret.i <- sqlQuery(conn, ret.query)  
  
  ret.xts.i <- xts(ret.i$Dretwd, order.by=as.Date(ret.i$Trddt))  
  reg.data.xts.i <- merge(ret.xts.i, thr.factor.xts, all=TRUE)
  
  reg.data.split <- split(reg.data.xts.i, f="years")
  num.years <- length(reg.data.split)
  
  res.i <- c(stk.i)
  
  for( t in 1:num.years ) {
    
    reg.data.t <- na.omit(reg.data.split[[t]])
    num.days <- length(index(reg.data.t))
    
    alpha.i.t <- NA
    
    if( num.days > 90 ){       
      reg.i.t <- lm(data=reg.data.t, ret.xts.i~ Rmrf + Smb + Hml)      
      alpha.i.t <- coef(reg.i.t)[1]
    }
    
    res.i <- c(res.i, alpha.i.t)   
  } 
  
  return(res.i)  
}))

alpha.thr.factor <- data.frame(alpha.thr.factor)
names(alpha.thr.factor) <- c("Stkcd", "Y2009", "Y2010", 
                             "Y2011", "Y2012", "Y2013")
head(alpha.thr.factor)

write.csv(alpha.thr.factor, "alpha.thr.factor.res.csv", row.names=FALSE)

close(conn)
stopCluster(cl)

na.omit(alpha.thr.factor[alpha.thr.factor$Y2013 > 0.005, c("Stkcd", "Y2013")])

# 批量生成(end)




