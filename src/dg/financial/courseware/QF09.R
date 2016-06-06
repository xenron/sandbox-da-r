###### 通道突破 ######

library(xts)
library(xtsExtra)
library(quantmod)
library(TTR)

stk.prc.all <- read.csv("D:/R Quant/Data/Other/prc.csv", header=TRUE)
stk.prc.all.xts <- xts(stk.prc.all[, -1], order.by=as.Date(stk.prc.all[, 1]))

bband.bk.sim <- function(stk.prc.xts, k=20, p=1.65, q=0.8){   #q是交易倍数，表示资金的q分用于交易
  
  stk.prc <- coredata(stk.prc.xts)    #把主要数据取出
  Timeline <- index(stk.prc.xts)      
  End <- length(stk.prc.xts)
  
  MA <- c( rep(0, k), 0)           
  std <- c( rep(0, k), 0)          
  u.bound <- c( rep(0, k), 0)
  signal <- c( rep(0, k), 0)      #交易信号     
  trd.state <- c( rep(0, k), 0)    #记录买卖状态
  share <- c( rep(0, k), 0)       #记录持股份数
  
  cash <- c( rep(1e4, k), 0)    #现金部位  
  value <- c( rep(1e4, k), 0)    #资产价值=股票市值+现金部位
  
  # Sim ----
  
  for( t in k:End ){
    
    stk.prc.pre <- stk.prc[(t-k):t]   
    MA[t] <- mean( stk.prc.pre )
    std[t] <- sd( stk.prc.pre )
    u.bound[t] <- MA[t] + p * std[t]   #布林带上界
    
    signal[t] <- 0      #默认不交易
    if( stk.prc[t] >  u.bound[t] ) signal[t] =  1    #当股票价格超出布林上界时，buy
    if( stk.prc[t-1] > MA[t-1] & stk.prc[t] <= MA[t] ) signal[t] = -1   
    if( stk.prc[t-1] < MA[t-1] & stk.prc[t] >= MA[t] ) signal[t] = -1
    #卖的情况
    
    trd.state[t] <- trd.state[t-1]   
    cash[t] <- cash[t-1]
    share[t] <- share[t-1]
    value[t] <- value[t-1]
    
    #更新交易状态、持股数目、现金金额
    if( trd.state[t-1] == 0 & signal[t] ==  1 ){    
      trd.state[t] <- 1
      share[t] <- ( q * cash[t-1] ) / stk.prc[t]
      cash[t] <- cash[t-1] - share[t]*stk.prc[t]
    }
    
    if( trd.state[t-1] == 1 & signal[t] == -1 ){
      trd.state[t] <- 0
      share[t] <- 0
      cash[t] <- cash[t-1] + share[t-1]*stk.prc[t]
    }
    
    value[t] <- cash[t] + share[t]*stk.prc[t]
  }
  
  res <- cbind(stk.prc, signal, trd.state, share, cash, value)
  names(res) <- c("prc", "signal", "trd.state", "share", "cash", "value")
  
  return(res)
}

value.all <- sapply(1:33, FUN=function(i){
  stk.prc.xts.i <- stk.prc.all.xts[, i]
  res.i <- bband.bk.sim( stk.prc.xts.i )
  value.i <- res.i[, "value"]
  
  return( coredata(value.i) )
})

head(value.all)
value.sum <- xts(apply(value.all, MARGIN=1, sum), 
                 order.by=index(stk.prc.all.xts))

plot(value.sum, type='l', col='darkred', lty=1, lwd=2)

cum.ret <- xts( coredata(value.sum)/rep( coredata(value.sum[1]), 1211), 
                order.by=index(stk.prc.all.xts))

plot( cum.ret, col='darkred', lty=1, lwd=2, 
      main="Cumulative Return : Bollinger Band Breakout")

## 通道(end)


###### 均线系统策略 ######
##  双均线交叉策略 

library(xts)
library(xtsExtra)
library(quantmod)
library(TTR)

mov.avg.sim <- function(stk.prc.xts, k=50, n=7, p=1.05, q=1.10, m=0.8){
  
  stk.prc <- coredata(stk.prc.xts)
  Timeline <- index(stk.prc.xts)
  End <- length(stk.prc)
  
  MA.5  <- SMA(stk.prc, 5)   #计算5日均线
  MA.20 <- SMA(stk.prc, 20)  #计算20日均线
  
  signal    <- c( rep(0, k), 0)
  trd.state <- c( rep(0, k), 0)
  share     <- c( rep(0, k), 0) 
  
  cash  <- c( rep(1e4, k), 0)
  value <- c( rep(1e4, k), 0)
  
  # Sim -----
  
  for( t in k:End ){
    
    signal[t] <- 0
    
    if( sum(MA.5[(t-n):(t-1)] > MA.20[(t-n):(t-1)]) == n 
        & stk.prc[t-1]/MA.20[t-1] > p)   signal[t] <- 1
    
    if( MA.5[t-1] >= MA.20[t-1] & MA.5[t] <= MA.20[t]) signal[t] <- -1
    if( stk.prc[t-1]/MA.20[t-1] > q ) signal[t] <- -1
    
    trd.state[t] <- trd.state[t-1]
    cash[t]  <- cash[t-1]
    share[t] <- share[t-1]
    value[t] <- value[t-1]
    
    if( trd.state[t-1] == 0 & signal[t] ==  1 ){    
      trd.state[t] <- 1
      share[t] <- ( m * cash[t-1] ) / stk.prc[t]
      cash[t] <- cash[t-1] - share[t]*stk.prc[t]
    }
    
    if( trd.state[t-1] == 1 & signal[t] == -1 ){
      trd.state[t] <- 0
      share[t] <- 0
      cash[t] <- cash[t-1] + share[t-1]*stk.prc[t]
    }
    
    value[t] <- cash[t] + share[t]*stk.prc[t]
  }
  
  res <- xts( cbind(stk.prc, MA.5, MA.20, signal, trd.state, share, cash, value),
              order.by=Timeline)
  names(res) <- c("prc", "MA.5", "MA.20","signal", "trd.state", 
                  "share", "cash", "value")
  head(res)
  
  return(res)
}

res <- mov.avg.sim( stk.prc.all.xts[, 3] )
head(res)

plot( res$value )

value.all <- sapply(1:33, FUN=function(i){
  stk.prc.xts.i <- stk.prc.all.xts[, i]
  res.i <- mov.avg.sim( stk.prc.xts.i )
  value.i <- res.i[, "value"]
  
  return( coredata(value.i) )
})

head(value.all)
value.sum <- xts(apply(value.all, MARGIN=1, sum), 
                 order.by=index(stk.prc.all.xts))

plot(value.sum, type='l', col='darkred', lty=1, lwd=2)

cum.ret <- xts( coredata(value.sum)/rep( coredata(value.sum[1]), 1211), 
                order.by=index(stk.prc.all.xts))

plot( cum.ret, col='darkred', lty=1, lwd=2, 
      main="Cumulative Return : Dual Moving Average Crossover")

## 双均线交叉策略(end)


## MACD(begin)

library(xts)
library(xtsExtra)
library(quantmod)
library(TTR)

stk.prc.all <- read.csv("D:/R Quant/Data/Other/prc.csv", header=TRUE)
stk.prc.all.xts <- xts(stk.prc.all[, -1], order.by=as.Date(stk.prc.all[, 1]))

MACD.sim <- function(stk.prc.xts, k=50, m=0.8){
  
  stk.prc <- coredata(stk.prc.xts)
  Timeline <- index(stk.prc.xts)
  End <- length(stk.prc)
  
  macd.line <- MACD(stk.prc, nFast=12, nSlow=26, nSig=9)[, 1]
  signal.line <- MACD(stk.prc, nFast=12, nSlow=26, nSig=9)[, 2]
  
  signal    <- c( rep(0, k), 0)
  trd.state <- c( rep(0, k), 0)
  share     <- c( rep(0, k), 0) 
  
  cash  <- c( rep(1e4, k), 0)
  value <- c( rep(1e4, k), 0)
  
  # Sim -----
  
  for( t in (k+1):End ){
    
    signal[t] <- 0
    
    if( macd.line[t-1] <= signal.line[t-1] & macd.line[t] > signal.line[t])  signal[t] <- 1
    
    if( macd.line[t-1] >= signal.line[t-1] & macd.line[t] < signal.line[t])  signal[t] <- -1
    
    trd.state[t] <- trd.state[t-1]
    cash[t]  <- cash[t-1]
    share[t] <- share[t-1]
    value[t] <- value[t-1]
    
    if( trd.state[t-1] == 0 & signal[t] ==  1 ){    
      trd.state[t] <- 1
      share[t] <- ( m * cash[t-1] ) / stk.prc[t]
      cash[t] <- cash[t-1] - share[t]*stk.prc[t]
    }
    
    if( trd.state[t-1] == 1 & signal[t] == -1 ){
      trd.state[t] <- 0
      share[t] <- 0
      cash[t] <- cash[t-1] + share[t-1]*stk.prc[t]
    }
    
    value[t] <- cash[t] + share[t]*stk.prc[t]
  }
  
  res <- cbind(stk.prc, macd.line, signal.line, 
               signal, trd.state, share, cash, value)
  names(res) <- c("prc", "MACD.line", "signal.line", 
                  "signal", "trd.state", "share", "cash", "value")
  head(res)
  
  return(res)
}

res <- MACD.sim( stk.prc.all.xts[, 4] )
head(res)

plot(res[, "value"], type='l')
res <- as.xts(res, order.by=index(stk.prc.all.xts))
head(res)

plot( merge( res[, c(1,2,3,5)], res[, "value"]/1000 ), screens=1)

value.all <- sapply(1:33, FUN=function(i){
  stk.prc.xts.i <- stk.prc.all.xts[, i]
  res.i <- MACD.sim( stk.prc.xts.i )
  value.i <- res.i[, "value"]
  return( coredata(value.i) )
})

head(value.all)
value.sum <- xts(apply(value.all, MARGIN=1, sum), 
                 order.by=index(stk.prc.all.xts))

plot(value.sum, type='l', col='darkred', lty=1, lwd=2)

cum.ret <- xts( coredata(value.sum)/rep( coredata(value.sum[1]), 1211), 
                order.by=index(stk.prc.all.xts))

plot( cum.ret, col='darkred', lty=1, lwd=2, 
      main="Cumulative Return : MACD")

## MACD(end)

###### 动量 ######

library(xts)
library(xtsExtra)
library(quantmod)
library(TTR)

stk.prc.all <- read.csv("D:/R Quant/Data/Other/prc.csv", header=TRUE)
stk.prc.all.xts <- xts(stk.prc.all[, -1], order.by=as.Date(stk.prc.all[, 1]))

mmtm.sim <- function(stk.prc.xts, k=20, m=0.8){
  
  stk.prc <- coredata(stk.prc.xts)
  Timeline <- index(stk.prc.xts)
  End <- length(stk.prc)
  
  MMTM <- momentum(stk.prc, k)   #动量的计算
  
  signal    <- c( rep(0, k), 0)
  trd.state <- c( rep(0, k), 0)
  share     <- c( rep(0, k), 0) 
  
  cash  <- c( rep(1e4, k), 0)
  value <- c( rep(1e4, k), 0)
  
  # Sim -----
  
  for( t in (k+1):End ){
    
    signal[t] <- 0
    
    if( MMTM[t] > 0 )  signal[t] <- 1
    
    if( MMTM[t] < 0 )  signal[t] <- -1
    
    trd.state[t] <- trd.state[t-1]
    cash[t]  <- cash[t-1]
    share[t] <- share[t-1]
    value[t] <- value[t-1]
    
    if( trd.state[t-1] == 0 & signal[t] ==  1 ){    
      trd.state[t] <- 1
      share[t] <- ( m * cash[t-1] ) / stk.prc[t]
      cash[t] <- cash[t-1] - share[t]*stk.prc[t]
    }
    
    if( trd.state[t-1] == 1 & signal[t] == -1 ){
      trd.state[t] <- 0
      share[t] <- 0
      cash[t] <- cash[t-1] + share[t-1]*stk.prc[t]
    }
    
    value[t] <- cash[t] + share[t]*stk.prc[t]
  }
  
  res <- cbind(stk.prc, MMTM, signal, trd.state, share, cash, value)
  names(res) <- c("prc", "MMTM", "signal", "trd.state", "share", "cash", "value")
  head(res)
  
  return(res)
}

res <- mmtm.sim( stk.prc.all.xts[, 3] )
head(res)

plot(res[, "value"], type='l')
res <- as.xts(res, order.by=index(stk.prc.all.xts))
head(res)

par(mfrow=c(1,1))
plot( merge( res[, c(1,2,4)], res[, "value"]/10000 ), screens=1)

value.all <- sapply(1:33, FUN=function(i){
  stk.prc.xts.i <- stk.prc.all.xts[, i]
  res.i <- mmtm.sim( stk.prc.xts.i )
  value.i <- res.i[, "value"]
  return( coredata(value.i) )
})

head(value.all)
value.sum <- xts(apply(value.all, MARGIN=1, sum), 
                 order.by=index(stk.prc.all.xts))

plot(value.sum, type='l', col='darkred', lty=1, lwd=2)

cum.ret <- xts( coredata(value.sum)/rep( coredata(value.sum[1]), 1211), 
                order.by=index(stk.prc.all.xts))

plot( cum.ret, col='darkred', lty=1, lwd=2, 
      main="Cumulative Return : Momentum")
## 动量(end)


## BL模型(begin)

#调用包xts，BLCOP

library(BLCOP)
library(xts)


#读取五只股票收益率数据，并将其转化为xts格式，同时删除缺失值
ret <- read.csv("D:/R Quant/Data/Other/ret.csv", header=TRUE)
ret <- xts(ret[, -1], order.by=as.Date(ret[, 1]))
ret <- na.omit(ret)

#查看前6行数据
head(ret)

# Set Risk Free Rate
Spec <- portfolioSpec()
setRiskFreeRate(Spec) <- 1.03^(1/360) - 1 

#构建资产选择矩阵，假设分析师认为
#1.前4支股票收益率为日均3.5%，信心水平为90%
#2.两只银行股日均收益比保利地产高2.5%，信心水平为100%
pickMatrix<-matrix(0, nrow=2, ncol=5)
colnames(pickMatrix) <- colnames(ret)
pickMatrix[1,1:4] <- 1
pickMatrix[2,c(1,2,5)]<- c(0.5,0.5,-1)
pickMatrix

#构造看法向量（续上）
q<-c(0.035,0.025)

#构造信心向量（续上）
confidences<- c(90,100)

#构建BLViews类
views<-BLViews(pickMatrix,q,confidences,assetNames=colnames(ret))
views


#用收益均值及其方差协方差矩阵来描述先验分布
priorMeans <- colMeans(ret)
priorMeans

priorVarcov <- cov(ret)
priorVarcov

#计算后验分布,tau是组合的历史误差与看法误差的比例，参照Litterman等当年的假定，tau接近于0,
#此值越小预期收益越远离历史收益，接近主观预期，这里我们采用0.1
posterior<-posteriorEst(views,tau=0.1,priorMeans,priorVarcov)
posterior

#比较某只股票的先验分布与后验分布密度
densityPlots(posterior, assetsSel = "X600018")

#最优化，这里采用的方法是tangencyPortfolio
#The default risk free return is 0
optPorts<-optimalPortfolios.fPort(posterior,optimizer="tangencyPortfolio")
optPorts

weightsPie(optPorts$priorOptimPortfolio)

weightsPie(optPorts$posteriorOptimPortfolio)
