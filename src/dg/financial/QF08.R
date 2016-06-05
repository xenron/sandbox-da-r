######资产组合######
#最小方差求解函数
minvariance <- function(assets, mu = 0.005) {
  return <- log(tail(assets, -1) / head(assets, -1))
  Q <- rbind(cov(return), rep(1, ncol(assets)),
             colMeans(return))
  Q <- cbind(Q, rbind(t(tail(Q, 2)), matrix(0, 2, 2)))
  b <- c(rep(0, ncol(assets)), 1, mu)
  solve(Q, b)
}

#下载数据
library(Quandl)
IT <- Quandl('USER_1KR/1KT',start_date = '2008-01-01', end_date = '2012-12-31')
str(IT)
head(IT)

Quandl.auth("xPoug42ozpP1ETJmn2zQ")
IT <- Quandl('USER_1KR/1KT',start_date = '2008-01-01', end_date = '2012-12-31')
help(Quandl)

#计算收益率
assets <- IT[, -1]
return <- log(tail(assets, -1) / head(assets, -1))
head(return)

#利用returns函数计算收益率
library(timeSeries)
assets<-ts(assets)
head(returns(assets))

#将最小方差求解函数的命令逐行运行
Q <- rbind(cov(return), rep(1, ncol(assets)), colMeans(return))
round(Q, 5)
Q <- cbind(Q, rbind(t(tail(Q, 2)), matrix(0, 2, 2)))
round(Q, 5)

mu <- 0.005
b <- c(rep(0, ncol(assets)), 1, mu)
b
solve(Q, b)

#比较效果
minvariance(IT[, -1])


#有限边界计算
frontier <- function(assets) {
  return <- log(tail(assets, -1) / head(assets, -1))
  Q <- cov(return)
  n <- ncol(assets)
  r <- colMeans(return)
  Q1 <- rbind(Q, rep(1, n), r)
  Q1 <- cbind(Q1, rbind(t(tail(Q1, 2)), matrix(0, 2, 2)))
  rbase <- seq(min(r), max(r), length = 100)
  s <- sapply(rbase, function(x) {
    y <- head(solve(Q1, c(rep(0, n), 1, x)), n)
    y %*% Q %*% y
  })
  plot(s, rbase, xlab = 'Return', ylab = 'Variance')
}

frontier(return)
#利用fPortfolio包计算有效前沿
library(timeSeries)
IT <- timeSeries(IT[, 2:6], IT[, 1])
log(lag(IT) / IT)
IT_return <- returns(IT)
chart.CumReturns(IT_return, legend.loc = 'topleft', main = '')
library(fPortfolio)
plot(portfolioFrontier(IT_return))
Spec = portfolioSpec()
setSolver(Spec) = "solveRshortExact"
Frontier <- portfolioFrontier(as.timeSeries(IT_return), Spec, > constraints = "Short")
frontierPlot(Frontier, col = rep('orange', 2), pch = 19)
monteCarloPoints(Frontier, mcSteps = 1000, cex = 0.25, pch = 19)
grid()


#资本市场线
n <- 6; mu <- 0.005
Q <- cbind(cov(return), rep(0, n - 1))
Q <- rbind(Q, rep(0, n))
r <- c(colMeans(return), rf)
Q <- rbind(Q, rep(1, n), r)
Q <- cbind(Q, rbind(t(tail(Q, 2)), matrix(0, 2, 2)))
b <- c(rep(0, n), 1, mu)
round(Q, 6)
b
w <- solve(Q, b)
w <- head(w, -3)
w / sum(w)

#另一种方法
Spec <- portfolioSpec()
setSolver(Spec) <- "solveRshortExact"
setTargetReturn(Spec) <- mean(colMeans(IT_return))
efficientPortfolio(IT_return, Spec, 'Short')
minvariancePortfolio(IT_return, Spec, 'Short')
minriskPortfolio(IT_return, Spec)
maxreturnPortfolio(IT_return, Spec)


###### 投资组合风险-收益计算与模拟 ######

library(RODBC)
library(xts)
library(xtsExtra)
library(quantmod)
library(TTR)
library(fPortfolio)

#读取数据（随机选取上证50指数中的5支成分股）
ret <- read.csv("D:/R Quant/Data/Other/ret.csv", header=TRUE)
head(ret)
ret <- xts(ret[, -1], order.by=as.Date(ret[, 1]))
#统计缺失值的数目
na.num <- apply( is.na(ret), MARGIN=2, sum)
na.num
#去除缺失值
ret <- na.omit(ret)
#计算累积收益率
cumret <- cumprod( 1 + ret )

plot.xts(ret, main = "Daily Return of 6 Stocks (Y2009-Y2013)")
plot.xts(cumret, screens=1, main="Cumulative Return of 6 Stocks (Y2009-Y2013)")

# 股票间收益率相关性的度量
head(data.frame(ret))
pairs(data.frame(ret), pch=20, col='darkblue',
      main="Correlations among 6 Stocks Daily Returns")

cov.mat <- cov(ret)
cov.mat
corr.mat <- cor(ret)
corr.mat


# 投资组合风险——收益计算
w.g <- c(0.1, 0.3, 0.2, 0.1, 0.3)

ret.g <- ret %*% w.g
ret.g <- xts(ret.g, order.by=index(ret))
cumret.g <- cumprod(1+ret.g)
mean(ret.g)
var(ret.g)
w.g %*% cov(ret) %*% w.g

plot.xts(cumret.g)

cumret.g.df <- data.frame(index(cumret.g), cumret.g)
names(cumret.g.df) <- c("Trddt", "CumRet.g")

# 随机资产配置市场表现模拟

set.seed(123)
L <- 1001
w.mat <- t(sapply(1:L, FUN=function(i){  
  weight <- runif(5, min=0, max=1)
  weight <- weight/sum(weight)
  
  return(weight)
}))

ret.sim <- sapply(1:L, FUN=function(i){
  w.i <- w.mat[i, ]
  ret <- ret %*% w.i
  
  return(ret)  
})

ret.sim <- xts(ret.sim, order.by=index(ret))

#
ret.sim.sample <- ret.sim[, sample(1:dim(ret.sim)[2], 100)]
cumret.sim.sample <- cumprod(1+ret.sim.sample)
plot.xts(cumret.sim.sample, screens=1, lty=2, col='lightgreen',
         main="Cumulative Return of Sample Portfolios")

# 均值方差方程模拟
mu.sim <- apply(ret.sim, MARGIN=2, mean)
var.sim <- apply(ret.sim, MARGIN=2, var)

mu.var.sim <- data.frame(cbind(var.sim, mu.sim))
head(mu.var.sim)

#无卖空限制的最优投资组合求解函数

min.var <- function(ret.exp, mu){
  
  n <- dim(ret)[2]
  Q <- cov(ret)
  r <- ret.exp
  L1 <- cbind(Q, rep(1, n), r)
  L2 <- rbind( c(rep(1, n), 0, 0), c(r, 0, 0) )
  L <- rbind(L1, L2)
  b <- c( rep(0, n), 1, mu)
  
  solve.res <- solve(L, b)
  
  wt <- solve.res[1:n]
  ret.mu <- r %*% wt
  ret.var <- wt %*% Q %*% wt
  
  return( c(ret.mu, ret.var, wt) )  
  
}

#有效边界的计算
ret.exp <- apply(ret, MARGIN=2, FUN=mean)
step <- seq(-0.0002, 0.0012, by=0.000002)
frontier.res <- t(sapply( step, FUN=function(mu) min.var(ret.exp, mu) ))
frontier.res <- data.frame(frontier.res)
names(frontier.res)[1:2] <- c("mu", "var")
head(frontier.res)
dim(frontier.res)


#训练数据与测试数据
ret.train <- ret["2009-01-01/2012-12-31"]   
ret.test <- ret["2013-01-01/2013-03-31"]

#实际效果检验
mu <- 0.0006
ret.exp <- apply(ret.train, MARGIN=2, FUN=mean)
train.res <- min.var(ret.exp, mu)
wt.res <- train.res[3:length(train.res)]
wt.res
ret.res <- ret.test %*% wt.res
ret.res <- xts(ret.res, order.by=index(ret.test))
cumret.res <- cumprod(1+ret.res)
plot.xts(cumret.res)

L <- 100
w.sim <- t(sapply(1:L, FUN=function(i){  
  weight <- runif(5, min=0, max=1)
  weight <- weight/sum(weight)
  
  return(weight)
}))

ret.test.sim <- sapply(1:L, FUN=function(i){
  w.i <- w.sim[i, ]
  ret <- ret.test %*% w.i
  
  return(ret)  
})
ret.test.sim <- xts(ret.test.sim, order.by=index(ret.test))

cumret.test.sim <- cumprod(1+ret.test.sim)
dim(cumret.test.sim)
plot.xts( merge(cumret.test.sim, cumret.res), 
          col=c(rep('lightgreen', 100), 'darkblue'),
          screens=1)

