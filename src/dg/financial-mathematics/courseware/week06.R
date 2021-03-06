# 0. initialize
# (1) set path
setwd('d:/data')
rm(list=ls())
options(digits=4, width=70)

# (2) load packages
library(quantmod)
library(fPortfolio)
library(timeSeries)

# 1. make portfolio line
# (1) set parameters
mu.1 <- 0.10
mu.2 <- 0.05
sig.1 <- 0.08
sig.2 <- 0.04


rhos <- seq(-1, 1, by=0.5)
weights <- seq(0, 1, length=100)

# (2) define a function to create portfolio
twoAssetsPortfolio <- function(mu, sigma, weight, rho){
  # (1) compute expected return and variance of a portfolio
  mu.p <- weight*mu[1] + (1-weight)*mu[2]
  sig2.p <- weight^2*sigma[1]^2+2*weight*(1-weight)*rho*sigma[1]*sigma[2]+(1-weight)^2*sigma[2]^2
  sig.p <- sqrt(sig2.p)
  
  # (2) ouput
  ans <- cbind(weight, mu.p, sig.p)
  colnames(ans) <- c('weights', 'return', 'Std.')
  return(ans)
}

# (3) draw plot
portfolio.results_1 <- matrix(NA, nrow=length(weights), ncol=3)
colnames(portfolio.results_1) <- c('weights', 'return', 'Std.')
portfolio.results_5 <- portfolio.results_4 <- portfolio.results_3 <- portfolio.results_2 <- portfolio.results_1
for (i in seq_along(weights)){
  weight <- weights[i]
  portfolio.results_1[i,] <- twoAssetsPortfolio(mu=c(mu.1, mu.2), sigma=c(sig.1, sig.2), weight=weight, rho=1)
  portfolio.results_2[i,] <- twoAssetsPortfolio(mu=c(mu.1, mu.2), sigma=c(sig.1, sig.2), weight=weight, rho=0.5)
  portfolio.results_3[i,] <- twoAssetsPortfolio(mu=c(mu.1, mu.2), sigma=c(sig.1, sig.2), weight=weight, rho=0)
  portfolio.results_4[i,] <- twoAssetsPortfolio(mu=c(mu.1, mu.2), sigma=c(sig.1, sig.2), weight=weight, rho=-0.5)
  portfolio.results_5[i,] <- twoAssetsPortfolio(mu=c(mu.1, mu.2), sigma=c(sig.1, sig.2), weight=weight, rho=-1)
}

par(mfrow=c(1,1))
plot(portfolio.results_1[,'Std.'], portfolio.results_1[,'return'], xlim=c(0, max(sig.1, sig.2)),
     ylim=c(0.04, max(mu.1, mu.2)), type='l', xlab=expression(sigma[p]), ylab=expression(mu[p]))
lines(portfolio.results_2[,'Std.'], portfolio.results_2[,'return'], lty=2)
lines(portfolio.results_3[,'Std.'], portfolio.results_3[,'return'], lty=3)
lines(portfolio.results_4[,'Std.'], portfolio.results_4[,'return'], lty=4)
lines(portfolio.results_5[,'Std.'], portfolio.results_5[,'return'], lty=5)
points(sig.1, mu.1, pch=20, cex=2, col='red')
points(sig.2, mu.2, pch=20, cex=2, col='red')
legend('bottomright', legend=c(expression(rho==1), expression(rho==0.5), expression(rho==0),
                               expression(rho==-0.5), expression(rho==-1)), lty=1:5)
title(main='组合投资线')
text(sig.1-0.015, mu.1, '证券1')
text(sig.2+0.01, mu.2, '证券2')
# text(sig.1, mu.1, '证券1')
# text(sig.2, mu.2, '证券2')

# 2. solve mean-variance model: fesible portfolio
# (1) read data from Yahoo
# S&P 500 index (vfinx), European stock index (veurx), Emerging markets fund (veiex)
# Long term bond index (vbltx), Short term bond index (vbisx), Pacific stock index (vpacx)
tickers = c("vfinx","veurx","veiex","vbltx","vbisx","vpacx")
getSymbols(tickers, from='2000-12-01', to='2013-12-31')       # or use get.hist.quote
vfinx.prices <- VFINX[,'VFINX.Adjusted']
veurx.prices <- VEURX[,'VEURX.Adjusted']
veiex.prices <- VEIEX[,'VEIEX.Adjusted']
vbltx.prices <- VBLTX[,'VBLTX.Adjusted']
vbisx.prices <- VBISX[,'VBISX.Adjusted']
vpacx.prices <- VPACX[,'VPACX.Adjusted']

# (2) merge data
projectPrices <- merge(vfinx.prices,veurx.prices,veiex.prices,vbltx.prices,vbisx.prices,vpacx.prices)
colnames(projectPrices) <- tickers
head(projectPrices)

# (3) compute log returns
projectReturns <- na.omit(100*diff(log(projectPrices)))
head(projectReturns)

# (4) plot data
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}
plot.zoo(projectPrices,xlab="指数" ,main="组合投资价格")
plot.zoo(projectReturns, panel=my.panel)
par(mfrow=c(2,2))
ret.mat = coredata(projectReturns)
hist(ret.mat[,"vfinx"],main="日收益", xlab="vfinx",ylab="密度", probability=T)
boxplot(ret.mat[,"vfinx"],outchar=T, main='箱线图')
plot(density(ret.mat[,"vfinx"]), main="平滑密度", type="l",xlab="日收益",ylab="密度")
qqnorm(ret.mat[,"vfinx"],main="正态QQ图",xlab="理论分位数",ylab="样本分位数")
qqline(ret.mat[,"vfinx"])
par(mfrow=c(1,1))

# (5) analyze correlation
pairs(ret.mat, col="blue")

# (6) portolio specification
rts <- as.timeSeries(projectReturns)          # change class of data
ewSpec <- portfolioSpec()
nAssets <- ncol(rts)
setWeights(ewSpec) <- rep(1/nAssets, times=nAssets)

# (7) fesible portfolio
ewPortfolio <- feasiblePortfolio(data=rts, spec=ewSpec, constraints='LongOnly')
print(ewPortfolio)

# (8) show results
col <- divPalette(ncol(rts), 'RdBu')
weightsPie(ewPortfolio, radius=0.7, col=col)
weightedReturnsPie(ewPortfolio, radius=0.7, col=col)
mtext(text='等权MV投资组合', side=3, line=1.5, font=2, cex=0.7, adj=0)
covRiskBudgetsPie(ewPortfolio, radius=0.7, col=col)
mtext(text='等权MV投资组合', side=3, line=1.5, font=2, cex=0.7, adj=0)

# 3. solve mean-variance model: minimum risk efficient portfolio
# (1) set target return
minriskSpec <- portfolioSpec()
targetReturn <- getTargetReturn(ewPortfolio@portfolio)['mean']
setTargetReturn(minriskSpec) <- targetReturn     # add the target return to specification

# (2) optimize the portfolio
minriskPortfolio <- efficientPortfolio(data=rts, spec=minriskSpec, constraints='LongOnly')
print(minriskPortfolio)

# (3) show results
col <- qualiPalette(ncol(rts), 'Dark2')
weightsPie(minriskPortfolio, radius=0.7, col=col)
weightedReturnsPie(minriskPortfolio, radius=0.7, col=col)
mtext(text='minimal risk MV portoflio', side=3, line=1.5, font=2, cex=0.7, adj=0)
covRiskBudgetsPie(minriskPortfolio, radius=0.7, col=col)
mtext(text='minimal risk MV portoflio', side=3, line=1.5, font=2, cex=0.7, adj=0)

# 4. solve mean-variance model: global minimum variance portfolio
# (1) solve the portfolio
globminSpec <- portfolioSpec()
globminPortfolio <- minvariancePortfolio(data=rts, spec=globminSpec, constraints='LongOnly')
print(globminPortfolio)

# (2) show results
col <- seqPalette(ncol(rts), 'YlGn')
weightsPie(globminPortfolio, radius=0.7, col=col)
weightedReturnsPie(globminPortfolio, radius=0.7, col=col)
mtext(text='global minimum variance MV portoflio', side=3, line=1.5, font=2, cex=0.7, adj=0)
covRiskBudgetsPie(globminPortfolio, radius=0.7, col=col)
mtext(text='global minimum variance MV portoflio', side=3, line=1.5, font=2, cex=0.7, adj=0)

# 5. solve mean-CVaR portfolios
# (1) compute a feasible portfolio
feasSpec <- portfolioSpec()
setType(feasSpec) <- 'CVaR'
nAssets <- ncol(rts)
setWeights(feasSpec) <- rep(1/nAssets, times=nAssets)
setSolver(feasSpec) <- 'solveRshortExact'
feasPortfolio <- feasiblePortfolio(data=rts, spec=feasSpec, constraints='LongOnly')
print(feasPortfolio)

weightsPie(feasPortfolio, radius=0.7)
text <- 'feasible mean-CVaR portfolio'
mtext(text, side=3, line=1.5, font=2, cex=0.7, adj=0)
weightedReturnsPie(feasPortfolio, radius=0.8, legend=FALSE)
covRiskBudgetsPie(feasPortfolio, radius=0.9, legend=FALSE)

# (2) compute portfolio with the lowest risk for a given return
minriskSpec <- portfolioSpec()
setType(minriskSpec) <- 'CVaR'
setAlpha(minriskSpec) <- 0.05
setSolver(minriskSpec) <- 'solveRshortExact'
setTargetReturn(minriskSpec) <- getTargetReturn(feasPortfolio@portfolio)['mean']
minriskPortfolio <- efficientPortfolio(data=rts, spec=minriskSpec, constraints='LongOnly')
print(minriskPortfolio)

weightsPie(minriskPortfolio, radius=0.7)
text <- 'lowest risk mean-CVaR portfolio'
mtext(text, side=3, line=1.5, font=2, cex=0.7, adj=0)
weightedReturnsPie(minriskPortfolio, radius=0.8)
covRiskBudgetsPie(minriskPortfolio, radius=0.9)

# (3) compute the global minimum risk portfolio
globminSpec <- portfolioSpec()
setType(globminSpec) <- 'CVaR'
setAlpha(globminSpec) <- 0.05
setSolver(globminSpec) <- 'solveRshortExact'
setTargetReturn(globminSpec) <- getTargetReturn(feasPortfolio@portfolio)['mean']
globminPortfolio <- minriskPortfolio(data=rts, spec=globminSpec, constraints='LongOnly')
print(globminPortfolio)

weightsPie(globminPortfolio, radius=0.7)
text <- 'global minmum risk portfolio'
mtext(text, side=3, line=1.5, font=2, cex=0.7, adj=0)
weightedReturnsPie(globminPortfolio, radius=0.8)
covRiskBudgetsPie(globminPortfolio, radius=0.9)
