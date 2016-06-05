###### 喷气式飞机燃料的跨产品对冲(begin)######

library(xts)
library(urca)

prices <- read.zoo("D:/R Quant/Data/Other/JetFuelHedging.csv", sep = ",",
                   FUN = as.yearmon, format = "%Y-%m", header = TRUE)
simple_mod <- lm(diff(prices$JetFuel) ~ diff(prices$HeatingOil)+0)
summary(simple_mod)

plot(prices$JetFuel, main = "Jet Fuel and Heating Oil Prices",
     xlab = "Date", ylab = "USD")
lines(prices$HeatingOil, col = "red")

jf_adf <- ur.df(prices$JetFuel, type = "drift")
summary(jf_adf)


ho_adf <- ur.df(prices$HeatingOil, type = "drift")
summary(ho_adf)

mod_static <- summary(lm(prices$JetFuel ~ prices$HeatingOil))
error <- residuals(mod_static)
error_cadf <- ur.df(error, type = "none")
summary(error_cadf)

djf <- diff(prices$JetFuel)
dho <- diff(prices$HeatingOil)
error_lag <- lag(error, k = -1)
mod_ecm <- lm(djf ~ dho + error_lag)
summary(mod_ecm)

### 喷气式飞机燃料的跨产品对冲(end)###


######## 中国石化与中国人寿配对交易实例(begin)######

library(RODBC)
library(xts)
library(xtsExtra)
library(quantmod)
library(urca)


#
prc <- read.csv("D:/R Quant/Data/Other/prc.csv", header=TRUE)
prc.xts <- xts(prc[, -1], order.by=as.Date(prc[, 1]))

prc.pair <- prc.xts[, c("Stk_600028", "Stk_601628")] 

plot.xts(prc.pair, screens=1, main="Prices of ZGSH and ZGRS", lwd=2,
         auto.legend=TRUE, legend.loc='topright')
prc.A <- prc.pair[, 1]
prc.B <- prc.pair[, 2]

St <- 300

prc.trn.A <- prc.A[1:St]
prc.trn.B <- prc.B[1:St]

cor(prc.trn.A, prc.trn.B)

reg.res <- lm(prc.trn.A~ prc.trn.B)
summary(reg.res)

alpha <- coef( reg.res )[1]
beta <- coef( reg.res )[2]
spread <- prc.A - beta*prc.B - alpha
plot(spread, main="Spread", lwd=2, col='darkblue', main="Spread Series")

ur.res <- ur.df( coredata(resid(reg.res)), type='none')
summary(ur.res)

sigma <- sd(spread)
phi.1 <- 0.5
phi.2 <- 1.2
theta <- 2.5


plot(coredata(spread), type='l', col='lightgreen', lwd=2, 
     main='Spread and State Zone')
abline( h=c(-phi.2*sigma, phi.2*sigma), col='darkred', lwd=2, lty=2)
abline( h=c(-phi.1*sigma, phi.1*sigma), col='darkblue', lwd=2, lty=4)
abline( h=c(-theta*sigma, theta*sigma), col='gray30', lwd=2, lty=3)


hist(spread, breaks=100, col='lightgreen', border=FALSE)
abline(v=c(-phi.2*sigma, phi.2*sigma), col='darkred', lwd=2, lty=2)
abline(v=c(-phi.1*sigma, phi.1*sigma), col='darkblue', lwd=3, lty=4)
abline(v=c(-theta*sigma, theta*sigma), col='gray30', lwd=3, lty=3)


state <- cut(spread, breaks=c(-Inf, -theta*sigma, -phi.2*sigma, -phi.1*sigma, 
                              phi.1*sigma, phi.2*sigma, theta*sigma, Inf))
state <- as.numeric(state) - 4
plot( merge(spread, state), col=c('darkblue', 'darkgreen'),
      lwd=2, main='Spread and State Line')


End <- length(spread)
End

# 1 means open, 2 means close

End <- length(spread)
signal <- c( rep(0, St), 0)

for( t in (St+1):End ){
  
  signal[t] <- 0
  if( state[t-1] == -2 & state[t] >  -2 ) signal[t] <- -1
  if( state[t-1] <= -1 & state[t] >  -1 ) signal[t] <- -2
  if( state[t-1] >= -2 & state[t] == -3 ) signal[t] <- -2
  if( state[t-1] ==  2 & state[t] <   2 ) signal[t] <-  1
  if( state[t-1] >=  1 & state[t] <   1 ) signal[t] <-  2
  if( state[t-1] <=  2 & state[t] ==  3 ) signal[t] <-  2
  
}

trd.state <- c( rep(0, St), 0)

for( t in (St+1):End ){
  
  trd.state[t] <- trd.state[t-1]
  
  if( trd.state[t-1] == 0 & signal[t] == 1 ){
    trd.state[t] <- 1
  } 
  
  if( trd.state[t-1] == 0 & signal[t] == -1 ){
    trd.state[t] <- -1
  } 
  
  if( trd.state[t-1] != 0 & abs(signal[t]) == 2 ){
    trd.state[t] <- 0
  }
  
}


plot(coredata(spread), type='l', col='lightgreen', lwd=2, 
     main='Spread and State Zone')
lines(trd.state, col='red', lty=1, lwd=2)
abline( h=c(-phi.2*sigma, phi.2*sigma), col='darkred', lwd=1, lty=2)
abline( h=c(-phi.1*sigma, phi.1*sigma), col='darkblue', lwd=1, lty=4)
abline( h=c(-theta*sigma, theta*sigma), col='gray30', lwd=1, lty=4)

plot( merge(prc.A, beta*coredata(prc.B), spread, trd.state), screens=1, 
      auto.legend=TRUE, legend.loc='topright',
      main="Prices, Spread, and Trading Line")


cash <- c( rep(0, St), 0 )
share.B <- c( rep(0, St), 0)
share.A <- c( rep(0, St), 0)
value <- c( rep(0, St), 0)

for( t in (St+1):End ) {
  
  if( trd.state[t-1]==0 & trd.state[t] == 0 ){  
    share.B[t] <- 0
    share.A[t] <- 0
    cash[t] <- cash[t-1]    
  }
  
  if( trd.state[t-1] == 0 & trd.state[t] != 0 ){
    share.A[t] <- trd.state[t]
    share.B[t] <- beta*trd.state[t]
    cash[t] <- cash[t-1] + prc.A[t]*share.A[t] - prc.B[t]*share.B[t]
  }
  
  if( trd.state[t-1] != 0 & trd.state[t] !=0 ){
    share.B[t] <- share.B[t-1]
    share.A[t] <- share.A[t-1]
    cash[t] <- cash[t-1]    
  }
  
  if( trd.state[t-1] !=0 & trd.state[t] == 0 ){
    share.B[t] <- 0
    share.A[t] <- 0
    cash[t] <- cash[t-1] - prc.A[t]*share.A[t] + prc.B[t]*share.B[t]
  } 
  
  value[t] <- cash[t] - prc.A[t]*share.A[t] + prc.B[t]*share.B[t]
  
}

sim.res.xts <- merge(spread, signal, trd.state, cash, value)
plot(sim.res.xts, screens=1)

plot(sim.res.xts[, "value"], col='darkred', lty=1, lwd=2, main="Value")

write.csv(sim.res.xts, "sim.res.csv")

### 中国石化与中国人寿配对交易实例(end)###


