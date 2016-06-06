###### A股市场可配对交易股票检验(begin)######

prc <- read.csv("D:/R Quant/data/other/prc.csv",header=TRUE)
prc.xts <- xts(prc[, -1], order.by=as.Date(prc[, 1]))

# - Find pairs  ---------
# Ur means unit root test. 
# Here uses Engle Granger Two Step Method to test if co-integration.
N<-33
No.ij <- t(combn(1:N,2))
head(No.ij)

St <- 601

pairs.test.res <- t(mapply(No.ij[, 1], No.ij[, 2], FUN=function(i,j){
  
  #Pre Data
  stk.i <- prc.xts[1:(St-1), i]
  stk.j <- prc.xts[1:(St-1), j]
  
  cor.pair <- cor(stk.i, stk.j)   
  reg.pair <- lm(stk.i~ stk.j)
  coefs <- coef(reg.pair)
  error <- as.numeric(reg.pair$residuals)
  ur <- ur.df(error,type="none",lags=5,selectlags="AIC")
  # Notice that ur.df() returns a s4 class:urca, 
  # we should use @ instead of $
  ur.stat <- ur@teststat 
  ur.signif <- ur@cval
  
  signif.level <- ""
  if ( ur.stat < ur.signif[1] ) { signif.level <- "***" 
  } else if ( ur.stat < ur.signif[2] ) { signif.level <- "**"    
  } else if ( ur.stat < ur.signif[3] ) { signif.level <- "*"    
  } else { signif.level <- "" }
  
  Flag <- 0
  if( ur.stat < ur.signif[1] && cor.pair > 0.85 ) { Flag <- 1 }
  
  ret <- c(i, j, names(stk.i), names(stk.j), cor.pair, 
           coefs[1], coefs[2], ur.stat, signif.level, Flag)
  return(ret)
  
}))

pairs.test.res <- data.frame(pairs.test.res)
names(pairs.test.res) <- c("No.i","No.j","Nme.i","Nme.j","Corr",
                           "Alpha", "Beta", 
                           "Ur.stat", "Signif.level","Flag") 
head(pairs.test.res)
head( pairs.test.res[pairs.test.res$Flag == 1, ] )

### A股市场可配对交易股票检验(end) ###



###### 期货市场Copula高频套利策略实例(begin)######

library(xts)
library(copula)

ag <- read.csv("D:/R Quant/Data/High Freq/ag0613.csv", header=TRUE)[, 1]
au <- read.csv("D:/R Quant/Data/High Freq/au0613.csv", header=TRUE)[, 1]

plot(scale(ag), type='l', col='darkgreen', main="Standadized Prices")
lines(scale(au), col='darkred')

logReturn <- function(prc) { log( prc[-1]/prc[-length(prc)] ) }

ret.ag <- logReturn(ag)
ret.au <- logReturn(au)

plot(ret.ag, ret.au, pch=20, col='darkgreen', main="Scatterplot of the Returns")

fGumbel<-function(u,v,a) { -((-log(u))^(1/a)+(-log(v))^(1/a))^a }
Gumbel<-function(u,v,a) { exp(fGumbel(u,v,a)) }
p1.Gumbel<-function(u,v,a) { exp(fGumbel(u,v,a))*(-log(u))^(-1+1/a)*(((-log(u))^(1/a)+(-log(v))^(1/a))^(-1+a))/u } 
p2.Gumbel<-function(u,v,a) { exp(fGumbel(u,v,a))*(-log(v))^(-1+1/a)*(((-log(u))^(1/a)+(-log(v))^(1/a))^(-1+a))/v }

Misp.Idx<-function(r1, r2){ 
  
  frac1 <- ecdf(r1)    
  frac2 <- ecdf(r2)
  
  size<-length(r1)
  
  xpar.1 <- c()
  xpar.2 <- c()
  
  for(i in 1:size)         
  {                             
    xpar.1[i] <- frac1(r1[i])                        
    xpar.2[i] <- frac2(r2[i])
  }
  
  u0 <- pobs( cbind(xpar.1, xpar.2) )
  gumbel.cop<- gumbelCopula(3,dim=2)           
  fit.ml <- fitCopula(gumbel.cop, u0)      
  
  alpha <- fit.ml@estimate          
  
  a <- alpha
  u <- frac1(r1[size])
  v <- frac2(r2[size])
  
  mis1 <- eval(p1.Gumbel(u,v,a))
  mis2 <- eval(p2.Gumbel(u,v,a))
  
  return( c(mis1, mis2) )
}

# test: 
# Misp.Idx(ret.ag[1:300], ret.au[1:300])

misp.idx.t <- c()
misp.idx.1 <- c()
misp.idx.2 <- c()

trd.prc.1 <- c()
trd.prc.2 <- c()

position <- c()
position.t <- 0

profit <- c()
m0 <- 100000

d1 <- 0.27
d2 <- 0.2
d3 <- 1.7
d4 <- 0.9
d5 <- 0.1
d6 <- 0.5

p1 <- ag
p2 <- au

p10 <- 0
p20 <- 0

k <- 200

for(i in (k+1):(length(p1)-1)){
  
  p.10 <- p1[i+1]
  p.20 <- p2[i+1]
  
  r1 <- logReturn(p1[(i-k):i])
  r2 <- logReturn(p2[(i-k):i])
  
  misp.idx.t <- Misp.Idx(r1,r2)     
  misp.idx.1 <- c(misp.idx.1, misp.idx.t[1])
  misp.idx.2 <- c(misp.idx.2, misp.idx.t[2])
  
  if( is.nan(misp.idx.t[1]) || is.nan(misp.idx.t[2]) ){ 
    position.t  <- position.t
    position <- c(position, position.t)
  }
  else{
    if( position.t == 0 ){
      if( misp.idx.t[1] > d1 & misp.idx.t[2] < d2 ){     
        trd.prc.1 <- c(trd.prc.1, p.10) 
        trd.prc.2 <- c(trd.prc.2, p.20) 
        vol.1 <-  m0*p.20/(p.10+p.20)
        vol.2 <-  -m0*p.10/(p.10+p.20)
        position.t  <-  1
        position <- c(position, position.t) 
      }
      else {
        if( misp.idx.t[1] < d3 && misp.idx.t[2] > d4 ){
          trd.prc.1 <- c(trd.prc.1, p.10) 
          trd.prc.2 <- c(trd.prc.2, p.20) 
          vol.1 <-  -m0*p.20/(p.10+p.20)
          vol.2 <-   m0*p.10/(p.10+p.20)
          position.t  <-  -1
          position <- c(position, position.t)
        }
        else{ 
          position.t <- position.t;
          position <- c(position, position.t)
        }
      }
    }
    else{
      if((misp.idx.t[1] < d5 && position.t == 1) || (misp.idx.t[2] > d6 && position.t == -1)){ 
        profit.t <- position.t*( vol.1*(trd.prc.1[length(trd.prc.1)]-p.10) + vol.2*(p.20-trd.prc.2[length(trd.prc.2)]) )
        profit <- c(profit, profit.t)
        position.t <- 0
        position <- c(position, position.t)
      }
      else { # do nothing
        position.t  <- position.t
        position <- c(position, position.t) 
      } 
    }
  }
} 

win <- sum(profit > 0)
lose <- sum(profit <= 0)
win.ratio <- win/(win+lose)
win.ratio

plot(cumsum(profit), type='l')
plot(profit, type='l')

hist(profit, 50)
mean(profit)

## 期货市场Copula高频套利策略实例(end)