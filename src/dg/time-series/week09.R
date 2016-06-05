mae<-function(y,pre)
{
  e=y-pre
  mae<-mean(abs(e))
  return(mae)
}

ae<-function(y,pre)
{
  return(mean(y-pre))
}

mape<-function(y,pre)
{
  e=y-pre
  mape=mean(abs(e/y))
  return(mape)
}

rmse<-function(y,pre)
{
  e=y-pre
  rmse<-sqrt(mean(e^2))
  return(rmse)
}

MaxAE<-function(y,pre)
{
  return(max(abs(y-pre)))
}

MaxAPE<-function(y,pre)
{
  e=y-pre
  return(max(abs(e/y)))
}

##读取数据
library("RODBC")
conn<-odbcConnectExcel2007("D:/data/Amtrak.xls")
Amtrak<-sqlFetch(conn,"Data")
close(conn)

Amtrak<-ts(Amtrak[,2],start=c(1991,01))


#一次移动平滑法
kma.cal <- function( ts, k=20 ) { 
  n <- length(ts)
  kwma <- c()
  for( t in (k+1): n ) kwma[t]= mean(ts[(t-k+1):t])
  return(kwma)
}
#一次指数平滑法
ewma.cal <- function( ts, a=0.2 ){
  n <- length(ts)
  ewma <- c(ts[1]) 
  for( t in 2:n ) ewma[t]= a*ts[t-1] + (1-a)*ewma[t-1]
  return(ewma)
}


pre<-kma.cal(Amtrak,k=9)
mae(Amtrak[-(1:9)],pre[-(1:9)])
ae(Amtrak[-(1:9)],pre[-(1:9)])
mape(Amtrak[-(1:9)],pre[-(1:9)])
rmse(Amtrak[-(1:9)],pre[-(1:9)])
MaxAE(Amtrak[-(1:9)],pre[-(1:9)])
MaxAPE(Amtrak[-(1:9)],pre[-(1:9)])

pre2<-ewma.cal(Amtrak,a=0.2)
mae(Amtrak,pre2)
ae(Amtrak,pre2)
mape(Amtrak,pre2)
rmse(Amtrak,pre2)
MaxAE(Amtrak,pre2)
MaxAPE(Amtrak,pre2)



