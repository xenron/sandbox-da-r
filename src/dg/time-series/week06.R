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

#加权平滑法
wma.cal <- function( ts, weight ){
  n <- length(ts)
  k <- length(weight)
  wma <- c()
  for( t in (k+1):n ) wma[t]= sum( ts[(t-k+1):t] * weight )  
  return(wma)
}

##模型拟合
n<-length(Amtrak)
tail(kma.cal(Amtrak,k=12),12)
Amtrak[(n-12):n]
tail(wma.cal(Amtrak,w=c(0.4,0.3,0.2,0.1)),12)
Amtrak[(n-12):n]
plot(Amtrak,type="l",col="Black")
lines(ts(kma.cal(Amtrak,k=12),start=c(1991,1)),col="blue")
lines(ts(wma.cal(Amtrak,w=c(0.4,0.3,0.2,0.1)),start=c(1991,1)),col="red")

#一次指数平滑法
ewma.cal <- function( ts, a=0.2 ){
  n <- length(ts)
  ewma <- c(ts[1]) 
  for( t in 2:n ) ewma[t]= a*ts[t-1] + (1-a)*ewma[t-1]
  return(ewma)
}



#模型拟合
tail(ewma.cal(Amtrak,a=0.2),12)
plot(Amtrak,type="l",col="Black")
lines(ts(ewma.cal(Amtrak,a=0.2),start=c(1991,1)),col="blue")

#线性二次移动平均
dkma.cal<-function(ts,k,m=1)
{
  st<-kma.cal(ts,k)
  stt<-kma.cal(st,k)
  at<-2*st-stt
  bt<-(2/(k-1))*(st-stt)
  n<-length(ts)
  f<-c(NA)
  for(i in 2:(n+m))
  {
    if(i<=n)
    {
      f[i]=at[i-1]+bt[i-1]
    } 
    else
    {
      f[i]=at[n]+(i-n)*bt[n]
    }
  }
  return(f)
}

#线性二次指数平滑
dema.cal<-function(ts,a=0.2,m=1)
{
  st<-ewma.cal(ts,a)
  stt<-ewma.cal(st,a)
  at<-2*st-stt
  bt<-(a/(1-a))*(st-stt)
  n<-length(ts)
  f<-c(NA)
  for(i in 2:(n+m))
  {
    if(i<=n)
    {
      f[i]=at[i-1]+bt[i-1]
    } 
    else
    {
      f[i]=at[n]+(i-n)*bt[n]
    }
  }
  return(f)
} 

#双重指数-加法模型
ema.add<-function(ts,a,b,m=1)
{
  n<-length(ts)
  l<-c(ts[1])
  t<-c(0)
  f<-c(NA)
  f[2]=l[1]+t[1]
  for(i in 2:n)
  {
    l[i]=a*ts[i]+(1-a)*(l[i-1]+t[i-1])
    t[i]=b*(l[i]-l[i-1])+(1-b)*t[i-1]
    f[i+1]=l[i]+t[i]
  }
  for(i in (n+1):(n+m))
  {
    f[i]=l[n]+i*t[n]
  }
  return(f)
}

#双重指数-乘法模型
ema.mul<-function(ts,a,b,m=1)
{
  n<-length(ts)
  l<-c(ts[1])
  t<-c(1)
  f<-c(NA)
  f[2]=l[1]*t[1]
  for(i in 2:n)
  {
    l[i]=a*ts[i]+(1-a)*(l[i-1]*t[i-1])
    t[i]=b*(l[i]/l[i-1])+(1-b)*t[i-1]
    f[i+1]=l[i]*t[i]
  }
  for(i in (n+1):(n+m))
  {
    f[i]=l[n]*(t[n])^i
  }
  return(f)
}

#霍特·温特指数平滑
ema.HT<-function(ts,a,b,c,M,k=1)
{
  n<-length(ts)
  l<-c(ts[1])
  t<-c(0)
  s<-rep(1,M)
  f<-c(NA)
  f[2]=(l[1]+t[1])*s[2]
  for(i in 2:n)
  {
    if(i>M)
    {
      l[i]=a*ts[i]/s[i-M]+(1-a)*(l[i-1]+t[i-1])
      t[i]=b*(l[i]-l[i-1])+(1-b)*t[i-1]
      s[i]=c*(ts[i]/l[i])+(1-c)*s[i-M]
      f[i+1]=(l[i]+t[i])*s[i]
    }
    else
    {
      l[i]=a*ts[i]/s[i]+(1-a)*(l[i-1]+t[i-1])
      t[i]=b*(l[i]-l[i-1])+(1-b)*t[i-1]
      f[i+1]=(l[i]+t[i])*s[i]
    }
    
  }
  for(i in (n+1):(n+k))
  {
    f[i]=(l[n]+i*t[n])*s[n-M+(i%%M)]
  }
  return(f)
}

##计算
dkma.cal(Amtrak,k=9,m=3)
dema.cal(Amtrak,a=0.2,m=3)
ema.add(Amtrak,a=0.2,b=0.2,m=3)
ema.mul(Amtrak,a=0.2,b=0.2,m=3)
ema.HT(Amtrak,a=0.2,b=0.2,c=0.2,M=12,k=3)
