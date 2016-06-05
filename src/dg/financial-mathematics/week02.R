#####单利的计算#####
SInt<-function(vt=NULL,p=NULL,t=NULL,r=NULL)
{
  if(is.null(vt))
 {vt=(1+r*t)*p
 return(vt)}
 if(is.null(p))
{ p=vt/(1+r*t)
  return(p)}
if(is.null(r))
{ r=(vt/p-1)/t
  return(r)}
if(is.null(t))
{ t=(vt/p-1)/r
  return(t)}
}
  
#1
SInt(p=150,t=20/365,r=0.08)
#2
SInt(p=9000,t=61/365,vt=9020)
Sret(v0=9000,vt=9020)
#3
SInt(r=0.02,vt=1000,t=1)
#4
SInt(p=800,vt=830,r=0.09)
Sret(v0=800,vt=830)



#####复利的计算######
CInt<-function(vt=NULL,p=NULL,t=NULL,r=NULL,m)
{
  if(is.null(vt))
  {vt=((1+r/m)^(t*m))*p
   return(vt)}
  if(is.null(p))
  { p=vt/((1+r/m)^(t*m))
    return(p)}
  if(is.null(r))
  { r=m*(((vt/p)^(1/(t*m)))-1)
    return(r)}
  if(is.null(t))
  { t=log(vt/p)/(m*log(1+r/m))
    return(t)}
}
#1
CInt(r=0.06,m=365,p=1,vt=2)
#2
CInt(t=10,m=1,p=1,vt=2)
#3
CInt(r=0.1,t=2,m=1,p=100)
CInt(r=0.1,t=2,m=2,p=100)

#####年金#####
PA<-function(r,n)
{
  PA=0
  if(is.numeric(n))
  {
    for(i in 1:n)
      PA<-PA+1/(1+r)^i
  }
  if(n=="inf")
    PA<-1/r
  return(PA)
}

annuity<-function(p=NULL,c=NULL,r,m=1,t,compute.vt=FALSE)
{
  n=t*m
  if(is.null(p))
  {
    p<-c*PA(r/m,n)
    re=p
  }
  if(is.null(c))
  {
    c<-p/PA(r/m,n)
    re=c
  }
  if(compute.vt)
  {
    vt=p*(1+r/m)^(t*m)
    re=c(re,vt)
  }
  return(re)
}

#1
annuity(r=0.09,c=100,t=5)
#2
annuity(r=0.08,m=4,t=10,c=100,compute.vt=T)
#3
annuity(r=0.06,t=5,m=12,p=20000)

#4
p=annuity(c=700,r=0.05,m=1,t=15)
p=p/(1+0.05)
annuity(p=p,r=0.05,t=10)

######连续复合#####
CCInt<-function(p=NULL,vt=NULL,t=NULL,r=NULL)
{
  if(is.null(vt))
  {vt=p*exp(-(t*r))
   return(vt)}
  if(is.null(p))
  { p=vt/exp(-(t*r))
    return(p)}
  if(is.null(r))
  { r=-log(p/vt)/t
    return(r)}
  if(is.null(t))
  { t=-log(p/vt)/r
    return(t)}
}

CCInt(r=0.06,vt=1000001,p=1000000)


#####零息债券#####
r=CInt(t=1/2,p=95,vt=100,m=1)
0.5-CInt(p=99,vt=100,r=r,m=1)

######付息债券#####
a=annuity(r=0.12,c=10,t=5)
b=CInt(t=5,r=0.12,vt=100,m=1)
a+b