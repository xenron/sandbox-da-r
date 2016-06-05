#1
atan( 1 / 1:1000 )

#2
x <-  1:1000
y <- atan(1/x)
z <- 1 /tan(y)

#3
sum(factorial(1:20))

#4
encryption<-function(a)
{
  b=c()
  c=c()
  b[1]=a%/%1000
  b[2]=a%/%100%%10
  b[3]=a%/%10%%10
  b[4]=a%%10
  #写成循环
  #for(i in 1:4)
  #{
  #  b[i]=a%/%(10^(4-i))%%10
  #}
  #使用字符串处理方法
  #b=as.numeric(substring(a, c(1,2,3,4),c(1,2,3,4)))
  b=(b+5)%%10
  c[1]=b[4]
  c[2]=b[3]
  c[3]=b[2]
  c[4]=b[1]
  #c=b[4:1]
  return(c[1]*1000+c[2]*100+c[3]*10+c[4])
}
decryption<-function(a)
{
  b=c()
  c=c()
  b[1]=a%/%1000
  b[2]=a%/%100%%10
  b[3]=a%/%10%%10
  b[4]=a%%10
  
  c[1]=b[4]
  c[2]=b[3]
  c[3]=b[2]
  c[4]=b[1]
  c=(c+5)%%10
  return(c[1]*1000+c[2]*100+c[3]*10+c[4])
}

encryption(1234)
decryption(9876)

#5
helen<-function(a,b,c)
{
  if((a+b<=c)|(b+c<=a)|(a+c<=b))
  {
    return("输入的边长数据不能构成三角形")
  }
  else
  {
    s=(a+b+c)/2
    area=sqrt(s*(s-a)*(s-b)*(s-c))
    return(area)
  }
}
helen(1,2,3)
helen(5,12,13)

#6
ss<-function(x)
{
  a=nchar(x)
  b=c()
  for(i in 1:a)
  {
    b[i]=x%/%(10^(a-i))%%10
  }
  return(sum(b))
}

#7
for(i in 0:100)
{
  if((i%%2)|(i%%3)|(i%%7))
    print(i)
}
#c(0:100)[(i%%2)|(i%%3)|(i%%7)]

#8
f<-function(x)
{
  if(x<=0)
  {
    return(-(x^3))
  }
  else if (x<=1&x>0)
  {
    return(x^2)
  }
  else
  {
    return(sqrt(x))
  }
}
x=seq(from=-2,to=2,by=0.1)
f(x)
y=sapply(x,f)
plot(x,y)
#for(i in 1:length(x))
#{
#   f(x[i])
#}

#9
h<-function(x,n)
{
  sum=0
  for(i in 0:n)
  {
    sum<-sum+x^i
  }
  return(sum)
}
h(2,3)


#10
a=choose(20,3)*choose(17,4)*choose(13,5)
b=choose(18,1)*choose(17,4)*choose(13,5)
c=choose(18,3)*choose(15,2)*choose(13,5)
d=choose(18,3)*choose(15,4)*choose(11,3)
(b+c+d)/a

#简化思路
choose(18,1)/choose(20,3)+choose(18,2)/choose(20,4)+choose(18,3)/choose(20,5)
sum(choose(18,1:3)/choose(20,3:5))

#11
#(1)
sum=0
for(i in 10:100)
{
  sum=sum+i^3+4*i^2
}
sum

sum(sapply(10:100,function(x) x^3+4*x^2))

#(2)
sum=0
for(i in 1:25)
{
  sum=sum+(2^i)/i+(3^i)/(i^2)
}
sum

sum(sapply(1:25,function(i) (2^i)/i+(3^i)/(i^2)))

#12
MC<-function(n){
  k<-0
  x<-runif(n)
  y<-runif(n,0,sqrt(2))
  for(i in 1:n){
    if (sqrt(1+x[i]^2)>y[i])
      k<-k+1
  }
  k/n*sqrt(2)
}
MC(1000000)
integrate(function(x) sqrt(1+x^2),0,1)