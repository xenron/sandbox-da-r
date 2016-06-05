#1
1:20
20:1
c(1:20,19:1)
tmp<-c(4,6,3)
rep(tmp,10)
c(rep(tmp,10),4)
c(rep(4,10),rep(6,20),rep(3,30))

#2
x=seq(3,6,by=0.1)
y=exp(x)*cos(x)

#3
a=seq(3,36,by=3)
b=a-2
c=0.1^a*0.2^b

#4
paste("label",1:30)
paste0("fn",1:30)

#5
x <- c(
  "Swan swam over the pond, Swim swan swim!",
  "Swan swam back again - Well swum swan!"
)
# 使用一个可选的逗号，必须的空格，可以的连字符以及一个可选的空格分开
strsplit (x, ",? -? ?")
# 或者把最后的连字符的空格括起来
strsplit(x, ",? (- )?")

#6
set.seed(50)
xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace=T)
#(a)
yVec[-1]-xVec[-250]
#(b)
sin(yVec[-250])/cos(xVex[-1])
#(c)
x1<-xVec[-(249,250)]
x2<-xVec[-c(1,250)]
x3<-xVec[-(1:2)]
x1+2*x2-x3
#(d)
x1<-xVec[-1]
x2<-xVec[-250]
sum(exp(-x1)/(x2+10))

#7
#(a)
yVec[which(yVec>600)]
#(b)
which(yVec>600)
#(c)
sum(xVec[xVec>600&yVec>600]*yVec[xVec>600&yVec>600])
#(d)
xbar<-mean(xVec)
z<-sqrt(abs(x-xbar))
#(e)
length(yVec[yVec<=200])
#(f)
length(xVec[xVec%%2==0])
#(g)
xVec[order(yVec,decreasing=T)]
#(h)
yVec[seq(1,250,by=3)]



