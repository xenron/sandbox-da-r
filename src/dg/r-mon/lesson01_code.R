#####---lesson 1---#####

###-how to use help

?mean                    # 打开mean 函数的帮助页面
?"+"                     # 打开加法操作的帮助页面
?"if"                    # 打开if 的帮助页面，用于分支代码
??plotting               # 搜索所有包含"plotting" 的主题
??"regression model"     # 搜索所有与regression model 相关的主题

help("mean")
help("+")
help("if")
help.search("plotting")
help.search("regression model")

a_vector<-c(1,2,3)
apropos("vector")
apropos("z$")
apropos("[4-9]")

example(plot)
demo() # 列出所有演示
demo(Japanese)

browseVignettes()
vignette("Sweave", package="utils")
RSiteSearch("{Bayesian regression}")

###-basic mathematical operation
exp(1)
log(1)
log10(1e3)
sqrt(2^2)
abs(-3)
sin(pi)
cos(pi)
min(1:4)
max(1:4)
which.min(c(1,4,2,3))
round(3.5)
floor(3.9)
ceiling(3.1)
sum(1:4)
#.......#
cos(c(0, pi / 4, pi / 2, pi))           #pi 是内置常数
exp(pi * 1i) + 1                        # 欧拉公式
factorial(5)                            #阶乘
factorial(7) + factorial(1) - 71 ^ 2    #5041 是一个大数字
choose(5, 0:5)                          #二项式里的c(n,k)

#emp.1 prod()连乘函数
exactlyone<-function(p){
  notp<-1-p
  tot<-0.0
  for(i in 1:length(p))
    tot<-tot +p[i]*prod(notp[-i])
  return(tot)
}

#emp2. cumsum() 累积和and sumprod()累计乘积
x<-c(12,5,13)
cumsum(x)
cumprod(x)

#emp.3 最小值最大值
z<-matrix(c(1,2,5,3,6,2),nrow=3,byrow=T)
min(z[,1],z[,2])
pmin(z[,1],z[,2])
pmin(z[1,],z[2,],z[3,])
nlm(function(x) return(x^2-sin(x)),8)
optim()

#emp.4 微积分
D(expression(exp(x^2)),"x")
integrate(function(x) x^2, 0, 1)

###-vector and matrix
y<-c(1,2,3,4)
2*y
crossprod(1:3,c(5,12,13))

a<-matrix(c(1,2,3,4),nrow=2, byrow=T)
b<-matrix(c(1,-1,0,1),nrow=2, byrow=T)
a%*%b

a<-matrix(c(1,1,-1,1),nrow=2, byrow=T)
b<-c(2,4)
solve(a,b)

a
da<-diag(a)
diag(da)
diag(3)

m<-matrix(1:9,byrow=T, nrow=3)
m
sweep(m,1,c(1,4,7),"+")



###-variable assignment and logical vector
x <- 1:5
y = 6:10
x+2*y-3
?make.names

x<-3
x< -3
x <- 3

x <<- exp(exp(1))

assign("my_local_variable", 9 ^ 3 + 10 ^ 3)
assign("my_global_variable", 1 ^ 3 + 12 ^ 3, globalenv())

x
z <- rnorm(5);z
(zz <- rlnorm(5))

(x <- 1:10 >= 5)
!x
(y <- 1:10 %% 2 == 0)
x & y
x | y

#真值表
x <- c(TRUE, FALSE, NA) 　　　   # 三个逻辑值
xy <- expand.grid(x = x, y = x)  # 取得x 和y 的所有组合
within( # 在xy 内赋值
  xy,
{
  and <- x & y
  or <- x | y
  not.y <- !y
  not.x <- !x
}
)

none_true <- c(FALSE, FALSE, FALSE)
some_true <- c(FALSE, TRUE, FALSE)
all_true <- c(TRUE, TRUE, TRUE)
any(none_true)
any(some_true)
any(all_true)
all(none_true)
all(some_true)
all(all_true)

###-statistical distribution function
mean(rchisq(1000,df=2))
qchisq(c(0.5,0.95),df=2)
dchisq(1,df=2)
pchisq(1,df=2)

###-sorting
x <- c(13,5,12,89);x
x<-sort(x,decreasing=F)
x
order(x)
(x<-x[order(x)])

#order()应用在数据框中排序
name<-c("aaa","bbb","ccc")
data<-c(2,6,4)
y<-data.frame(name, data);y
r<-order(y$data);r
z<-y[r,];z
y[order(y$name),]
y[order(y$data),]

x<-c(12,5,67,2,4,4)
rank(x)

###-sets
x<-c(12,5,67,2,4,4)
y<-c(1,2,5,95)
union(x,y)
intersect(x,y)
setdiff(x,y)
setdiff(y,x)
setequal(x,y)
setequal(y,c(1,5,7))
2 %in% x
choose(5,3)

#emp.1对称差
symdiff<-function(x,y){
  sdfxy<-setdiff(x,y)
  sdfyx<-setdiff(y,x)
  return(union(sdfxy,sdfyx))
}
x
y
symdiff(x,y)

#emp.2判断集合u是否为v子集
"%subsetof%"<-function(u,v){
  return(setequal(intersect(u,v),u))
}
c(3,5) %subsetof% 1:10
c(2,8) %subsetof% 1:5

###-simulation using R
#MC1.R
MC1<-function(n){
  k<-0; x<-runif(n); y<-runif(n)
  for(i in 1:n){
    if (x[i]^2+y[i]^2 < 1)
      k<-k+1
  }
  4*k/n
}
MC1(1000000)

#MC2.R
integrate(function(x) sqrt(1-x^2),0,1)
MC2<-function(n){
  k<-0; x<-runif(n); y<-runif(n)
  for(i in 1:n){
    if (y[i]<=sqrt(1-x[i]^2))
      k<-k+1
  }
  k/n
}
MC2(100000)
