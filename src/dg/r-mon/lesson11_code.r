###1
x<-seq(0,2*pi,length=100)
sinx<-sin(x)
plot(x,sinx,type="l")

###2
f<-function(x){
  substitute(x)
}
f(1:10)
f(x)
f(x+y^2/z+exp(a*sin(b)))

###3
g<-function(x) deparse(substitute(x))
g(1:10)
g(x)
g(x+y^2/z+ exp(a*sin(b)))
###4
g<-function(x) deparse(f(x))
g(1:10)
g(x)
g(x+y^2/z+exp(a*sin(b)))

###5
x <- 1:4 
y <- letters[1:4] 
names(data.frame(x, y)) 


###6
sample_df <- data.frame(a = 1:5, b = 5:1, c = c(5, 3, 1, 4, 1)) 
subset(sample_df, a >= 4) ##相当于sample_df[sample_df$a>=4,]
subset(sample_df, b == c) ##相当于sample_df[sample_df$b==sample_df$c,]


###7
quote(1:10)
quote(x)
quote(x+y^2)

substitute(y==sqrt(a,b),list(a=3,b=2))

###8
x<-10 
eval(quote(x))  ##也可以通过eval(quote(x<-10))的方式进行赋值
e <- new.env()  ##提供一个新环境e
e$x<-20    
eval(quote(x^2),e) 
eval(x^2,e) ##与不使用quote函数时的对比；如果我们不用quote函数那么这个参数将直接在当前环境下对表达式进行计算


###9
eval(quote(x), list(x = 30)) 
eval(quote(x), data.frame(x = 40)) 

###10
eval(quote(a >= 4), sample_df) 
eval(quote(b == c), sample_df) 


###11
a <- 10 
eval(quote(a), sample_df) ##参数a在sample_df中运行 
eval(a, sample_df) ##数值10在sample_df中运行
eval(quote(b), sample_df) ##参数b在sample_df中运行 
eval(b, sample_df) ##全局环境中并未定义b的数值


###12
subset2 <- function(x, condition) { 
  condition_call <- substitute(condition) 
  r <- eval(condition_call, x) 
  x[r,] 
} 
subset2(sample_df, a >= 4) 


###13
y <- 4 
x <- 4 
condition <- 4 
condition_call <- 4 
subset2(sample_df,a==4) 
subset2(sample_df,a==y)
subset2(sample_df,a==x) 
subset2(sample_df,a==condition) 
subset2(sample_df,a==condition_call) 


###14
subset2 <- function(x, condition) { 
  condition_call <- substitute(condition) 
  r <- eval(condition_call, x, parent.frame()) 
  x[r, ] 
} 
x <- 4 
subset2(sample_df, a == x) 

###15
subset2a <- function(x, condition) { 
  condition_call <- substitute(condition) 
  env <- list2env(x, parent = parent.frame()) 
  r <- eval(condition_call, env) 
  x[r, ] 
} 
x <- 5 
subset2a(sample_df, a == x) 


###16
subset2 <- function(x, condition) { 
  condition_call <- substitute(condition) 
  r <- eval(condition_call, x, parent.frame()) 
  x[r, ] 
} 
scramble <- function(x) x[sample(nrow(x)), ] 
subscramble <- function(x, condition) { 
  scramble(subset2(x, condition)) 
} 


###17
subscramble(sample_df, a >= 4) 
traceback() 


###18
debugonce(subset2) 
subscramble(sample_df, a >= 4) 


###19
a <- 4 
subscramble(sample_df, a == 4) 
a <- c(1, 1, 4, 4, 4, 4) 
subscramble(sample_df, a >= 4) 


###20
subset2_q <- function(x, condition) { 
  r <- eval(condition, x, parent.frame()) 
  x[r, ] 
} 


###21
subset2 <- function(x, condition) { 
  subset2_q(x, substitute(condition)) 
} 
subscramble <- function(x, condition) { 
  condition <- substitute(condition) 
  scramble(subset2_q(x, condition)) 
} 
subscramble(sample_df, a >= 3) 
subscramble(sample_df, a >= 3) 


###22
a <- 1 
b <- 2 
if ((b <- a + 1) > (a <- b - 1)) { 
  b <- b + 2 
} 


###23
install.packages("ggplot2")  ##如果已有可以省略这步
ggplot2 <- "plyr" 
library(ggplot2) 


###24
x <- 10 
y <- "a" 
df <- data.frame(x, y) 
names(df) 


####表达式####
install.packages("pryr")
library(pryr)
###25
x <- 4 
y <- x*10 
y 
###26
z <- quote(y <- x*10) 
z 


###28
ast(y <- x*10) 


###29
ast("a") 
ast(1) 
ast(1L) 
ast(TRUE) 
###30
identical(1, quote(1)) 
identical("test", quote("test")) 
###31
ast(x) 
ast(mean) 
ast(`an unusual name`) 


###32
ast(f())
ast(f(1,2))
ast(f(a,b))
ast(f(g(),h(1,a)))

ast(a + b)
ast(if (x > 1) x else 1/x)

#33
ast(function(x = 1, y) x)
ast(function(x = 1, y = x * 2) {x / y})

str(quote(a))
str(quote(a + b))

