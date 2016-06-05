
###############################################################################################
##  (1) f(x)求一阶导数f’(x) 
fun<-expression(6*log(x)/(2+x))
dx1<-D(fun,"x")
class(dx1)
dx1<-function(x) 6 * (1/x)/(2 + x) - 6 * log(x)/(2 + x)^2
dx1(2)

dx2<-deriv(y~6*log(x)/(2+x),"x",func=T)
class(dx2)
dx2(2)

dx3<-function(x) (6+12/x-6*log(x))/(2+x)^2
dx3(2)



###############################################################################################
##  (2) 采用二分法求f’(x) = 0的x，精度要求e=10-6，计算需要迭代多少次。

# 方法1
bisection<-function(fun,a,b,tol){
  i<-0
  if(fun(a)*fun(b)>0){
    list(fail="finding root is fail")
  }else{
    repeat{
      if(abs(a-b)<tol) break
      else{
        mid<-(a+b)/2
        if(fun(a)*fun(mid)<0) b<-mid
        else a<-mid
        i<-i+1
      }
    }
  }
  if(i==0) print("finding root is fail")
  list(root=(a+b)/2,iterations=i)
}

fun<-function(x) (6+12/x-6*log(x))/(2+x)^2
bisection(fun,a=1,b=5,tol = 1e-6)


# 方法2：递归调用
i<-0
bisection2<-function(fun,a,b,tol){
  if(fun(a)*fun(b)>0){
    list(fail="finding root is fail")
  }
  else{
    if(abs(a-b)<tol){
      return(list(root=(a+b)/2,iterations=i))
    }
    else{
      mid<-(a+b)/2
      if(fun(a)*fun(mid)<0) b<-mid
      else a<-mid
      i<<-i+1
      bisection2(fun,a,b,tol)
    }
  }
}

fun<-function(x) (6+12/x-6*log(x))/(2+x)^2
bisection2(fun,a=1,b=5,tol = 1e-6)

# 方法3：animation包
library(animation)
fun<-function(x) 6 * (1/x)/(2 + x) - 6 * log(x)/(2 + x)^2
root<-bisection.method(fun,c(1,5),tol=1e-6)


###############################################################################################
##  (3) f(x)求二阶导数f’’(x)

# 方法1
fun<-expression(6*log(x)/(2+x))
dx<-D(fun,"x")
ddx<-D(dx,"x")
ddx1<-function(x){
  -(6 * (1/x^2)/(2 + x) + 6 * (1/x)/(2 + x)^2 + (6 * (1/x)/(2 + 
   x)^2 - 6 * log(x) * (2 * (2 + x))/((2 + x)^2)^2))
}

ddx1(5)

# 方法2
ddx2<-deriv3(y~6*log(x)/(2+x),"x",func=T)
ddx2(5)


###############################################################################################
## 案例2：编写一个求解f(x)多阶导数的函数DD。

DD<-function(expr,name,order){
  if(order<1) stop("'order' must be >=1")
  if(order==1) D(expr,name)
  else DD(D(expr,name),name,order-1)
}   

expr<-expression(6*log(x)/(2+x))
DDX<-DD(expr=expr,name="x",order=4)
x<-5
eval(DDX)

DD<-function(expr,name,order,value){
  if(order<1) stop("'order' must be >=1")
  if(order==1){
    DDX<-D(expr,name)
    x<-value
    return(eval(DDX))
  }else{
    DDX<- DD(D(expr,name),name,order-1,value)
    x<-value
    return(eval(DDX))
  }
}

expr<-expression(6*log(x)/(2+x))
DDX<-DD(expr=expr,name="x",order=4,value=5)














































