install.packages("car")
install.packages("ridge")
install.packages("lars")
install.packages("https://cran.r-project.org/src/contrib/Archive/ridge/ridge_2.1-3.tar.gz", repos = NULL, type = "source")
library(car)
library(MASS)

# data <- data.frame(X1 = c(7, 1, 11, 11, 7, 11, 3, 1, 2, 21, 1, 11, 10),
#                    X2 = c(26, 29, 56, 31, 52, 55, 71, 31, 54, 47, 40, 66, 68),
#                    X3 = c(6, 15, 8, 8, 6, 9, 17, 22, 18, 4, 23, 9, 8),
#                    X4 = c(60, 52, 20, 47, 33, 22, 6, 44, 22, 26, 34, 12, 12),
#                    Y = c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2, 102.7, 72.5, 93.1, 115.9, 83.8, 113.3, 109.4)
# )
# lm.sol <- lm(Y ~ ., data=data)
# summary(lm.sol)
# 
# XX <- cor(data[1:4])
# kappa(XX)
# 
# vif(lm.sol)
# 
# ridge.sol <-lm.ridge(Y ~ ., lambda = seq(0,150,length=151), data=data, model = TRUE)
# names(ridge.sol)
# 
# ridge.sol$lambda(which.min(ridge.sol$GCV))
# ridge.sol$coef(which.min(ridge.sol$GCV))
# par(mfrow=c(1,2))
# matplot(ridge.sol$lambda, t(ridge.sol$coef), xlab = expression((lambda), ylab="Cofficients", type="l",lty=1:20))



cement<-data.frame(X1 = c( 7, 1, 11, 11, 7, 11, 3, 1, 2, 21, 1,11, 10),
                   X2 = c(26, 29, 56, 31, 52, 55, 71, 31, 54,47, 40, 66, 68),
                   X3 = c( 6, 15, 8, 8, 6, 9, 17, 22, 18, 4, 23,9, 8),
                   X4 = c(60, 52, 20, 47, 33, 22, 6, 44, 22, 26,34, 12, 12),
                   Y = c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2,102.7, 72.5,93.1,115.9, 83.8, 113.3, 109.4))
lm.sol<-lm(Y ~ X1+X2+X3+X4, data=cement)
summary(lm.sol)



# 书中是用逐步回归方法去除多余变量，这里我们用岭回归和Lasso做。
library(MASS)
#lm.ridge函数默认的lambda=0，等同于普通最小二乘回归。
lm.ridge(Y~., cement)

ridge=lm.ridge(Y~., cement, lambda=seq(0,0.5,0.01))
plot(ridge)


select(ridge)
lm.ridge(Y~., cement, lambda= 0.08499604)
lm.ridge(Y~., cement, lambda= 0.05830686)
lm.ridge(Y~., cement, lambda=0.32)


#用ridge包做岭回归比用MASS包更简便，结果也报告得更加详细
library(ridge)
lr=linearRidge(Y~., data=cement)
summary(lr)


#可以看出，X3的岭回归系数不显著，去掉X3再做一次

lr2=linearRidge(Y~.-X3, data=cement)
summary(lr2)


#X4也不太显著，去掉X4再试试

lr3=linearRidge(Y~.-X3-X4, data=cement)
summary(lr3)


#得到和书中变量选择一样的最简模型，回归系数也很接近。


#用LAR做变量选择
library(lars)
w=as.matrix(cement)
laa=lars(w[,1:4], w[,5])
laa

#由此可见，LASSO的变量选择是X4，X1，X2，由于X4与Y的相关系数是最大的，LASSO最先选择的就是X4，这一点与上面的结果不同。
plot(laa)
summary(laa)

#根据课上对Cp含义的解释（衡量多重共线性，其值越小越好），我们取到第3步，使得Cp值最小，也就是选择X4，X1，X2这三个变量。

