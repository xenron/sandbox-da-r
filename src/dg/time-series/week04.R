#读取数据
library("RODBC")
conn<-odbcConnectExcel2007("D:/data/Amtrak.xls")
Amtrak<-sqlFetch(conn,"Data")
close(conn)
library(xts)
Amtrak<-ts(Amtrak[,2],start=c(1991,1))

#计算自相关函数
# 观察季节性周期是否存在
acf(Amtrak)
pacf(Amtrak)

# 季节性趋势的数据，可以认为是非平稳的时间序列
# 进行季节性差分
#计算AR模型
# compute ACF
acf(Amtrak,lag=12)
# compute PACF
pacf(Amtrak,lag=12)
# 最大似然法，可能有收敛问题
# mm1=ar(Amtrak,method='mle')
# 使用最小二乘法进行估计
mm1=ar(Amtrak,method='ols')

# Find the identified order
mm1$order


#构建方程
#构建AR(22)模型
m1=arima(Amtrak,order=c(22,0,0))
m1
# 对比结果，aic值
# 可能出现拟合错误

#模型检验
# 检验残差是否白噪声
# 观察残差图，是否有季节性趋势
# Ljung-Box统计量，p值比较大，存在白噪声
# AR模型一般最多拟合3-5阶
tsdiag(m1,gof=24)
# 检验残差是否为白噪声
Box.test(m1$residuals,lag=24,type='Ljung')

# 预测后面一期的值
predict(m1,1)
# 预测后面二期的值
predict(m1,2)
library("forecast")
# 给出80%,95%的置信区间
forecast(m1,1)
forecast(m1,2)



#构建MA模型

#定阶
acf(Amtrak,lag=20)

#模型拟合
# unrestricted model
# 5阶
m1=arima(Amtrak,order=c(0,0,5))
m1
# 可以设置其中一部分不需要拟合（设为0）
m2=arima(Amtrak,order=c(0,0,5),fixed=c(NA,NA,NA,NA,0,NA,NA))
m2

#模型检验
# 检查残差是否白噪声
# 残差分布，小于零的居多
# ACF图，12阶有明显正相关性，18阶有负相关性
# Ljung-Box统计量，后面基本没有白噪声，说明存在相关性
# 可以把阶数适当提高
tsdiag(m1,gof=24)
# model checking
Box.test(m1$residuals,lag=24,type='Ljung')


#预测
predict(m1,10) # prediction 



#构建ARMA模型

#根据信息准则定阶
library("forecast")
# 根据极大似然法，给出阶数的估计
auto.arima(Amtrak)
#根据eacf定阶
library(TSA)
# 如果含有非平稳成分存在，则还不能使用eacf定阶
eacf(Amtrak)

#模型拟合
m2=arima(Amtrak,order=c(1,0,1))
m2

#模型检验
# 检查残差是否白噪声
tsdiag(m2,gof=24)
Box.test(m2$residuals,lag=24,type='Ljung')



#预测
predict(m2,1)


# 单位根检验
ar(Amtrak,method='ols')$order

library(fUnitRoots)
# p值非常大，接近1，非平稳时间序列
adfTest(Amtrak,lags=22,type="c")


# ARIMA模型
# 一次差分序列图
# 一阶差分运算
plot(diff(Amtrak))


# 一次差分的ACf
# 包含季节性存在
acf(as.vector(diff(Amtrak)),lag.max=36)


# 一次差分和季节差分的序列图
plot(diff(diff(Amtrak),lag=6)) 


# 一次差分与季节差分的ACF
# 1和12阶存在周期性，之后就没有相关性
# 之后再结合12阶为周期的季节性模型
acf(as.vector(diff(diff(Amtrak),lag=12)),lag.max=36)
# 存在较强的周期性
acf(as.vector(diff(diff(Amtrak),lag=6)),lag.max=36)
# 模型拟合
# AMIRA阶数，314，12个周期的1,1模型
m1=arima(Amtrak,order=c(3,1,4),seasonal=list(order=c(0,1,1),period=12))
m1

# 残差检验
# 残差分布比较均匀
# ACF没有超出置信区间
# LB，后面的部分基本上是白噪声
# 可以使用本模型进行预测
tsdiag(m1, gof.lag=36)

