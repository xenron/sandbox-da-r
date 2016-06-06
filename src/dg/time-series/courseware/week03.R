###回归分析-身高与体重
h=c(171,175,159,155,152,158,154,164,168,166,159,164)
w=c(57,64,41,38,35,44,41,51,57,49,47,46)
lm.hw<-lm(h~w)
summary(lm.hw)
plot(h~w)
abline(lm.hw)


###回归分析—swiss数据
head(swiss)
# Fertility作为因变量，其余变量作为自变量
lm.swiss<-lm(Fertility~.,data=swiss)
summary(lm.swiss)


###回归分析—婚外情数据
library(AER)
data(Affairs)
summary(Affairs)
table(Affairs$affairs)

# 数据转化
Affairs$ynaffair[Affairs$affairs>0]<-1
Affairs$ynaffair[Affairs$affairs==0]<-0
# 因子
Affairs$ynaffair<-factor(Affairs$ynaffair,levels=c(0,1),labels=c("No","Yes"))
table(Affairs$ynaffair)

# 模型构建
fit.full<-glm(ynaffair~.,data=Affairs[,-1],family=binomial())
summary(fit.full)


# 模型优化
fit.reduced<- glm(ynaffair~age+yearsmarried+religiousness+rating,data=Affairs,family=binomial())
summary(fit.reduced)

# 模型比较
anova(fit.reduced,fit.full,test="Chisq")

# 模型参数解释
coef(fit.reduced)
exp(coef(fit.reduced))


#预测
testdata<- data.frame(rating=1:5,age=mean(Affairs$age),yearsmarried=mean(Affairs$yearsmarried), religiousness=mean(Affairs$religiousness))
testdata
testdata$prob<-predict(fit.reduced,newdata=testdata,type="response")
testdata

testdata<-data.frame(rating=mean(Affairs$rating),age=seq(17,57,10), yearsmarried=mean(Affairs$yearsmarried), religiousness=mean(Affairs$religiousness))
testdata
testdata$prob<-predict(fit.reduced,newdata=testdata,type="response")
testdata



###时间序列的线性趋势
# 读取数据
library("RODBC")
conn<-odbcConnectExcel2007("D:/data/Amtrak.xls")
Amtrak<-sqlFetch(conn,"Data")
close(conn)
head(Amtrak)

Am<-Amtrak[,c(1,2)]
lm.am<-lm(Ridership~Month,data=Am)
plot(Am)
abline(lm.am)

# 时间转换为顺序化的时间点
t<-(1:nrow(Am))
lm.am<-lm(Am$Ridership~t)
summary(lm.am)
# 生成四张图形
# Residuals vs Fitted，残差和拟合值，如果拟合好的话，残差应该是均匀分布
# Normal Q-Q，残差QQ图，验证残差是否符合正态分布
# Scale-Location，标准化残差图
# Residuals vs Leverage，离群值
plot(lm.am)

###时间序列的非线性趋势——指数趋势
y<-log(Am$Ridership)
lm.am2<-lm(y~t)
summary(lm.am2)
plot(lm.am2)

###时间序列的非线性趋势——多项式趋势
lm.am3=lm(Am$Ridership~t+I(t^2))
summary(lm.am3)
plot(lm.am3)


###随性序列，随机游走
library("TSA")
data(rwalk)
t=time(rwalk)
model1=lm(rwalk~t)
summary(model1)

plot(rwalk,type='o',ylab='y')
abline(model1) 

###季节性趋势
# 季节均值法
# season(tempdub) creates a vector of the month index of the data as a factor 
# season()这个函数创建一个月份向量的数据作为一个因子
data(tempdub)
# the period sign is included to make the printout from
# 月份标识（哑变量）
month=season(tempdub)
# the commands two line below clearer; ditto below.
# -1 removes the intercept term
model2=lm(tempdub~month-1)
summary(model2)

# 有截距的情形，自动将1月的数据丢掉
# 在1月份气温的基础上，加上截距
# intercept is automatically included so one month (Jan) is dropped
model3=lm(tempdub~month)
summary(model3)

# 余弦趋势法
har.=harmonic(tempdub,1)
model4=lm(tempdub~har.)
summary(model4)
plot(ts(fitted(model4),freq=12,start=c(1964,1)),ylab='Temperature',type='l', ylim=range(c(fitted(model4),tempdub)))
# 参数ylim的设定确保了图象y轴的范围
points(tempdub)