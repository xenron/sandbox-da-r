###logistic回归
#读取数据
library("RODBC")
library(xts)
conn<-odbcConnectExcel2007("D:/tmp/MelbourneRainfall.xls")
data<-sqlFetch(conn,"data")
close(conn)
head(data)

#数据转化
data$y[data[,2]>0]<-1
data$y[data[,2]==0]<-0
data$y<-factor(data$y,levels=c(0,1),labels=c("No","Yes"))
table(data$y)
1572/nrow(data)
data<-data.frame(data[-1,],y.lag=data[-nrow(data),2])

#聚合数据图
data.xts<-xts(data[,2],order.by=data[,1])
data.month<-apply.monthly(data.xts,sum)
head(data.month)
plot(data.month)
apply.yearly(data.month,plot)

# 数据分割
(r<-which(data[,1]=="2010-01-01"))
train<-data[1:r,]
test<-data[(r+1):nrow(data),]

#模型构建
fit<-glm(y~y.lag+1,data=train,family=binomial())
summary(fit)


#预测
test$prob<-predict(fit,newdata=test,type="response")
test$pre[test$prob>0.5]<-1
test$pre[test$prob<=0.5]<-0
test$pre<-factor(test$pre,levels=c(0,1),labels=c("No","Yes"))
table(test$y,test$pre)
(379+47)/nrow(test)
47/(227+47)


###神经网络
library(AMORE)

#数据整理
conn<-odbcConnectExcel2007("D:/data/Amtrak.xls")
data<-sqlFetch(conn,"Data")
close(conn)
head(data)
data.xts<-xts(data$Ridership,order.by=data[,1])
nam<-c()
for(i in 1:12)
{
  data.xts<-cbind(data.xts,lag(data.xts[,1],i))
  nam[i]<-paste0("lag",i)
}
data.xts2<-na.omit(data.xts)
names(data.xts2)<-c("Ridership",nam)

#模型构建
p<-data.xts2[,-1]
target <- data.xts2[,1]

net <- newff(n.neurons=c(12,10,1), learning.rate.global=0.01, momentum.global=0.6,
             error.criterium="LMS", Stao=NA, hidden.layer="tansig",
             output.layer="purelin", method="ADAPTgdwm")
result <- train(net, p, target, error.criterium="LMS", report=TRUE, show.step=100, n.shows=5 )
y <- sim(result$net, p)
plot(data[-(1:12),1],y, col="blue", pch="+")
points(target, col="red", pch="x")


# 安装并导入neuralnet包（还需要安装grid和MASS两个依赖包）

library("neuralnet")

# 训练10个隐藏神经元的神经网络
net.sqrt <- neuralnet(Ridership~lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8+lag9+
                        lag10+lag11+lag12+1,data.xts2, hidden=10, threshold=0.01)
print(net.sqrt)

# 绘制神经网络拓扑图
plot(net.sqrt)

net.results <- compute(net.sqrt, data.xts2[,-1]) 

ls(net.results)

# 查看结果
print(net.results$net.result)


##分类神经网络
library(nnet) 
library(mlbench) 


#构建模型
model=nnet(y~y.lag,data=train,size=10,rang=0.1,decay=5e-4,maxit=200)

#给出训练集分类结果
table(train$y,predict(model,type="class"))
#给出测试集分类结果
table(test$y,predict(model,test,type="class"))
#构建隐藏层包含15个节点的网络。接着上面的语句输入如下程序：
model=nnet(y~y.lag,data=train,size=15,rang=0.1,decay=5e-4,maxit=200)

#给出训练集分类结果
table(train$y,predict(model,type="class"))
#给出测试集分类结果
table(test$y,predict(model,test,type="class"))