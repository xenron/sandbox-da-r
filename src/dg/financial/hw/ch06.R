# 第六周 量化投资书面作业
# （1）使用财务指标选股方法，选取CSRC互联网和相关服务行业中EPS、ROE、NPM均排在行业前5名的股票，并做表现实测
# （2）试运行课程代码中CAPM模型Alpha的批量化计算，并找出超额收益率最高的5家公司
# （3）试运行课程代码中三因子模型Alpha的批量化计算，并找出超额收益率最高的5家公司

# 解答：（1）使用财务指标选股方法，选取CSRC互联网和相关服务行业中EPS、ROE、NPM均排在行业前5 名（因为行业中共有13名企业）的股票，并做表现实测
# install.packages("xts")
# install.packages("xtsExtra")
# install.packages("xts", repos="http://R-Forge.R-project.org")
# install.packages("xtsExtra", repos="http://R-Forge.R-project.org")
# install.packages("quantmod")
# install.packages("RODBC")

library(xts)
library(xtsExtra)
library(quantmod)
library(RODBC)
# 读入数据
fin <- read.csv("D:/tmp/Finance Report 2012.csv",header=TRUE)
sort(table(fin$Nnindcd))
fin.net <- fin[fin$Nnindcd == " CSRC互联网和相关服务",]
stk.net.list <- fin.net[, "Stkcd"]
stk.net.num <- length(stk.net.list)
stk.net.num
#排序
rank.net <- apply(fin.net, MARGIN=2, FUN=function(var) rev(rank(var)))
rank.net<- data.frame(rank.net)
#筛选股票
k=5
win.select <- ifelse(rank.net$EPS <= k & rank.net$ROE <= k & rank.net$NPM <= k, 1, 0)
win.select
fin.win.net <- fin.net[ win.select == 1,]
fin.lose.net<- fin.net[ win.select == 0,]
win.list <- fin.win.net$Stkcd
# win.list <- fin.win.net$Stkcd
lose.list <- fin.lose.net$Stkcd
win.nme <- fin.win.net$Stknme
lose.nme <- fin.lose.net$Stknme
# 输出结果
win.nme
lose.nme
# 股票选取(end)

# # 股票选取(begin)
# fin <- read.csv("D:/tmp/Finance Report 2012.csv", header=TRUE)
# sort(table(fin$Nnindcd))
# fin.net <- fin[fin$Nnindcd == "CSRC医药制造业",]
# stk.net.list <- fin.net[,"Stkcd"]
# stk.net.num <- length(stk.net.list)
# stk.net.num
# rank.net <- apply(fin.net, MARGIN=2,FUN=function(var) rev(rank(var)))
# rank.net <- data.frame(rank.net)
# k <- 15
# win.select <- ifelse(rank.net$EPS <=k & rank.net$ROE <= k & rank.net$NPM <= k, 1, 0)
# win.select
# fin.win.net <- fin.net[ win.select == 1,]
# fin.lose.net <- fin.net[ win.select ==0, ]
# win.list <- fin.win.net$Stkcd
# lose.list <- fin.lose.net$Stkcd
# win.nme <- fin.win.net$Stknme
# lose.nme <- fin.lose.net$Stknme
# win.nme
# lose.nme
# # 股票选取(end)

# 表现实测(begin)
idx.data <- read.table("D:/tmp/TRD_Index.txt",header=TRUE)
idx.HS300 <- idx.data[idx.data$Indexcd == 902 & as.Date(idx.data$Trddt) >= as.Date("2013-01-01"), c("Trddt","Clsindex")]
idx.HS300 <- xts(idx.HS300$Clsindex, order.by=as.Date(idx.HS300$Trddt))
names(idx.HS300) <- "HS300"
win.prc.car <- xts( order.by=index(idx.HS300) )
lose.prc.car <- xts( order.by=index(idx.HS300) )

conn <- odbcConnectAccess2007(access.file="D:/tmp/Stock.accdb",uid="test", pwd="test")
for( stk.cd.i in win.list ){
  prc.query <- paste("SELECT Trddt, Adjprcwd FROM Stock WHERE Trddt >= #1/1/2013# AND Stkcd = ", stk.cd.i)
  stk.prc.i <- sqlQuery(conn, prc.query)
  stk.prc.xts.i <- xts(stk.prc.i$Adjprcwd, order.by=as.Date(stk.prc.i$Trddt))
  win.prc.car <- merge(win.prc.car, stk.prc.xts.i, all=TRUE)
}
for( stk.cd.i in lose.list ){
  prc.query <- paste("SELECT Trddt, Adjprcwd FROM Stock WHERE Trddt >= #1/1/2013# AND Stkcd = ", stk.cd.i)
  stk.prc.i <- sqlQuery(conn, prc.query)
  stk.prc.xts.i <- xts(stk.prc.i$Adjprcwd, order.by=as.Date(stk.prc.i$Trddt))
  lose.prc.car <- merge(lose.prc.car, stk.prc.xts.i, all=TRUE)
}
close(conn)

head(lose.prc.car)
head(win.prc.car)

# plot(win.prc.car, screens=1)
# plot(lose.prc.car, screens=1)
# plot(na.approx(lose.prc.car), screens=1)

win.mean <- xts(rowMeans(na.locf(win.prc.car[, -3])), order.by=index(win.prc.car) )
lose.mean <- xts(rowMeans(na.locf(lose.prc.car[, -18])), order.by=index(lose.prc.car) )

rep(coredata(win.mean[1]), length(win.mean))
coredata(lose.mean[1])
win.ret <- win.mean / rep(coredata(win.mean[1]), length(win.mean))
lose.ret <- lose.mean / rep(coredata(lose.mean[1]), length(win.mean))
# plot(win.mean)
# plot(lose.mean)
# plot(merge(win.ret, lose.ret)[1:30], screens=1, col=c('darkred', 'darkgreen'), main="Cumulative Return of Win and Lose Groups")
# 表现实测(end)

# （2）试运行课程代码中CAPM模型Alpha的批量化计算，并找出超额收益率最高的5家公司
source("D:/tmp/CAPM source.R")
head(alpha.CAPM.df,3)
alpha.CAPM.df[is.na(alpha.CAPM.df)==1]<-0
alpha.CAPM.df$sum<-apply(alpha.CAPM.df[-1],1,sum)
top5<- alpha.CAPM.df[order(alpha.CAPM.df$sum,decreasing=T)[1:5],c(1,62)]
top5

# （3）试运行课程代码中三因子模型Alpha的批量化计算，并找出超额收益率最高的5家公司
source("D:/tmp/ThrFactorSource.R")
head(alpha.thr.factor,3)
alpha.thr.factor[is.na(alpha.thr.factor)==1]<-0
alpha.thr.factor$sum<-apply(alpha.thr.factor[-1],1,sum)
top5<- alpha.thr.factor[order(alpha.thr.factor$sum,decreasing=T)[1:5],c(1,7)]
top5

