######install packages######
install.packages("RODBC")
install.packages("xts")
install.packages("dygraphs")
install.packages("xtsExtra", repos="http://R-Forge.R-project.org")
# http://r-forge.r-project.org/src/contrib/xtsExtra_0.0.876.tar.gz
install.packages("quantmod")
install.packages("forecast")
install.packages("urca")
install.packages("FinTS")
install.packages("rugarch")



######load data######
library("RODBC")
conn<-odbcConnectExcel2007("D:/data/Amtrak.xls")
Amtrak<-sqlFetch(conn,"Data")
close(conn)
head(Amtrak)

# 转换为时间序列的数据
library(xts)
# 只保留第二列
Amtrak<-xts(Amtrak[,2],order.by=Amtrak[,1])
plot(Amtrak)
plot(Amtrak['2000/2003'])


######dygraphs######
install.packages("dygraphs")

library(dygraphs)
lungDeaths <- cbind(mdeaths, fdeaths)
dygraph(lungDeaths)

#增加时间选择条
dygraph(lungDeaths) %>% dyRangeSelector()

#其他设置
dygraph(lungDeaths) %>%
  dySeries("mdeaths", label = "Male") %>%
  dySeries("fdeaths", label = "Female") %>%
  dyOptions(stackedGraph = TRUE) %>%
  dyRangeSelector(height = 20)

#增加预测区域
hw <- HoltWinters(ldeaths)
predicted <- predict(hw, n.ahead = 72, prediction.interval = TRUE)

dygraph(predicted, main = "Predicted Lung Deaths (UK)") %>%
  dyAxis("x", drawGrid = FALSE) %>%
  dySeries(c("lwr", "fit", "upr"), label = "Deaths") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))

##############################
##############################
##############################

library(xts)
library(xtsExtra)
library(quantmod)
library(forecast)
library(urca)
library(FinTS)
library(rugarch)

########xts包简介#######
#读取上证指数数据（开盘，收盘，最高，最低）
index.dt <- read.table("D:/R Quant/data/Index/TRD_Index.txt", header=TRUE)
head(index.dt)
# 去掉第一列
SH.index <- index.dt[index.dt$Indexcd == 1, -1]
head(SH.index)
# 将交易日期去除，order.by将日期数据转化为行号
SH.index <- xts(SH.index[, -1], order.by = as.Date(SH.index$Trddt))
head(SH.index)

# 选取2012年的数据
SH.index.2012 <- SH.index["2012-01-01/2013-01-01"]
head(SH.index.2012, 3)
tail(SH.index.2012, 3)


# xts数据子集的其他提取方式（begin）
SH.index.2012 <- SH.index["2012"]
head(SH.index.2012,3)
tail(SH.index.2012,3)

# 不设置最终日期，一直提取到最新的数据
SH.index.after2010 <- SH.index["2010-01-01/"]
head(SH.index.after2010, 3)
tail(SH.index.after2010, 3)

# 省略月日的设置
SH.index.2010MartoEnd <- SH.index["2010-03/2010"]
head(SH.index.2010MartoEnd,3)
tail(SH.index.2010MartoEnd,3)
# xts数据子集的其他提取方式（end）



# OHLC数据格式
SH.ohlc <- SH.index[, c("Trddt", "Opnindex", "Hiindex", "Loindex", "Clsindex")]
# 变成标准的ohlc格式数据，每日交易数据
SH.ohlc.daily <- to.daily(SH.ohlc)
head(SH.ohlc.daily)
# 提取每周交易数据
SH.ohlc.weekly <- to.weekly(SH.ohlc)
head(SH.ohlc.weekly)



# 自动算周收益率 日收益率dailyRuturn()
SH.ret <- weeklyReturn(SH.ohlc)
barChart(SH.ret, main = "Time Series of SH Index Weekly Return", 
         theme = chartTheme("white"), col = 'darkblue', border = FALSE)



# period.apply函数（begin）
ep <- endpoints(SH.ohlc, 'weeks') 
#对数据按照每一周分组
#自动计算周的最高值，最低值等

ep
# 计算每周最高值（最低值等数据）
max <- period.apply(SH.ohlc[, 4], ep, max)
head(max)
# period.apply函数（end）


#画出上证指数收益率的直方图
hist(SH.ret, breaks = 50, col = 'darkgreen', border = FALSE, main = "Histogram of SH Index Weekly Return")
# 时间序列的基本描述
summary(coredata(SH.ret))
# 设置更详细的百分位数
quantile(coredata(SH.ret), probs = seq(0, 1, 0.1))
mean(coredata(SH.ret))
sd(coredata(SH.ret))
