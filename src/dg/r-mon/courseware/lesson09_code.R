##准备工作##
install.packages("vcd")
install.packages("RColorBrewer")
install.packages("sm")
install.packages("car")
install.packages("hexbin")

library(learningr)
#####散点图#####
###base作图系统###
##添加了最佳拟合曲线的散点图
attach(mtcars)                                                     
plot(wt, mpg, 
     main="Basic Scatterplot of MPG vs. Weight",       
     xlab="Car Weight (lbs/1000)", 
     ylab="Miles Per Gallon ", pch=19)
abline(lm(mpg ~ wt), col="red", lwd=2, lty=1)     #最佳拟合的线性直线
lines(lowess(wt, mpg), col="blue", lwd=2, lty=2)     #最佳拟合曲线

##使用car包绘制更复杂的散点图
library(car) 
scatterplot(mpg ~ wt | cyl, data=mtcars, lwd=2,
            main="Scatter Plot of MPG vs. Weight by # Cylinders", 
            xlab="Weight of Car (lbs/1000)", 
            ylab="Miles Per Gallon", id.method="identify",
            legend.plot=TRUE, labels=row.names(mtcars), 
            boxplots="xy")

###散点图矩阵
pairs(~ mpg + disp + drat + wt, data=mtcars, 
      main="Basic Scatterplot Matrix")
##散点图矩阵中的参数调整
scatterplotMatrix(~ mpg + disp + drat + wt, data=mtcars, spread=FALSE,
                  lty.smooth=2, main="Scatterplot Matrix via car package")

###高密度散点图
##案例
set.seed(1234)
n <- 10000
c1 <- matrix(rnorm(n, mean=0, sd=.5), ncol=2)
c2 <- matrix(rnorm(n, mean=3, sd=2), ncol=2)
mydata <- rbind(c1, c2)
mydata <- as.data.frame(mydata)
names(mydata) <- c("x", "y")
with(mydata,
     plot(x, y, pch=19, main="Scatter Plot with 10000 Observations"))  #生成标准散点图
with(mydata,
     smoothScatter(x, y, main="Scatterplot colored by Smoothed Densities"))  #利用光平滑密度估计生成的散点图
#利用Hexbin包生成高密度散点图
library(hexbin)
with(mydata, {
  bin <- hexbin(x, y, xbins=50)
  plot(bin, main="Hexagonal Binning with 10,000 Observations")
})


###气泡图
attach(mtcars)
r <- sqrt(disp/pi)
symbols(wt, mpg, r, inches=0.30, fg="white", bg="lightblue",
        main="Bubble Plot with point size proportional to displacement",
        ylab="Miles Per Gallon",
        xlab="Weight of Car (lbs/1000)")
text(wt, mpg, rownames(mtcars), cex=0.6)
detach(mtcars)


###lattice作图系统###
library(lattice)
obama_vs_mccain<-obama_vs_mccain[!is.na(obama_vs_mccain$Turnout),]
xyplot(Turnout ~ Income, obama_vs_mccain)
xyplot(Turnout ~ Income, obama_vs_mccain, col = "violet", pch = 20)
xyplot(
  Turnout ~ Income,
  obama_vs_mccain,
  scales = list(log = TRUE) # x 和 y 轴都是对数坐标
) 
xyplot(
  Turnout ~ Income,
  obama_vs_mccain,
  scales = list(y = list(log = TRUE)) # y 轴对数坐标
)

xyplot(
  Turnout ~ Income | Region,
  obama_vs_mccain,
  scales = list(
    log = TRUE,
    relation = "same",
    alternating = FALSE
  ),
  layout = c(5, 2)
)

(lat1 <- xyplot(
  Turnout ~ Income | Region,
  obama_vs_mccain
))
(lat2 <- update(lat1, col = "violet", pch = 20))

###ggplot2作图系统###
library(ggplot2)
ggplot(obama_vs_mccain, aes(Income, Turnout)) +
  geom_point()

ggplot(obama_vs_mccain, aes(Income, Turnout)) +
  geom_point(color = "violet", shape = 20)

ggplot(obama_vs_mccain, aes(Income, Turnout)) +
  geom_point() +
  scale_x_log10(breaks = seq(2e4, 4e4, 1e4)) +
  scale_y_log10(breaks = seq(50, 75, 5))

ggplot(obama_vs_mccain, aes(Income, Turnout)) +
  geom_point() +
  scale_x_log10(breaks = seq(2e4, 4e4, 1e4)) +
  scale_y_log10(breaks = seq(50, 75, 5)) +
  facet_wrap(~ Region, ncol = 4)

(gg1 <- ggplot(obama_vs_mccain, aes(Income, Turnout)) +
   geom_point()
)
(gg2 <- gg1 +
   facet_wrap(~ Region, ncol = 5) +
   theme(axis.text.x = element_text(angle = 30, hjust = 1))
)

#####线图#####
##base
with(
  crab_tag$daylog,
  plot(Date, -Max.Depth, type = "l", ylim = c(-max(Max.Depth), 0))
)

with(
  crab_tag$daylog,
  lines(Date, -Min.Depth, col = "blue")
)

##lattice
xyplot(-Min.Depth + -Max.Depth ~ Date, crab_tag$daylog, type = "l")

##ggplot2
ggplot(crab_tag$daylog, aes(Date, -Min.Depth)) +
  geom_line()

ggplot(crab_tag$daylog, aes(Date)) +
  geom_line(aes(y = -Max.Depth)) +
  geom_line(aes(y = -Min.Depth))

library(reshape2)
crab_long <- melt(
  crab_tag$daylog,
  id.vars = "Date",
  measure.vars = c("Min.Depth", "Max.Depth")
) 
ggplot(crab_long, aes(Date, -value, group = variable)) +
  geom_line()

ggplot(crab_tag$daylog, aes(Date, ymin = -Min.Depth, ymax = -Max.Depth)) +
  geom_ribbon(color = "black", fill = "white")

#####直方图#####
###base
##简单直方图
par(mfrow = c(1,1))
hist(mtcars$mpg)
##指定组数和颜色
hist(mtcars$mpg, breaks = 12, col = "red", 
     xlab = "Miles Per Gallon", 
     main = "Colored histogram with 12 bins")
##添加轴须图
hist(mtcars$mpg, freq = FALSE, breaks = 12, col = "red", 
     xlab = "Miles Per Gallon", 
     main = "Histogram, rug plot, density curve")
rug(jitter(mtcars$mpg))  #添加轴须图
lines(density(mtcars$mpg), col = "blue", lwd = 2)  #添加密度曲线
##添加正态密度曲线和外框
x <- mtcars$mpg
h <- hist(x, breaks = 12, col = "red", 
          xlab = "Miles Per Gallon", 
          main = "Histogram with normal curve and box")
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit * diff(h$mids[1:2]) * length(x)
lines(xfit, yfit, col = "blue", lwd = 2)  #添加正态密度曲线
box()  #添加外框


###lattice
histogram(~ Obama, obama_vs_mccain)

histogram(~ Obama, obama_vs_mccain, breaks = 10)

histogram(~ Obama, obama_vs_mccain, type = "percent")

###ggplot2
ggplot(obama_vs_mccain, aes(Obama)) +
  geom_histogram(binwidth = 5)

ggplot(obama_vs_mccain, aes(Obama, ..density..)) +
  geom_histogram(binwidth = 5)

#####箱线图#####
###base
boxplot(Obama ~ Region, data = obama_vs_mccain)

ovm <- within(
  obama_vs_mccain,
  Region <- reorder(Region, Obama, median)
) 
boxplot(Obama ~ Region, data = ovm)

##并列箱型图
boxplot(mpg ~ cyl, data = mtcars, 
        main = "Car Milage Data", 
        xlab = "Number of Cylinders", 
        ylab = "Miles Per Gallon")

##两个因子的箱线图
mtcars$cyl.f <- factor(mtcars$cyl, levels = c(4, 6, 8), labels = c("4", "6", "8")) #创建气缸数量的因子
mtcars$am.f <- factor(mtcars$am, levels = c(0, 1), 
                      labels = c("auto", "standard"))  #创建变速箱类型的因子
boxplot(mpg ~ am.f * cyl.f, data = mtcars, 
        varwidth = TRUE, col = c("gold", "darkgreen"),  #varwidth=TRUE将使箱型图的颜色与其样本大小的平方根成正比。col规定了箱型图的颜色。
        main = "MPG Distribution by Auto Type", 
        xlab = "Auto Type")


###lattice
bwplot(Obama ~ Region, data = ovm)

###ggplot2
ggplot(ovm, aes(Region, Obama)) +
  geom_boxplot()

#####条形图#####
###base###
###简单条形图
##垂直条形图
library(vcd)
counts<-table(Arthritis$Improved)
barplot(counts,
        main="Simple Bar Plot",
        xlab="Improvement",ylab="Frequency")  
##水平条形图
barplot(counts,
        main="Horizontal Bar Plot",
        xlab="Frequency",ylab="Improvement",
        horiz=TRUE) 

###堆砌条形图
counts <- table(Arthritis$Improved, Arthritis$Treatment)
counts
barplot(counts, main = "Stacked Bar Plot", xlab = "Treatment", 
        ylab = "Frequency", col = c("red", "yellow", "green"), #col赋予颜色
        legend = rownames(counts))
###分组条形图
barplot(counts, main = "Grouped Bar Plot", xlab = "Treatment", 
        ylab = "Frequency", col = c("red", "yellow", "green"),  
        beside = TRUE)


###均值条形图
states <- data.frame(state.region, state.x77)
means <- aggregate(states$Illiteracy, 
                   by = list(state.region), 
                   FUN = mean)
means <- means[order(means$x), ]  #将均值从小到大排序
barplot(means$x, names.arg = means$Group.1)
title("均值条形图")   #命名


library(RColorBrewer)
par(mfrow=c(2,1),mar=c(3,2.5,0.5,0.1))
death=t(VADeaths)[,5:1]
barplot(death,col=brewer.pal(4,"Set1"))
barplot(death,col=brewer.pal(4,"Set1"),beside=TRUE,
        legend=TRUE)

###棘状图
attach(Arthritis)
counts <- table(Treatment, Improved)
spine(counts, main = "Spinogram Example")
detach(Arthritis)

###lattice###
barchart(State ~ Catholic, ovm)
barchart(
  State ~ Catholic + Protestant + Non.religious + Other,
  ovm,
)

###ggplot2###
religions_long <- melt(
  ovm,
  id.vars = "State",
  measure.vars = c("Catholic", "Protestant", "Non.religious", "Other")
)

ggplot(religions_long, aes(State, value, fill = variable)) +
  geom_bar(stat = "identity") +
  coord_flip()

ggplot(religions_long, aes(State, value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip()


#####核密度图#####
##简单核密度图
par(mfrow = c(1, 1))
d <- density(mtcars$mpg)
plot(d)
##进阶
d <- density(mtcars$mpg)
plot(d, main = "Kernel Density of Miles Per Gallon") #取标题
polygon(d, col = "red", border = "blue") #加填充色、曲线颜色
rug(mtcars$mpg, col = "brown") #增加轴须图、为轴须图指定颜色

###用核密度图比较组间差异
par(lwd = 2)  #将线条宽度设置为双倍
library(sm)
attach(mtcars)  #绑定数据框
cyl.f <- factor(cyl, levels = c(4, 6, 8), 
                labels = c("4 cylinder", "6 cylinder", "8 cylinder"))  #创建分组因子
sm.density.compare(mpg, cyl, xlab = "Miles Per Gallon")  #绘制核密度图
title(main = "MPG Distribution by Car Cylinders")   #添加标题
##进阶：添加图例
colfill <- c(2:(2 + length(levels(cyl.f))))
cat("Use mouse to place legend...", "\n\n")
legend(locator(1), levels(cyl.f), fill = colfill)
detach(mtcars)
par(lwd = 1)


#####点图#####
##简单点图
dotchart(mtcars$mpg, labels = row.names(mtcars), 
         cex = 0.7, 
         main = "Gas Milage for Car Models", 
         xlab = "Miles Per Gallon")
##经过排序、分组、着色后的点图
x <- mtcars[order(mtcars$mpg), ]  #进行排序
x$cyl <- factor(x$cyl)
x$color[x$cyl == 4] <- "red"   #四缸着色为红色
x$color[x$cyl == 6] <- "blue"    #六缸着色为蓝色
x$color[x$cyl == 8] <- "darkgreen"   #八缸着色为深绿色
dotchart(x$mpg, labels = row.names(x), cex = 0.7,    #指定标签和标签大小
         pch = 19, groups = x$cyl, 
         gcolor = "black", color = x$color, 
         main = "Gas Milage for Car Models\ngrouped by cylinder", 
         xlab = "Miles Per Gallon")
