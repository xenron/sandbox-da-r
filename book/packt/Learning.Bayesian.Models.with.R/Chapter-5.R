###################################################################################################
#Codes used in Chapter 5 of book Learning Bayesian Models with R                                  #
#Author Hari M. Koduvely                                                                          #
###################################################################################################
#R packages required for this chapter
#All installations needs to be done only one time
install.packages("arm")     
install.packages("MCMCpack") 
install.packages("ggplot2") 
install.packages("gridExtra")
library(ggplot2)
library(gridExtra)
library(arm)
attach(dfTrain)

df <- read.csv("/Users/harikoduvely/Projects/Book/Data/ENB2012_data.csv", header = T)
#Excluding variable Y2 and removing records from 768 containing mainly null values
df <- df[1:768,c(1:9)]
str(df)
attach(df)
bp1 <- ggplot(data = df, aes(x = X1, y = Y1)) + geom_point()
bp2 <- ggplot(data = df, aes(x = X2, y = Y1)) + geom_point()
bp3 <- ggplot(data = df, aes(x = X3, y = Y1)) + geom_point()
bp4 <- ggplot(data = df, aes(x = X4, y = Y1)) + geom_point()
bp5 <- ggplot(data = df, aes(x = X5, y = Y1)) + geom_point()
bp6 <- ggplot(data = df, aes(x = X6, y = Y1)) + geom_point()
bp7 <- ggplot(data = df, aes(x = X7, y = Y1)) + geom_point()
bp8 <- ggplot(data = df, aes(x = X8, y = Y1)) + geom_point()

grid.arrange(bp1, bp2, bp3, bp4, bp5, bp6, bp7, bp8, nrow = 2, ncol = 4)
detach(df)
cor.val <- cor(df[,1:8], df[,9], method = "spearman")
cor.val
#Removing X6 and X8 since they don't have significant correlation with Y1
df <- df[,c(1,2,3,4,5,7,9)]
str(df)

#Splitting data set to Train and Test set in the ratio 
set.seed(123)
samp <- sample.int(nrow(df), as.integer(nrow(df)*0.2), replace = F)
dfTest <- df[samp,]
dfTrain <- df[-samp,]
xtest <- dfTest[,1:6]
ytest <- dfTest[,7]

#Regression using arm package


#Ordinary Multivariate Regression
fit.ols <- lm(Y1 ~ X1 + X2 + X3 + X4 + X5 + X7, data = dfTrain)
summary(fit.ols)
#plot(fit.ols)
fit.coeff <- fit.ols$coefficients
ypred.ols <- predict.lm(fit.ols, xtest, interval = "prediction", se.fit = T)
yout.ols <- as.data.frame(cbind(ytest,ypred.ols$fit))
ols.upr <- yout.ols$upr
ols.lwr <- yout.ols$lwr

p.ols <- ggplot(data = yout.ols, aes(x = yout.ols$ytest, y = yout.ols$fit)) + geom_point() + ggtitle("Ordinary Regression Prediction on Test Data") + labs(x = "Y-Test", y = "Y-Pred")
p.ols + geom_errorbar(ymin = ols.lwr, ymax = ols.upr)
err.ols <- ytest - ypred.ols


#Bayesian GLM
fit.bayes <- bayesglm(Y1 ~ X1 + X2 + X3 + X4 + X5 + X7, family=gaussian(link=identity), data=dfTrain, prior.df = Inf, prior.mean = 0, prior.scale = NULL, maxit = 10000)
ypred.bayes <- predict.glm(fit.bayes, newdata = xtest, se.fit = T)
yout.bayes <- as.data.frame(cbind(ytest,ypred.bayes$fit))
names(yout.bayes) <- c("ytest", "fit")
critval <- 1.96 #approx for 95% CI
bayes.upr <- ypred.bayes$fit + critval * ypred.bayes$se.fit
bayes.lwr <- ypred.bayes$fit - critval * ypred.bayes$se.fit

p.bayes <- ggplot(data = yout.bayes, aes(x = yout.bayes$ytest, y = yout.bayes$fit)) + geom_point() + ggtitle("Bayesian Regression Prediction on Test Data") + labs(x = "Y-Test", y = "Y-Pred")
p.bayes + geom_errorbar(ymin = bayes.lwr, ymax = bayes.upr)


p1 <-  p.ols + geom_errorbar(ymin = ols.lwr, ymax = ols.upr)
p2 <-  p.bayes + geom_errorbar(ymin = bayes.lwr, ymax = bayes.upr)

grid.arrange(p1, p2, ncol = 2)
detach(dfTrain)

posterior.bayes <- as.data.frame(coef(sim(fit.bayes)))
attach(posterior.bayes)

h1 <- ggplot(data = posterior.bayes, aes(x = X1)) + geom_histogram() + ggtitle("Histogram X1")
h2 <- ggplot(data = posterior.bayes, aes(x = X2)) + geom_histogram() + ggtitle("Histogram X2")
h3 <- ggplot(data = posterior.bayes, aes(x = X3)) + geom_histogram() + ggtitle("Histogram X3")
h4 <- ggplot(data = posterior.bayes, aes(x = X4)) + geom_histogram() + ggtitle("Histogram X4")
h5 <- ggplot(data = posterior.bayes, aes(x = X5)) + geom_histogram() + ggtitle("Histogram X5")
h7 <- ggplot(data = posterior.bayes, aes(x = X7)) + geom_histogram() + ggtitle("Histogram X7")
grid.arrange(h1,h2,h3,h4,h5,h7, nrow = 2, ncol = 3)

detach(posterior.bayes)

