#########################################
# Visual Depiction of Heteroskedasticity
#########################################

library(ggplot2)
set.seed(15)
x1 <- order(rnorm(200, mean = 100, sd = 25))
y <- sapply(x1, function(x) {17+1.8*x + rnorm(1,mean=0, sd = x/4)})
df <- data.frame(x1 = x1, y = y)
fit<-coef(lm(y ~ x1, data = df))
p <- qplot(x1,y,data=df) + geom_abline(intercept=fit[1],slope=fit[2]) + ggtitle("Linear Relationship with Heteroskedastic Errors")+ 
     theme(plot.title = element_text(lineheight=.8, face="bold"))
p

#########################################
# Simple Linear Regression
#########################################

set.seed(5427395)
nObs = 100
x1minrange = 5
x1maxrange = 25
x1 = runif(nObs,x1minrange,x1maxrange)
e = rnorm(nObs,mean = 0, sd = 2.0)
y = 1.67*x1 - 2.93 + e
df = data.frame(y,x1)
myfit <- lm(y~x1,df)
summary(myfit)

p <- qplot(x1,y,data=df) 
p <- p + ggtitle("Simple Linear Regression")  
p <- p + geom_abline(intercept=coef(myfit)[1],slope=coef(myfit)[2], aes(linetype="Estimated Line"), size=1.2, show_guide=T)  
p <- p + geom_abline(intercept = -2.93, slope = 1.67, aes(linetype="Population Regression Line"),size=1.2, show_guide=T)  
p <- p + xlab("Feature x1")  
p <- p + ylab("Output y")  
p <- p + scale_linetype_manual(name="",values=c("Population Regression Line"=1,"Estimated Line"=2))  
p <- p + theme(plot.title = element_text(lineheight=.8, face="bold"),legend.position = "bottom")
p

#########################################
# Hardware Model Setup
#########################################

machine <- read.csv("machine.data", header=F)
names(machine) <- c("VENDOR","MODEL","MYCT", "MMIN", "MMAX", "CACH", "CHMIN", "CHMAX", "PRP", "ERP")
machine <- machine[,3:9]
head(machine,n=3)

library(caret)
set.seed(4352345)
machine_sampling_vector <- createDataPartition(machine$PRP, p = 0.85, list = FALSE)
machine_train <- machine[machine_sampling_vector,]
machine_train_features <- machine[,1:6]
machine_train_labels <- machine$PRP[machine_sampling_vector]
machine_test <- machine[-machine_sampling_vector,]
machine_test_labels <- machine$PRP[-machine_sampling_vector]

machine_correlations <- cor(machine_train_features)
findCorrelation(machine_correlations)
findCorrelation(machine_correlations, cutoff = 0.75)
cor(machine_train$MMIN,machine_train$MMAX)

#########################################
# Cars Model Setup
#########################################

library(caret)
data(cars)

cars_cor <- cor(cars[,-1])
findCorrelation(cars_cor)
findCorrelation(cars_cor, cutoff=0.75)
cor(cars$Doors,cars$coupe)
table(cars$coupe,cars$Doors)

findLinearCombos(cars)

cars <- cars[,c(-15,-18)]
set.seed(232455)
cars_sampling_vector <- createDataPartition(cars$Price, p = 0.85, list = FALSE)
cars_train <- cars[cars_sampling_vector,]
cars_train_features <- cars[,-1]
cars_train_labels <- cars$Price[cars_sampling_vector]
cars_test <- cars[-cars_sampling_vector,]
cars_test_labels <- cars$Price[-cars_sampling_vector]

#########################################
# Constructing the Basic Models
#########################################

machine_model1 <- lm(PRP~.,data=machine_train)
cars_model1 <- lm(Price~.,data=cars_train)

summary(machine_model1)

#########################################
# Aliasing issue
#########################################

cars_model2 <- lm(Price~.-Saturn,data=cars_train)
summary(cars_model2)

#########################################
# Residuals
#########################################

machine_residuals <- machine_model1$residuals
machine_fitted_values <- machine_model1$fitted.values
machine_train_ids <- rownames(machine_train)
machine_large_residuals <- ifelse(abs(machine_residuals) > 150,machine_train_ids,'')
 
p1 <- qplot(machine_fitted_values,machine_residuals) 
p1 <- p1 + ggtitle("Residual Plot for CPU Data Set")
p1 <- p1 + theme(plot.title = element_text(lineheight=.8, face="bold"))
p1 <- p1 + xlab("Fitted Values")  
p1 <- p1 + ylab("Residuals")
p1 <- p1 + geom_text(size = 4, hjust=-0.15, vjust=0.1, aes(label=machine_large_residuals))
p1

cars_residuals <- cars_model1$residuals
cars_fitted_values <- cars_model1$fitted.values
cars_train_ids <- rownames(cars_train)
cars_large_residuals <- ifelse(abs(cars_residuals) > 9500,cars_train_ids,'')
 
p2 <- qplot(cars_fitted_values,cars_residuals) 
p2 <- p2 + ggtitle("Residual Plot for Cars Data Set") 
p2 <- p2 + theme(plot.title = element_text(lineheight=.8, face="bold")) 
p2 <- p2 + xlab("Fitted Values")  
p2 <- p2 + ylab("Residuals")
p2 <- p2 + geom_text(size = 4, hjust=-0.15, vjust=0.1, aes(label=cars_large_residuals))
p2

par(mfrow=c(2,1))
qqnorm(machine_residuals, main = "Normal Q-Q Plot for CPU data set")
qqline(machine_residuals)
qqnorm(cars_residuals, main = "Normal Q-Q Plot for Cars data set")
qqline(cars_residuals)

#########################################
# Significance Tests
#########################################

(q <- 5.210e-02 / 1.885e-02)
pt(q, df = 172, lower.tail = F) * 2

machine_model_null = lm(PRP~1,data=machine_train)
anova(machine_model_null, machine_model1)

n_machine <- nrow(machine_train)
k_machine <- length(machine_model1$coefficients) -1
sqrt(sum(machine_model1$residuals ^ 2) / (n_machine - k_machine - 1)) 

n_cars <- nrow(cars_train)
k_cars <- length(cars_model1$coefficients) -1
sqrt(sum(cars_model1$residuals ^ 2) / (n_cars - k_cars - 1))

mean(machine_train$PRP)
mean(cars_train$Price)

compute_rsquared <- function(x,y) {
  rss <- sum((x-y)^2)
  tss <- sum((y-mean(y))^2)
  return(1-(rss/tss))
}

compute_rsquared(machine_model1$fitted.values,machine_train$PRP)
compute_rsquared(cars_model2$fitted.values,cars_train$Price)

#########################################
# Comparing Regression Models
#########################################

compute_adjusted_rsquared <- function(x,y,p) {
  n <- length(y)
  r2 <- compute_rsquared(x,y)
  return(1 - ((1 - r2) * (n-1)/(n-p-1)))
}

compute_adjusted_rsquared(machine_model1$fitted.values,machine_train$PRP,k_machine)
compute_adjusted_rsquared(cars_model2$fitted.values,cars_train$Price,k_cars)

#########################################
# Test Set performance
#########################################

machine_model1_predictions <- predict(machine_model1, machine_test)
cars_model2_predictions <- predict(cars_model2, cars_test)

compute_mse <- function(predictions, actual) { mean((predictions-actual)^2) }

machine_model1_predictions <- predict(machine_model1, machine_test)
compute_mse(machine_model1$fitted.values, machine_train$PRP)
compute_mse(machine_model1_predictions, machine_test$PRP)

cars_model2_predictions <- predict(cars_model2, cars_test)
compute_mse(cars_model2$fitted.values, cars_train$Price)
compute_mse(cars_model2_predictions, cars_test$Price)

#########################################
# Variance Inflation Factor (VIF)
#########################################

library("car")
vif(cars_model2)

sedan_model <- lm(sedan ~.-Price-Saturn, data=cars_train)
sedan_r2 <- compute_rsquared(sedan_model$fitted.values,cars_train$sedan)
1 - (1-sedan_r2)

#########################################
# Outliers 
#########################################

machine_model2 <- lm(PRP~.,data=machine_train[!(rownames(machine_train)) %in% c(200),])
summary(machine_model2)

machine_model2_predictions <- predict(machine_model2, machine_test)
compute_mse(machine_model2_predictions, machine_test$PRP)

#########################################
# Feature selection
#########################################

machine_model3 <- step(machine_model_null, scope = list(lower = machine_model_null, upper=machine_model1), direction = "forward")

cars_model_null <- lm(Price~1,data=cars_train)
cars_model2 <- lm(Price~.-Saturn,data=cars_train)

cars_model_null <- lm(Price~1,data=cars_train)
cars_model3 <- step(cars_model2, scope=list(lower=cars_model_null, upper=cars_model2), direction="backward")

machine_model3_predictions <- predict(machine_model3, machine_test)
compute_mse(machine_model3_predictions, machine_test$PRP)

cars_model3_predictions <- predict(cars_model3, cars_test)
compute_mse(cars_model3_predictions, cars_test$Price)

#########################################
# Regularization
#########################################

library(glmnet)
cars_train_mat <- model.matrix(Price~.-Saturn, cars_train)[,-1]
lambdas <- 10 ^ seq(8,-4,length=250)
cars_models_ridge= glmnet(cars_train_mat,cars_train$Price,alpha=0,lambda=lambdas)
cars_models_lasso= glmnet(cars_train_mat,cars_train$Price,alpha=1,lambda=lambdas)

cars_models_ridge$lambdas[70]
coef(cars_models_ridge)[,70]

layout(matrix(c(1,2), 2, 1))
plot(cars_models_ridge, xvar = "lambda", main = "Coefficient Values vs. Log Lambda for Ridge Regression")
plot(cars_models_lasso, xvar = "lambda", main = "Coefficient Values vs. Log Lambda for Lasso")

layout(matrix(c(1,2), 1, 2))
plot(cars_models_ridge, xvar = "lambda", main = "Ridge Regression\n", col = gray.colors(1))
plot(cars_models_lasso, xvar = "lambda", main = "Lasso\n", col = gray.colors(1))

ridge.cv <- cv.glmnet(cars_train_mat,cars_train$Price,alpha=0,lambda=lambdas)
lambda_ridge <- ridge.cv$lambda.min
lambda_ridge

lasso.cv <- cv.glmnet(cars_train_mat,cars_train$Price,alpha=1,lambda=lambdas)
lambda_lasso <- lasso.cv$lambda.min
lambda_lasso

layout(matrix(c(1,2), 1, 2))
plot(ridge.cv, col = gray.colors(1))
title("Ridge Regression", line = +2)
plot(lasso.cv, col = gray.colors(1))
title("Lasso", line = +2)

predict(cars_models_lasso, type="coefficients", s = lambda_lasso)

cars_test_mat <- model.matrix(Price~.-Saturn, cars_test)[,-1]
cars_ridge_predictions <- predict(cars_models_ridge, s = lambda_ridge, newx = cars_test_mat)
compute_mse(cars_ridge_predictions, cars_test$Price)
cars_lasso_predictions <- predict(cars_models_lasso, s = lambda_lasso, newx = cars_test_mat)
compute_mse(cars_lasso_predictions, cars_test$Price)
