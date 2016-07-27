####################################################
# Some Plotting Functions
####################################################

getLineParameters<-function(p1,p2) {
  gradient <- (p2[2]-p1[2])/(p2[1]-p1[1])
  intercept <- p1[2] - gradient*p1[1]
  return(list(gradient=gradient,intercept=intercept))
}

getEuclideanDistance<-function(x1,x2,y1,y2) {
  return(sqrt( (x1-x2)^2 + (y1-y2)^2 ))
}

getMarginSegmentCoordinates<-function(xvar, yvar, intercept, gradient) {
  # 1.Compute all the coordinates of the perpendicular line segments
  # from your data points on the line
  a <- intercept
  b <- gradient
  xnorm <- (xvar+b*yvar-a*b)/(1+b^2)
  ynorm <- a + b*xnorm

  distances <- mapply(getEuclideanDistance,xvar,xnorm,yvar,ynorm)
  selector <- distances==min(distances)
  
  # Return a list with the input points you kept and the 
  return(list(xvar=xvar[selector], yvar=yvar[selector], xnorm=xnorm[selector], ynorm=ynorm[selector], margin=min(distances)))
}

getAllSegmentCoordinates<-function(xvar, yvar, intercept, gradient) {
  # 1.Compute all the coordinates of the perpendicular line segments
  # from your data points on the line
  a <- intercept
  b <- gradient
  xnorm <- (xvar+b*yvar-a*b)/(1+b^2)
  ynorm <- a + b*xnorm
    
  # Return a list with the input points you kept and the 
  return(list(xvar=xvar, yvar=yvar, xnorm=xnorm, ynorm=ynorm))
}

convertCoeffsToLineParameters<-function(coeffs) {
  b0 <- coeffs[1]
  b1 <- coeffs[2]
  b2 <- coeffs[3]
  return(list(intercept=(b0/-b2),gradient=b1/-b2))
}

####################################################
# The Plots Themselves
####################################################

library('MASS')
set.seed(808881)
n <- 100
x1 <- mvrnorm(n, mu=c(13,45), Sigma=matrix(c(2,0.5,05,2),nrow=2))
set.seed(675682)
x2 <- mvrnorm(n, mu=c(19,39), Sigma=matrix(c(2,0.5,05,2),nrow=2))
f1 <- c(x1[,1],x2[,1])
f2 <- c(x1[,2],x2[,2])
z <- c(rep.int(1,n),rep.int(-1,n))

l1<-getLineParameters(c(12,40.8),c(20,42.9))
l2<-getLineParameters(c(15,36),c(18,48))

library(e1071)
df=data.frame(f1,f2,y=as.factor(z))
svmfit<-svm(y~.,data=df,kernel="linear",cost=100,scale=FALSE)
w <- t(svmfit$coefs) %*% svmfit$SV
coefs<-c(-svmfit$rho,w)
lineparams<-convertCoeffsToLineParameters(coefs)

####################################################
# Plot 1 showing different separating lines
####################################################

library(ggplot2)
p <- ggplot(data = NULL, aes(x=f1, y=f2, shape = ifelse(z > 0, "Class 1","Class -1")))
p <- p + geom_point()
p <- p + ggtitle("Linearly Separable Classes in 2D Feature Space")
p <- p + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2), legend.position="bottom")
p <- p + xlab("f1")  
p <- p + ylab("f2") 
p <- p + scale_shape_manual(name="Class Labels", values=c(1,15))
p <- p + geom_abline(intercept = l1$intercept, slope = l1$gradient, aes(linetype="General Separating Line"), size = 0.5, show_guide=T)
p <- p + geom_abline(intercept = l2$intercept, slope = l2$gradient, aes(linetype="General Separating Line"), size = 0.5, show_guide=T)
p <- p + geom_abline(intercept = lineparams$intercept, slope = lineparams$gradient, aes(linetype="Line of Maximum Margin Separation"), size = 1, show_guide=T)
p <- p + scale_linetype_manual(name = "Separating Lines", values = c("dashed","solid"))
p <- p + guides(shape = guide_legend(override.aes = list(linetype = 0 )), 
       linetype = guide_legend())
p

####################################################
# Plot 2 showing margin
####################################################

marlist<-getAllSegmentCoordinates(f1,f2,lineparams$intercept, lineparams$gradient)
distances <- mapply(getEuclideanDistance,marlist$xvar[svmfit$index], marlist$xnorm[svmfit$index], marlist$yvar[svmfit$index], marlist$ynorm[svmfit$index])

library(grid)
p <- ggplot()
p <- p + geom_point(data = NULL, aes(x=f1, y=f2, shape = ifelse(z > 0, "Class 1","Class -1")))
p <- p + ggtitle("Computing the Margin of a Decision Boundary in 2D Feature Space")
p <- p + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2), legend.position="bottom")
p <- p + xlab("f1")  
p <- p + ylab("f2") 
p <- p + scale_shape_manual(name="Class Labels", values=c(1,15))
p <- p + geom_abline(intercept = lineparams$intercept, slope = lineparams$gradient, size = 1, show_guide=T)
p <- p + guides(shape = guide_legend(override.aes = list(linetype = 0 )), 
       linetype = guide_legend())
p <- p + geom_segment(data=NULL, aes(x = marlist$xvar[svmfit$index], y = marlist$yvar[svmfit$index], xend = marlist$xnorm[svmfit$index], yend = marlist$ynorm[svmfit$index]), arrow = arrow(ends="both", length = unit(0.3, "cm")))
p <- p + annotate("text", x = marlist$xnorm[svmfit$index[2]], y = marlist$ynorm[svmfit$index[2]], label = paste("Margin = ",round(distances[1],2)),hjust=1.7, vjust = -1.5)
p

####################################################
# Plot 3
####################################################

newf1<-c(f1,16)
newf2<-c(f2,40)
newz<-c(z,1)
newdf=data.frame(f1=newf1,f2=newf2,y=as.factor(newz))
svmfit2<-svm(y~.,data=newdf,kernel="linear",cost=100,scale=FALSE)
w2 <- t(svmfit2$coefs) %*% svmfit2$SV
coefs2<-c(-svmfit2$rho,w2)
lineparams2<-convertCoeffsToLineParameters(coefs2)
marlist2<-getAllSegmentCoordinates(newf1,newf2,lineparams2$intercept, lineparams2$gradient)
distances2 <- mapply(getEuclideanDistance,marlist2$xvar[svmfit2$index], marlist2$xnorm[svmfit2$index], marlist2$yvar[svmfit2$index], marlist2$ynorm[svmfit2$index])

library(grid)
p <- ggplot()
p <- p + geom_point(data = NULL, aes(x=newf1, y=newf2, shape = ifelse(newz > 0, "Class 1","Class -1")))
p <- p + ggtitle("Computing the Margin of a Decision Boundary in 2D Feature Space")
p <- p + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2), legend.position="bottom")
p <- p + xlab("f1")  
p <- p + ylab("f2") 
p <- p + scale_shape_manual(name="Class Labels", values=c(1,15))
p <- p + geom_abline(intercept = lineparams2$intercept, slope = lineparams2$gradient, size = 1, show_guide=T)
p <- p + guides(shape = guide_legend(override.aes = list(linetype = 0 )), 
       linetype = guide_legend())
p <- p + geom_segment(data=NULL, aes(x = marlist2$xvar[svmfit2$index], y = marlist2$yvar[svmfit2$index], xend = marlist2$xnorm[svmfit2$index], yend = marlist2$ynorm[svmfit2$index]), arrow = arrow(ends="both", length = unit(0.2, "cm")))
p <- p + annotate("text", x = marlist2$xnorm[svmfit2$index[2]], y = marlist2$ynorm[svmfit2$index[2]], label = paste("Margin = ",round(distances2[1],2)),hjust=1.7, vjust = -1.5)
p

####################################################
# Biodegration Data Set
####################################################

bdf <- read.table("biodeg.csv", sep=";", quote="\"")
levels(bdf$V42)<-c(0,1)

library(caret)
set.seed(23419002)
bdf_sampling_vector <- createDataPartition(bdf$V42, p = 0.80, list = FALSE)
bdf_train <- bdf[bdf_sampling_vector,]
bdf_test <- bdf[-bdf_sampling_vector,]

library(e1071)
model_lin<-svm(V42~.,data=bdf_train,kernel="linear",cost=10)

mean(bdf_train[,42] == model_lin$fitted)
table(actual = bdf_train[,42],predictions = model_lin$fitted)

test_predictions <- predict(model_lin,bdf_test[,1:41])
mean(bdf_test[,42] == test_predictions)

####################################################
# Function for different linear costs
####################################################

getAccuraciesOfLinearSVM<-function(cost) {
  
  model_lin<-svm(V42~.,data=bdf_train,kernel="linear",cost=cost)
  train_accuracy <- signif(mean(bdf_train[,42] == model_lin$fitted), digits = 3)
  test_predictions <- predict(model_lin,bdf_test[,1:41])
  test_accuracy <- signif(mean(bdf_test[,42] == test_predictions), digits = 3)
  return(list(training=train_accuracy, test=test_accuracy))
}

cost <- c(0.01, 0.1, 1, 10, 100, 1000)
linearPerformances <- sapply(cost, getAccuraciesOfLinearSVM)
colnames(linearPerformances) <- cost
linearPerformances

####################################################
# Radial Kernel
####################################################

model_radial<-svm(V42~., data=bdf_train, kernel="radial", cost=10, gamma=0.5)

mean(bdf_train[,42] == model_radial$fitted)
table(actual = bdf_train[,42],predictions = model_radial$fitted)

test_predictions <- predict(model_radial,bdf_test[,1:41])
mean(bdf_test[,42] == test_predictions)


####################################################
# Function for different radial costs and gamma
####################################################

getAccuraciesOfRadialSVM<-function(cost, gamma) {
  
  model_radial<-svm(V42~.,data=bdf_train,kernel="radial",cost=cost, gamma=gamma)
  train_accuracy <- signif(mean(bdf_train[,42] == model_radial$fitted), digits = 3)
  test_predictions <- predict(model_radial,bdf_test[,1:41])
  test_accuracy <- signif(mean(bdf_test[,42] == test_predictions), digits = 3)
  return(list(training=train_accuracy, test=test_accuracy))
}

cost <- c(0.01, 0.1, 1, 10, 100)
gamma <- c(0.01, 0.05, 0.1, 0.5, 1)
grid_params <- expand.grid(cost, gamma)
radialPerformances_u <- mapply(getAccuraciesOfRadialSVM, grid_params[,1], grid_params[,2])
radialPerformances <- t(data.frame(cost = grid_params[,1], gamma = grid_params[,2], t(radialPerformances_u)))
radialPerformances

bdf_radial_tune <- tune(svm,V42~.,data=bdf_train, kernel="radial", ranges = list(cost=c(0.01,0.1,1,10,100), gamma=c(0.01,0.05,0.1,0.5,1)))

####################################################
# German Data Set
####################################################

german_raw <- read.table("german.data", quote="\"")

names(german_raw) <- c("checking", "duration", "creditHistory", "purpose", "credit", "savings", "employment", "installmentRate", "personal", "debtors", "presentResidence", "property", "age", "otherPlans", "housing", "existingBankCredits", "job", "dependents", "telephone", "foreign", "risk")

library(caret)
dummies <- dummyVars(risk ~ ., data = german_raw)
german<- data.frame(predict(dummies, newdata = german_raw), risk=factor((german_raw$risk-1)))

set.seed(977)
german_sampling_vector <- createDataPartition(german$risk, p = 0.80, list = FALSE)
german_train <- german[german_sampling_vector,]
german_test <- german[-german_sampling_vector,]

class_weights <- c(1,5)
names(class_weights) <- c("0","1")
class_weights

set.seed(2423)
german_radial_tune <- tune(svm,risk~.,data=german_train, kernel="radial", ranges = list(cost=c(0.01,0.1,1,10,100), gamma=c(0.01,0.05,0.1,0.5,1)),class.weights = class_weights)
german_radial_tune$best.parameters
german_radial_tune$best.performance

german_model <- german_radial_tune$best.model
test_predictions <- predict(german_model,german_test[,1:61])
mean(test_predictions == german_test[,62])
table(predicted = test_predictions, actual = german_test[,62])

set.seed(2423)
german_radial_tune_unbiased <- tune(svm,risk~.,data=german_train, kernel="radial", ranges = list(cost=c(0.01,0.1,1,10,100), gamma=c(0.01,0.05,0.1,0.5,1)))
german_radial_tune_unbiased$best.parameters
german_radial_tune_unbiased$best.performance


