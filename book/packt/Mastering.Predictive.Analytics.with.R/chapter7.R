#########################################
# Bagged Skillcraft
#########################################

# Reload data from Chapter 6
skillcraft <- read.csv("SkillCraft1_Dataset.csv")

skillcraft<-skillcraft[-1]
skillcraft$TotalHours=as.numeric(levels(skillcraft$TotalHours))[skillcraft$TotalHours]
skillcraft$HoursPerWeek=as.numeric(levels(skillcraft$HoursPerWeek))[skillcraft$HoursPerWeek]
skillcraft$Age=as.numeric(levels(skillcraft$Age))[skillcraft$Age]
skillcraft<-skillcraft[complete.cases(skillcraft),]

library(caret)
set.seed(133)
skillcraft_sampling_vector <- createDataPartition(skillcraft$LeagueIndex, p = 0.80, list = FALSE)
skillcraft_train <- skillcraft[skillcraft_sampling_vector,]
skillcraft_test <- skillcraft[-skillcraft_sampling_vector,]

compute_SSE <- function(correct,predictions) {
  return(sum((correct-predictions)^2))
}

# Bagging for Skillcraft Data Set

library("ipred")
baggedtree <- bagging(LeagueIndex~., data=skillcraft_train, nbagg=100, coob=T)
baggedtree_predictions <- predict(baggedtree, skillcraft_test)
(baggedtree_SSE <- compute_SSE(baggedtree_predictions, skillcraft_test$LeagueIndex))

#########################################
# Bagged for Heart Data Set
#########################################

# First reload the heart data set just as we had in chapter 3

heart <- read.table("heart.dat", quote="\"")
names(heart) <- c("AGE", "SEX", "CHESTPAIN", "RESTBP", "CHOL", "SUGAR", "ECG", "MAXHR", "ANGINA", "DEP", "EXERCISE", "FLUOR", "THAL", "OUTPUT")

heart$CHESTPAIN = factor(heart$CHESTPAIN)
heart$ECG = factor(heart$ECG)
heart$THAL = factor(heart$THAL)
heart$EXERCISE = factor(heart$EXERCISE)

heart$OUTPUT = heart$OUTPUT-1

library(caret)
set.seed(987954)
heart_sampling_vector <- createDataPartition(heart$OUTPUT, p = 0.85, list = FALSE)
heart_train <- heart[heart_sampling_vector,]
heart_train_labels <- heart$OUTPUT[heart_sampling_vector]
heart_test <- heart[-heart_sampling_vector,]
heart_test_labels <- heart$OUTPUT[-heart_sampling_vector]

M <- 11
seeds <- 70000 : (70000 + M - 1)
n <- nrow(heart_train)
sample_vectors<-sapply(seeds,function(x) { set.seed(x); return(sample(n,n,replace=T)) })

train_1glm <- function(sample_indices) { 
	data <- heart_train[sample_indices,]; 
	model <- glm(OUTPUT~., data=data, family=binomial("logit")); 
	return(model)
}

models <- apply(sample_vectors, 2, train_1glm)

get_1bag <- function(sample_indices) {
	unique_sample <- unique(sample_indices); 
	df <- heart_train[unique_sample, ]; 
	df$ID <- unique_sample; 
	return(df)
}

bags<-apply(sample_vectors, 2, get_1bag)

glm_predictions <- function(model, data, model_index) {
	colname <- paste("PREDICTIONS",model_index);
	data[colname] <- as.numeric(predict(model,data,type="response") > 0.5); 
	return(data[,c("ID",colname), drop=FALSE])
}

training_predictions <- mapply(glm_predictions,models,bags,1:M,SIMPLIFY=F)

train_pred_df <- Reduce(function(x, y) merge(x, y, by = "ID", all = T), training_predictions)

train_pred_vote<-apply(train_pred_df[,-1],1,function(x) as.numeric(mean(x,na.rm=TRUE)>0.5))
(training_accuracy<-mean(train_pred_vote==heart_train$OUTPUT[as.numeric(train_pred_df$ID)]))

# Out of Bag Accuracy

get_1oo_bag <- function(sample_indices) {
	unique_sample <- setdiff(1:n,unique(sample_indices)); 
	df <- heart_train[unique_sample,]; 
	df$ID <- unique_sample; 
	if (length(unique(heart_train[sample_indices,]$ECG)) < 3) df[df$ECG == 1,"ECG"] = NA; 
	return(df)
}

oo_bags <- apply(sample_vectors,2, get_1oo_bag)
oob_predictions<-mapply(glm_predictions, models, oo_bags, 1:M,SIMPLIFY=F)
oob_pred_df <- Reduce(function(x, y) merge(x, y, by="ID", all=T), oob_predictions)
oob_pred_vote<-apply(oob_pred_df[,-1], 1, function(x) as.numeric(mean(x,na.rm=TRUE)>0.5))
(oob_accuracy<-mean(oob_pred_vote==heart_train$OUTPUT[as.numeric(oob_pred_df$ID)],na.rm=TRUE))

# Test Accuracy

get_1test_bag <- function(sample_indices) {
     df <- heart_test; 
     df$ID <- row.names(df); 
     if (length(unique(heart_train[sample_indices,]$ECG)) < 3) 
         df[df$ECG == 1,"ECG"] = NA; 
     return(df)
 }

test_bags <- apply(sample_vectors,2, get_1test_bag)
test_predictions<-mapply(glm_predictions, models, test_bags, 1 : M, SIMPLIFY = F)
test_pred_df <- Reduce(function(x, y) merge(x, y, by="ID",all=T), test_predictions)
test_pred_vote<-apply(test_pred_df[,-1],1,function(x) as.numeric(mean(x,na.rm=TRUE)>0.5))
(test_accuracy<-mean(test_pred_vote==heart_test[test_pred_df$ID,"OUTPUT"],na.rm=TRUE))

heart_bagger <- function(M) {

	seeds <- 70000 : (70000 + M - 1)
	sample_vectors<-sapply(seeds,function(x) { set.seed(x); return(sample(n,n,replace=T)) })
	models <- apply(sample_vectors, 2, train_1glm)

	bags<-apply(sample_vectors, 2, get_1bag)
	training_predictions <- mapply(glm_predictions,models,bags,1:M,SIMPLIFY=F)
	train_pred_df <- Reduce(function(x, y) merge(x, y, by = "ID", all = T), training_predictions)
	train_pred_vote<-apply(train_pred_df[,-1],1,function(x) as.numeric(mean(x,na.rm=TRUE)>0.5))
	training_accuracy<-mean(train_pred_vote==heart_train$OUTPUT[as.numeric(train_pred_df$ID)])

	oo_bags <- apply(sample_vectors,2, get_1oo_bag)
	oob_predictions<-mapply(glm_predictions, models, oo_bags, 1:M,SIMPLIFY=F)
	oob_pred_df <- Reduce(function(x, y) merge(x, y, by="ID", all=T), oob_predictions)
	oob_pred_vote<-apply(oob_pred_df[,-1], 1, function(x) as.numeric(mean(x,na.rm=TRUE)>0.5))
	oob_accuracy<-mean(oob_pred_vote==heart_train$OUTPUT[as.numeric(oob_pred_df$ID)],na.rm=TRUE)

	test_bags <- apply(sample_vectors,2, get_1test_bag)
	test_predictions<-mapply(glm_predictions, models, test_bags, 1 : M, SIMPLIFY = F)
	test_pred_df <- Reduce(function(x, y) merge(x, y, by="ID",all=T), test_predictions)
	test_pred_vote<-apply(test_pred_df[,-1],1,function(x) as.numeric(mean(x,na.rm=TRUE)>0.5))
	test_accuracy<-mean(test_pred_vote==heart_test[test_pred_df$ID,"OUTPUT"],na.rm=TRUE)

	return (c(M, training_accuracy, oob_accuracy, test_accuracy))

}

heart_bagger_df <- as.data.frame(t(sapply(c(11,51,101,501,1001),heart_bagger)))
names(heart_bagger_df) <- c("M", "Training Accuracy", "Out-of-bag Accuracy", "Test Accuracy")

#########################################
# Magic Telescope Data
#########################################

magic <- read.csv("magic04.data", header=FALSE)
names(magic)=c("FLENGTH", "FWIDTH", "FSIZE", "FCONC", "FCONC1", "FASYM", "FM3LONG", "FM3TRANS", "FALPHA", "FDIST", "CLASS")
magic$CLASS = as.factor(ifelse(magic$CLASS=='g',1,-1))

# Split the data

set.seed(33711209)
magic_sampling_vector <- createDataPartition(magic$CLASS, p = 0.80, list = FALSE)
magic_train <- magic[magic_sampling_vector,1:10]
magic_train_output <- magic[magic_sampling_vector,11]
magic_test <- magic[-magic_sampling_vector,1:10]
magic_test_output <- magic[-magic_sampling_vector,11]

# Pre Process Inputs
magic_pp <- preProcess(magic_train, method = c("center", "scale"))
magic_train_pp <- predict(magic_pp, magic_train)
magic_train_df_pp <- cbind(magic_train_pp, CLASS = magic_train_output)
magic_test_pp <- predict(magic_pp, magic_test)

library(nnet)
n_model <- nnet(CLASS~., data = magic_train_df_pp, size = 1)
n_test_predictions = predict(n_model, magic_test_pp, type = "class")
(n_test_accuracy <- mean(n_test_predictions == magic_test_output))

AdaBoostNN <- function(training_data, output_column, M, hidden_units) {
  require("nnet")
  models<-list()
  alphas<-list()
  n <- nrow(training_data)
  model_formula <- as.formula(paste(output_column,'~.',sep=''))
  w <- rep((1/n),n)
  for (m in 1:M) {
    model<-nnet(model_formula,data=training_data,size=hidden_units, weights=w)
    models[[m]]<-model
    predictions<-as.numeric(predict(model,training_data[,-which(names(training_data) == output_column)],type = "class"))
    errors<-predictions!=training_data[,output_column]
    error_rate<-sum(w*as.numeric(errors))/sum(w)
    alpha<-0.5*log((1-error_rate)/error_rate)
    alphas[[m]]<-alpha
    temp_w<-mapply(function(x,y) if (y) {x*exp(alpha)} else {x*exp(-alpha)},w,errors)
    w<-temp_w/sum(temp_w)
  }
  return(list(models=models, alphas=unlist(alphas)))
}

AdaBoostNN.predict<-function(ada_model,test_data) {
  models<-ada_model$models
  alphas<-ada_model$alphas
  prediction_matrix<-sapply(models,function (x) as.numeric(predict(x,test_data,type = "class")))
  weighted_predictions<-t(apply(prediction_matrix,1,function (x) mapply(function(y,z) y*z,x,alphas)))
  final_predictions<-apply(weighted_predictions,1,function(x) sign(sum(x)))
  return(final_predictions)
}

ada_model <- AdaBoostNN(magic_train_df_pp, 'CLASS', 10, 1)
predictions <- AdaBoostNN.predict(ada_model, magic_test_pp)
mean(predictions == magic_test_output)

# Experiment with different values of hidden units and number of models
magic_boost <- expand.grid(M = c(5, 10, 20, 50, 100, 200), hidden_units = c(1, 3))
magic_boost$accuracy <- mapply(function(x,y) { ada_model <- AdaBoostNN(magic_train_df_pp, 'CLASS', x, y); predictions <- AdaBoostNN.predict(ada_model, magic_test_pp); mean(predictions == magic_test_output) }, magic_boost$M, magic_boost$hidden_units )
magic_boost$hidden_units <- factor(magic_boost$hidden_units)

library(ggplot2)
p <- ggplot(data = magic_boost, aes(x = M, y = accuracy, shape = hidden_units, linetype = hidden_units))
p <- p + geom_line()
p <- p + geom_point(size=3)
p <- p + ggtitle("Accuracy of Boosted Neural Networks on the Magic Telescope Data")
p <- p + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2), legend.position="bottom")
p <- p + xlab("Number of Models")  
p <- p + ylab("Accuracy")
p <- p + scale_linetype_manual(name = "Hidden Units", values = c(1,2))
p <- p + scale_shape_manual(name = "Hidden Units", values = c(15,16))
p

#########################################
# Skillcraft
#########################################

skillcraft <- read.csv("SkillCraft1_Dataset.csv")

skillcraft<-skillcraft[-1]
skillcraft$TotalHours=as.numeric(levels(skillcraft$TotalHours))[skillcraft$TotalHours]
skillcraft$HoursPerWeek=as.numeric(levels(skillcraft$HoursPerWeek))[skillcraft$HoursPerWeek]
skillcraft$Age=as.numeric(levels(skillcraft$Age))[skillcraft$Age]
skillcraft<-skillcraft[complete.cases(skillcraft),]

library(caret)
set.seed(133)
skillcraft_sampling_vector <- createDataPartition(skillcraft$LeagueIndex, p = 0.80, list = FALSE)
skillcraft_train <- skillcraft[skillcraft_sampling_vector,]
skillcraft_test <- skillcraft[-skillcraft_sampling_vector,]

# From Chapter 6
compute_SSE <- function(correct,predictions) {
  return(sum((correct-predictions)^2))
}

library("gbm")
boostedtree <- gbm(LeagueIndex~., data=skillcraft_train, distribution="gaussian", n.trees=10000, shrinkage=0.1)
best.iter <- gbm.perf(boostedtree,method="OOB")
boostedtree_predictions = predict(boostedtree, skillcraft_test,best.iter)
(boostedtree_SSE <- compute_SSE(boostedtree_predictions, skillcraft_test$LeagueIndex))

#########################################
# Random Forests
#########################################

library("randomForest")
library("e1071")
rf_ranges <- list(ntree = c(500, 1000, 1500, 2000), mtry = 3:8)
rf_tune <- tune(randomForest, LeagueIndex~., data = skillcraft_train, ranges = rf_ranges)

rf_tune$best.parameters
rf_best <- rf_tune$best.model

rf_best_predictions = predict(rf_best,skillcraft_test)
(rf_best_SSE <- compute_SSE(rf_best_predictions, skillcraft_test$LeagueIndex))

par(mai=c(1.02,2.5,0.82,0.42))
rf_imp <- importance(rf_best)
rf_imp <- as.data.frame(rf_imp)[order(-rf_imp),,drop=F]
n<-rownames(rf_imp)
barplot(t(rf_imp),horiz=T,las=1,main="Variable Importance in Random Forest", names=n, col = "gray")
