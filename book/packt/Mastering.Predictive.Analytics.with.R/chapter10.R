##########################
# Symmetric
##########################

library(ggplot2)
library(gtools)
k = 5
alphas<- c(0.1,1,10)
number_of_samples<-4
dims<-number_of_samples*length(alphas)
set.seed(1124234)
my_layout <- matrix(1:dims,number_of_samples,length(alphas),byrow = F)
dir_draws<-lapply(alphas, function(x) rdirichlet(number_of_samples,rep(x,k)))
plotDirichletPlot <- function(y, my_alpha="") {
	p1 <- ggplot(data=data.frame(y), aes(x = 1:k, y = y))
	p1 <- p1 + geom_bar(stat="identity")
	p1 <- p1 + ggtitle(paste("Alpha = ",my_alpha," (Symmetric)", sep = ""))
	p1 <- p1 + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2))
	p1 <- p1 + xlab("Topics")  
	p1 <- p1 + ylab("Probability") 
	return (p1)
}
plotlist <- mapply(function(x,y) apply(x,1,function(z) plotDirichletPlot(z,y)), dir_draws, alphas, SIMPLIFY = F)
plotlist <- Reduce(c, plotlist)

##########################
# Skewed
##########################

k = 5
alphas<- c(0.1,1,10)
number_of_samples<-4
dims<-number_of_samples*length(alphas)
set.seed(1124234)
my_layout <- matrix(1:dims,number_of_samples,length(alphas),byrow = F)
dir_draws<-lapply(alphas, function(x) rdirichlet(number_of_samples,c((k-1)*x,rep(x/(k-1),k-1))))
plotSkewedDirichletPlot <- function(y, my_alpha="") {
	p1 <- ggplot(data=data.frame(y), aes(x = 1:k, y = y))
	p1 <- p1 + geom_bar(stat="identity")
	p1 <- p1 + ggtitle(paste("Alpha = ",my_alpha," (Skewed)", sep = ""))
	p1 <- p1 + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2))
	p1 <- p1 + xlab("Topics")  
	p1 <- p1 + ylab("Probability") 
	return (p1)
}
plotlist <- mapply(function(x,y) apply(x,1,function(z) plotSkewedDirichletPlot(z,y)), dir_draws, alphas, SIMPLIFY = F)
plotlist <- Reduce(c, plotlist)

##########################
# BBC Data
##########################

bbc_folder<-"bbc/"
bbcsports_folder<-"bbcsport/"

bbc_source <- paste(bbc_folder,"bbc.mtx",sep="")
bbc_source_terms<- paste(bbc_folder,"bbc.terms",sep="")
bbc_source_docs <- paste(bbc_folder,"bbc.docs",sep="")

bbcsports_source <- paste(bbcsports_folder,"bbcsport.mtx",sep="")
bbcsports_source_terms<- paste(bbcsports_folder,"bbcsport.terms",sep="")
bbcsports_source_docs <- paste(bbcsports_folder,"bbcsport.docs",sep="")

library("tm")
library("Matrix")

bbc_matrix<-readMM(bbc_source)
bbc_tdm<-as.TermDocumentMatrix(bbc_matrix,weightTf)

bbcsports_matrix<-readMM(bbcsports_source)
bbcsports_tdm<-as.TermDocumentMatrix(bbcsports_matrix,weightTf)

bbc_rows<-scan(bbc_source_terms,what="character")
bbc_cols<-scan(bbc_source_docs,what="character")
bbc_tdm$dimnames$Terms<-bbc_rows
bbc_tdm$dimnames$Docs<-bbc_cols
(bbc_dtm <- t(bbc_tdm))

bbcsports_rows<-scan(bbcsports_source_terms,what="character")
bbcsports_cols<-scan(bbcsports_source_docs,what="character")
bbcsports_tdm$dimnames$Terms<-bbcsports_rows
bbcsports_tdm$dimnames$Docs<-bbcsports_cols
(bbcsports_dtm <- t(bbcsports_tdm))

bbc_gold_topics<-sapply(bbc_cols,function(x) substr(x, 1, nchar(x)-4))
bbc_gold_factor<-factor(bbc_gold_topics)
summary(bbc_gold_factor)

bbcsports_gold_topics<-sapply(bbcsports_cols,function(x) substr(x, 1, nchar(x)-4))
bbcsports_gold_factor<-factor(bbcsports_gold_topics)
summary(bbcsports_gold_factor)

compute_model_list <- function (k, topic_seed, myDtm){
  LDA_VEM <- LDA(myDtm, k = k, control = list(seed = topic_seed))
  LDA_VEM_a <- LDA(myDtm, k = k, control = list(estimate.alpha = FALSE, seed = topic_seed))
  LDA_GIB <- LDA(myDtm, k = k, method = "Gibbs", control = list(seed = topic_seed, burnin = 1000, thin = 100, iter = 1000))
  CTM_VEM <- CTM(myDtm, k = k, control = list(seed = topic_seed, var = list(tol = 10^-4), em = list(tol = 10^-3)))
  return(list(LDA_VEM=LDA_VEM, LDA_VEM_a=LDA_VEM_a, LDA_GIB=LDA_GIB, CTM_VEM=CTM_VEM))
}

library("topicmodels")
k <- 5
topic_seed<-5798252
bbc_models<-compute_model_list(k, topic_seed,bbc_dtm)
bbcsports_models<-compute_model_list(k, topic_seed,bbcsports_dtm)

model_topics <- topics(bbc_models$LDA_VEM)
table(model_topics,bbc_gold_topics)

model_topics <- topics(bbc_models$LDA_GIB)
table(model_topics,bbc_gold_topics)

##########################
# Model Accuracies
##########################

compute_topic_model_accuracy <- function(model,gold_factor) {
  model_topics <- topics(model)
  model_table<-table(model_topics,gold_factor)
  model_matches<-apply(model_table,1,max)
  model_accuracy<-sum(model_matches) / sum(model_table)
  return(model_accuracy)
}

sapply(bbc_models, function(x) compute_topic_model_accuracy(x,bbc_gold_factor))
sapply(bbcsports_models, function(x) compute_topic_model_accuracy(x,bbcsports_gold_factor))

##########################
# Log Likelihoods
##########################

sapply(bbc_models, logLik)
sapply(bbcsports_models, logLik)

##########################
# Seeding
##########################

seeded_bbc_models<-lapply(5798252:5798256, function(x) compute_model_list(k,x,bbc_dtm))
seeded_bbcsports_models<-lapply(5798252:5798256, function(x) compute_model_list(k,x,bbcsports_dtm))

##########################
# Accuracy in Seeding
##########################

seeded_bbc_models_acc<-sapply(seeded_bbc_models, function(x) sapply(x, function(y) compute_topic_model_accuracy(y,bbc_gold_factor)))
seeded_bbcsports_models_acc<-sapply(seeded_bbcsports_models, function(x) sapply(x, function(y) compute_topic_model_accuracy(y,bbcsports_gold_factor)))

##########################
# Revised function with nstart
##########################

compute_model_list_r <- function (k, topic_seed, myDtm, nstart){
  seed_range <- topic_seed:(topic_seed+nstart-1)
  LDA_VEM <- LDA(myDtm, k = k, control = list(seed = seed_range, nstart = nstart))
  LDA_VEM_a <- LDA(myDtm, k = k, control = list(estimate.alpha = FALSE, seed = seed_range, nstart = nstart))
  LDA_GIB <- LDA(myDtm, k = k, method = "Gibbs", control = list(seed = seed_range, burnin = 1000, thin = 100, iter = 1000, nstart = nstart))
  CTM_VEM <- CTM(myDtm, k = k, control = list(seed = seed_range, var = list(tol = 10^-4), em = list(tol = 10^-3), nstart = nstart))
  return(list(LDA_VEM=LDA_VEM, LDA_VEM_a=LDA_VEM_a, LDA_GIB=LDA_GIB, CTM_VEM=CTM_VEM))
}

nstart=5
k = 5
topic_seed<-5798252
nstarted_bbc_models_r<-compute_model_list_r(k,topic_seed,bbc_dtm,nstart)
nstarted_bbcsports_models_r<-compute_model_list_r(k,topic_seed,bbcsports_dtm,nstart)

sapply(nstarted_bbc_models_r, function(x) compute_topic_model_accuracy(x,bbc_gold_factor))
sapply(nstarted_bbcsports_models_r, function(x) compute_topic_model_accuracy(x,bbcsports_gold_factor))

##########################
# Topic Distributions
##########################

plot_topic_model_posterior_max <- function(model,title) {
  topic_posterior <- posterior(model)$topics
  max_topics <- data.frame(best_topic = apply(topic_posterior,1,max))
  p1 <- qplot(best_topic, data=max_topics, geom="histogram")
  p1 <- p1 + ggtitle(paste("Histogram of Max Probabilities\n for Model",title))
  p1 <- p1 + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2))
  p1 <- p1 + xlab("Probability of Most Likely Topic")  
  p1 <- p1 + ylab("Frequency") 
  return (p1)
  #hist(apply(topic_posterior,1,max),xlab="Probability of Most Likely Topic", main=paste("Histogram of Max Probabilities\n for Model",title))
  #return
}
my_layout <- matrix(1:4,2,2)
my_plotlist <- mapply(plot_topic_model_posterior_max, bbc_models, names(bbc_models), SIMPLIFY = F)

##########################
# Entropy
##########################

compute_entropy <- function(probs) {
  return(- sum(probs * log(probs)))
}

compute_model_mean_entropy <- function(model) {
  topics <- posterior(model)$topics
  return(mean(apply(topics, 1, compute_entropy)))
}

sapply(bbc_models, compute_model_mean_entropy)
sapply(bbcsports_models, compute_model_mean_entropy)

##########################
# Word Clouds
##########################

GIB_bbc_model<-bbc_models[[3]]
terms(GIB_bbc_model,5)

plot_wordcloud<-function(model, myDtm, index, numTerms) {
  
  model_terms<-terms(model,numTerms)
  model_topics<-topics(model)
  
  terms_i<-model_terms[,index]
  topic_i<-model_topics==index
  dtm_i<-myDtm[topic_i,terms_i]
  frequencies_i<-colSums(as.matrix(dtm_i))
  wordcloud(terms_i,frequencies_i,min.freq=0)
}

library(wordcloud)
par(oma=c(2.5,2.5,3.5,1.5))
layout(matrix(1:6,2,3))
lapply(1:5, function(x) { plot_wordcloud(bbc_models[[3]], bbc_dtm, x, 25); title(paste("Topic ",x), cex=1.5)})
title("Word Clouds for 25 Most Frequent Terms using LDA_GIB on the BBC Dataset", outer=T, cex = 1.5)
