#########################################
# Graph Views
#########################################

library("igraph")
adjacency_m = matrix(0,7,7)
adjacency_m[1,6]=1
adjacency_m[2,1]=1
adjacency_m[3,7]=1
adjacency_m[4,3]=1
adjacency_m[4,5]=1
adjacency_m[4,7]=1
adjacency_m[6,4]=1
adjacency_m[6,5]=1
adjacency_m[6,7]=1
adjacency_m[7,5]=1
colnames(adjacency_m)<-1:7
rownames(adjacency_m)<-1:7
g <- graph.adjacency(adjacency_m)
par(oma=c(0,0,0,0), mai= c(0,0,0,0))
par(mfrow=c(1,2)); plot(g); plot(g)

adjacency_m=matrix(0,3,3)
adjacency_m[2,1]=1
adjacency_m[2,3]=1
bayex<-c("Job Offer", "Undergraduate Degree Performance", "Graduate School Offer")
colnames(adjacency_m)<-bayex
rownames(adjacency_m)<-bayex
g<- graph.adjacency(adjacency_m)
plot(g)

adjacency_m=matrix(0,8,8)
adjacency_m[1,2]=1
adjacency_m[1,3]=1
adjacency_m[1,4]=1
adjacency_m[1,5]=1
adjacency_m[1,6]=1
adjacency_m[1,7]=1
adjacency_m[1,8]=1
bayex<-c("Sentiment", "bad", "good", "fun", "boring", "exciting", "sad", "adventure")
colnames(adjacency_m)<-bayex
rownames(adjacency_m)<-bayex
g<- graph.adjacency(adjacency_m)
plot(g, vertex.label.dist=0.7)

#########################################
# Sentiment Analysis
#########################################

# Set these paths to where you downloaded the files
path_to_neg_folder<-"aclImdb/train/neg"
path_to_pos_folder<-"aclImdb/train/pos"

library("tm")
nb_pos <-Corpus(DirSource(path_to_pos_folder), readerControl = list(language="en"))
nb_neg <-Corpus(DirSource(path_to_neg_folder), readerControl = list(language="en"))
nb_all=c(nb_pos,nb_neg,recursive=T)

meta(nb_all[[1]])

ids<-sapply(1:length(nb_all),function(x) meta(nb_all[[x]],"id"))
head(ids)

scores<-as.numeric(sapply(ids,function(x) sub("[0-9]+_([0-9]+)\\.txt","\\1",x)))
scores<-factor(ifelse(scores>=7,"positive","negative"))
summary(scores)

nb_all <- tm_map(nb_all, content_transformer(removeNumbers))
nb_all <- tm_map(nb_all, content_transformer(removePunctuation))
nb_all <- tm_map(nb_all, content_transformer(tolower))
nb_all <- tm_map(nb_all, content_transformer(removeWords), stopwords("english"))
nb_all <- tm_map(nb_all, content_transformer(stripWhitespace))

nb_dtm <-DocumentTermMatrix(nb_all)
dim(nb_dtm)

nb_dtm<-removeSparseTerms(x=nb_dtm, sparse = 0.99)
dim(nb_dtm)

nb_dtm <- weightBin(nb_dtm)

inspect(nb_dtm[10:16,1:6])

nb_df <- as.data.frame(as.matrix(nb_dtm))
library(caret)
set.seed(443452342)
nb_sampling_vector <- createDataPartition(scores, p = 0.80, list = FALSE)
nb_df_train <- nb_df[nb_sampling_vector,]
nb_df_test <- nb_df[-nb_sampling_vector,]
scores_train = scores[nb_sampling_vector]
scores_test = scores[-nb_sampling_vector]

library("e1071")
nb_model <- naiveBayes(nb_df_train, scores_train)

nb_train_predictions <- predict(nb_model, nb_df_train)
mean(nb_train_predictions == scores_train)
table(actual = scores_train, predictions = nb_train_predictions)

nb_test_predictions <- predict(nb_model, nb_df_test)
mean(nb_test_predictions == scores_test)
table(actual = scores_test, predictions = nb_test_predictions)

nb_all <- tm_map(nb_all, stemDocument, language = "english")
nb_dtm <- DocumentTermMatrix(nb_all) 
nb_dtm <- removeSparseTerms(x=nb_dtm, sparse = 0.99)
nb_dtm <- weightBin(nb_dtm)
nb_df <- as.data.frame(as.matrix(nb_dtm))
nb_df_train <- nb_df[nb_sampling_vector,]
nb_df_test <- nb_df[-nb_sampling_vector,]

nb_model_stem <- naiveBayes(nb_df_train, scores_train)
nb_test_predictions_stem <- predict(nb_model_stem, nb_df_test)
mean(nb_test_predictions_stem == scores_test)
table(actual = scores_test, predictions = nb_test_predictions_stem)

# Note: Recompute the nb_dtm without stemming before running the next bit
nb_model_laplace <- naiveBayes(nb_df_train, scores_train, laplace=10)
nb_test_predictions_laplace <- predict(nb_model_laplace, nb_df_test)
mean(nb_test_predictions_laplace == scores_test)
table(actual = scores_test, predictions = nb_test_predictions_laplace)

#########################################
# Gene Sequencing
#########################################

promoters <- read.csv("promoters.data", header=F, dec=",", strip.white=TRUE, stringsAsFactors = FALSE)
promoters[1,]

positive_observations <- subset(promoters, V1=='+', 3)
negative_observations <- subset(promoters, V1=='-', 3)

positive_observations<-sapply(positive_observations, function(x) paste("S",x,"X",sep=""))
negative_observations<-sapply(negative_observations, function(x) paste("S",x,"X",sep=""))
positive_observations[1]

positive_observations<-strsplit(positive_observations,"")
negative_observations<-strsplit(negative_observations,"")
head(positive_observations[1])

states <- c("S", "X", "a", "c", "g", "t")
symbols <- c("S", "X", "a", "c", "g", "t")
startingProbabilities<-c(1,0,0,0,0,0)
emissionProbabilities<-diag(6)
colnames(emissionProbabilities)<-states
rownames(emissionProbabilities)<-symbols
emissionProbabilities

calculateTransitionProbabilities <- function(data, states) {

	transitionProbabilities<-matrix(0,length(states),length(states))
	colnames(transitionProbabilities)<-states
	rownames(transitionProbabilities)<-states

	for (index in 1:(length(data)-1)) {
		current_state <- data[index]
		next_state <- data[index+1]
		transitionProbabilities[current_state, next_state] <- transitionProbabilities[current_state, next_state] + 1
	}
	
	transitionProbabilities<-sweep(transitionProbabilities, 1, rowSums(transitionProbabilities), FUN="/")
	return(transitionProbabilities)
}

negative_observation<-Reduce(function(x,y) c(x, y), negative_observations, c())
(transitionProbabilitiesNeg  <- calculateTransitionProbabilities(negative_observation, states))

library("HMM")
negative_hmm = initHMM(states, symbols, startProbs=startingProbabilities, transProbs=transitionProbabilitiesNeg, emissionProbs=emissionProbabilities)

incorrect<-0
for (obs in 1:length(positive_observations)) {

positive_observation<-Reduce(function(x,y) c(x, y), positive_observations[-obs], c())
transitionProbabilitiesPos  <- calculateTransitionProbabilities(positive_observation, states)
positive_hmm = initHMM(states, symbols, startProbs=startingProbabilities, transProbs=transitionProbabilitiesPos, emissionProbs=emissionProbabilities)

test_observation<-positive_observations[[obs]]
final_index<-length(test_observation)

pos_probs<-exp(forward(positive_hmm,test_observation))
neg_probs<-exp(forward(negative_hmm,test_observation))

pos_seq_prob<-sum(pos_probs[,final_index])
neg_seq_prob<-sum(neg_probs[,final_index])

if (pos_seq_prob<neg_seq_prob) incorrect<-incorrect+1

}

positive_observation <- Reduce(function(x,y) c(x, y), positive_observations, c())
transitionProbabilitiesPos  <- calculateTransitionProbabilities(positive_observation, states)
positive_hmm = initHMM(states, symbols, startProbs=startingProbabilities, transProbs=transitionProbabilitiesPos, emissionProbs=emissionProbabilities)

for (obs in 1:length(negative_observations)) {

negative_observation<-Reduce(function(x,y) c(x, y), negative_observations[-obs], c())
transitionProbabilitiesNeg <- calculateTransitionProbabilities(negative_observation, states)
negative_hmm = initHMM(states, symbols, startProbs=startingProbabilities, transProbs=transitionProbabilitiesNeg, emissionProbs=emissionProbabilities)

test_observation<-negative_observations[[obs]]
final_index<-length(test_observation)

pos_probs<-exp(forward(positive_hmm,test_observation))
neg_probs<-exp(forward(negative_hmm,test_observation))

pos_seq_prob<-sum(pos_probs[,final_index])
neg_seq_prob<-sum(neg_probs[,final_index])

if (pos_seq_prob>neg_seq_prob) incorrect<-incorrect+1

}

#########################################
# English Letter Patterns
#########################################

library(ggplot2)
library("tm")
nb_pos <-Corpus(DirSource(path_to_pos_folder), readerControl = list(language="en"))
nb_neg <-Corpus(DirSource(path_to_neg_folder), readerControl = list(language="en"))
nb_all=c(nb_pos,nb_neg,recursive=T)
nb_all <- tm_map(nb_all, content_transformer(tolower))

texts <- sapply(1 : length(nb_all), function(x) nb_all[[x]])

texts<-sapply(texts, function(x) gsub("\\s","W", x))
texts<-sapply(texts, function(x) gsub("[0-9]","N", x))
texts<-sapply(texts, function(x) gsub("[[:punct:]]","P", x))
texts<-sapply(texts, function(x) gsub("[^a-zWNP]","O", x))

big_text_splits<-lapply(texts[1:40], function(x) strsplit(x, ""))
big_text_splits<-unlist(big_text_splits, use.names = F)

states <- c("s1", "s2", "s3")
numstates <- length(states)
symbols <- c(letters,"W","N","P","O")
numsymbols <- length(symbols)

set.seed(124124)
startingProbabilities <- matrix(runif(numstates), 1, numstates)
startingProbabilities<-sweep(startingProbabilities, 1, rowSums(startingProbabilities), FUN="/")
set.seed(454235)
transitionProbabilities<-matrix(runif(numstates*numstates),numstates,numstates)
transitionProbabilities<-sweep(transitionProbabilities, 1, rowSums(transitionProbabilities), FUN="/")
set.seed(923501)
emissionProbabilities<-matrix(runif(numstates*numsymbols),numstates,numsymbols)
emissionProbabilities<-sweep(emissionProbabilities, 1, rowSums(emissionProbabilities), FUN="/")

library("HMM")
hmm <- initHMM(states, symbols, startProbs = startingProbabilities, transProbs = transitionProbabilities, emissionProbs = emissionProbabilities)
hmm_trained <- baumWelch(hmm, big_text_splits)

p1 <- ggplot(data=data.frame(hmm_trained$hmm$emissionProbs[1,]), aes(x = names(hmm_trained$hmm$emissionProbs[1,]), y = hmm_trained$hmm$emissionProbs[1,]))
p1 <- p1 + geom_bar(stat="identity")
p1 <- p1 + ggtitle("Symbol Emission Probabilities for State 1")
p1 <- p1 + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2))
p1 <- p1 + xlab("State")  
p1 <- p1 + ylab("Emission Probability") 
p1
 
p2 <- ggplot(data=data.frame(hmm_trained$hmm$emissionProbs[2,]), aes(x = names(hmm_trained$hmm$emissionProbs[2,]), y = hmm_trained$hmm$emissionProbs[2,]))
p2 <- p2 + geom_bar(stat="identity")
p2 <- p2 + ggtitle("Symbol Emission Probabilities for State 2")
p2 <- p2 + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2))
p2 <- p2 + xlab("State")  
p2 <- p2 + ylab("Emission Probability") 
p2

p3 <- ggplot(data=data.frame(hmm_trained$hmm$emissionProbs[3,]), aes(x = names(hmm_trained$hmm$emissionProbs[3,]), y = hmm_trained$hmm$emissionProbs[3,]))
p3 <- p3 + geom_bar(stat="identity")
p3 <- p3 + ggtitle("Symbol Emission Probabilities for State 3")
p3 <- p3 + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2))
p3 <- p3 + xlab("State")  
p3 <- p3 + ylab("Emission Probability") 
p3

(trained_transition_probabilities<-hmm_trained$hmm$transProbs)

set.seed(9898)
simHMM(hmm_trained$hmm, 30)

library("markovchain")
simpleMc<-new("markovchain", states=c("s1","s2","s3"), transitionMatrix=trained_transition_probabilities, name="simpleMc")
steadyStates(simpleMc)
