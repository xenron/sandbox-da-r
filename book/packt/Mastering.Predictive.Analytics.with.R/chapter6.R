#########################################
# Plot 1
#########################################

library(ggplot2)
polygon_id <-  rep(factor(1:3), each = 4)
fill_value <- rep(factor(c(0.3,0.5,0.7)), each = 4)
xcoords <- c(-5,-5,46,46,46,46,105,105,-5,-5,105,105)
ycoords <- c(23,105,105,23,23,105,105,23,-5,23,23,-5)
polygon_data <- data.frame(id = polygon_id, value = fill_value, x = xcoords, y = ycoords)

p <- ggplot(polygon_data, aes(x=x, y=y))
p <- p + ggtitle("2D Feature Space")
p <- p + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2), legend.position="bottom")
p <- p + xlab("x1")  
p <- p + ylab("x2") 
p <- p + geom_polygon(aes(fill=value, group=id)) 
p <- p + coord_cartesian(xlim = c(-5, 105), ylim = c(-5,105))
p <- p + guides(fill=FALSE)
p <- p + scale_fill_grey(start = 0.3, end = 0.7)
p <- p + annotate("text", x = 23, y = 64, label = "1.2", colour = "white", size = 12)
p <- p + annotate("text", x = 69, y = 64, label = "-3.7", colour = "white", size = 12)
p <- p + annotate("text", x = 50, y = 11.5, label = "2.1", colour = "white", size = 12)
p

#########################################
# SSE Splits
#########################################

compute_SSE_split <- function(v, y, split_point) {
  index<-v<split_point
  y1<-y[index]
  y2<-y[!index]
  SSE<-sum((y1-mean(y1))^2) + sum((y2-mean(y2))^2)
  return(SSE)
}

compute_all_SSE_splits <- function(v, y) {
  sapply(unique(v), function(sp) compute_SSE_split(v,y,sp))
}

set.seed(99)
x1<-rbinom(20,1,0.5)
set.seed(100)
x2<-round(10+rnorm(20,5,5),2)
set.seed(101)
y<-round((1+(x2*2/5)+x1-rnorm(20,0,3)),2)
rcart_df<-data.frame(x1,x2,y)
rcart_df

#########################################
# SSE Plot
#########################################

x1splits<-compute_all_SSE_splits(x1,y)
x2splits<-compute_all_SSE_splits(x2,y)

p1 <- ggplot(data = NULL, aes(x = unique(x1), y = x1splits))
p1 <- p1 + geom_point(shape=4)
p1 <- p1 + ggtitle("SSE values for Splits on Feature x1")
p1 <- p1 + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2), legend.position="bottom")
p1 <- p1 + xlab("x1")  
p1 <- p1 + ylab("SSE")
p1 <- p1 + coord_cartesian(xlim = c(-0.1,1.1), ylim = c(225,235))
p1 <- p1 + annotate("text", x = unique(x1), y = x1splits, label = round(x1splits,2), vjust = -1, hjust = 1, size = 3)
p1

p2 <- ggplot(data = NULL, aes(x = unique(x2), y = x2splits))
p2 <- p2 + geom_point(shape=4)
p2 <- p2 + ggtitle("SSE values for Splits on Feature x2")
p2 <- p2 + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2), legend.position="bottom")
p2 <- p2 + xlab("x2")  
p2 <- p2 + ylab("SSE")
p2 <- p2 + coord_cartesian(xlim = c(10,30), ylim = c(110,250))
p2 <- p2 + annotate("text", x = unique(x2), y = x2splits, label = round(x2splits,2), vjust = -1, hjust = 1, size = 3)
p2

#########################################
# Gini Index and Entropy
#########################################

gini_index <- function(v) {
  t <- table(v)
  probs <- t / sum(t)
  terms <- sapply(probs,function(p) p*(1-p) )
  return(sum(terms))
}

gini_binary <- function(p) {
  return(2 * p * (1 - p))
}

entropy_binary <- function(p) {
  return(-(p*log2(p) + (1-p)*log2(1-p)))
}

x <- seq(0,1,length=10000)
gini <- gini_binary(x)
entropy <- entropy_binary(x)

p <- ggplot()
p <- p + geom_line(data = NULL, aes(x = x, y = gini, lty = "Gini Index"))
p <- p + geom_line(data = NULL, aes(x = x, y = entropy, lty = "Entropy"))
p <- p + ggtitle("Splitting Criteria for Binary Classification")
p <- p + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2), legend.position="bottom")
p <- p + xlab("Probability of Class 1")  
p <- p + ylab("Splitting Criterion")
p <- p + scale_linetype_discrete(name = "Splitting Criterion")
p <- p + coord_cartesian(xlim = c(0,1), ylim = c(0,1))
p

#########################################
# Synthetic Data Set Generation
#########################################

seed<-4556339
set.seed(seed)
x1<-rnorm(75,mean=16,sd=3)
seed<-seed+1
set.seed(seed)
y1<-rnorm(75,mean=14,sd=4)
df1<-data.frame(x1=x1, x2=y1,class='a')

seed<-seed+1
set.seed(seed)
x1<-rnorm(36,mean=22,sd=8)
seed<-seed+1
set.seed(seed)
y1<-rnorm(36,mean=84,sd=6)
df2<-data.frame(x1=x1, x2=y1,class='b')

seed<-seed+1
set.seed(seed)
x1<-rnorm(50,mean=49,sd=4)
seed<-seed+1
set.seed(seed)
y1<-rnorm(50,mean=72,sd=2)
df3<-data.frame(x1=x1, x2=y1,class='c')

seed<-seed+1
set.seed(seed)
x1<-rnorm(54,mean=68,sd=3)
seed<-seed+1
set.seed(seed)
y1<-rnorm(54,mean=39,sd=6)
df4<-data.frame(x1=x1, x2=y1,class='b')

seed<-seed+1
set.seed(seed)
x1<-rnorm(43,mean=77,sd=4)
seed<-seed+1
set.seed(seed)
y1<-rnorm(43,mean=86,sd=3)
df5<-data.frame(x1=x1, x2=y1,class='b')

seed<-seed+1
set.seed(seed)
x1<-rnorm(29,mean=88,sd=3.5)
seed<-seed+1
set.seed(seed)
y1<-rnorm(29,mean=10,sd=2.5)
df6<-data.frame(x1=x1, x2=y1,class='c')

mcdf<-rbind(df1,df2,df3,df4,df5,df6)

#########################################
# Banknote
#########################################

bnote <- read.csv("data_banknote_authentication.txt", header=FALSE)
names(bnote) <- c("waveletVar", "waveletSkew", "waveletCurt", "entropy", "class")
bnote$class <- factor(bnote$class)

library(caret)
set.seed(266)
bnote_sampling_vector <- createDataPartition(bnote$class, p = 0.80, list = FALSE)
bnote_train <- bnote[bnote_sampling_vector,]
bnote_test <- bnote[-bnote_sampling_vector,]

library(C50)
bnote_tree <- C5.0(class~.,data=bnote_train)

bnote_predictions <- predict(bnote_tree,bnote_test)
mean(bnote_test$class == bnote_predictions)

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

library(rpart)
regtree <- rpart(LeagueIndex~., data=skillcraft_train)

par(mar=c(2,1,1,1))
plot(regtree, uniform=TRUE, main="Regression Tree for Skillcraft Data Set")
text(regtree, use.n=FALSE, all=TRUE, cex=.8)

#########################################
# Performance
#########################################

compute_SSE <- function(correct,predictions) {
  return(sum((correct-predictions)^2))
}

regtree_predictions = predict(regtree, skillcraft_test)
(regtree_SSE <- compute_SSE(regtree_predictions, skillcraft_test$LeagueIndex))

regtree.random <- rpart(LeagueIndex~., data=skillcraft_train, control=rpart.control(minsplit=20, cp=0.001, maxdepth=10))
regtree.random_predictions = predict(regtree.random, skillcraft_test)
(regtree.random_SSE <- compute_SSE(regtree.random_predictions, skillcraft_test$LeagueIndex))

library(e1071)
rpart.ranges<-list(minsplit=seq(5,50,by=5), cp=c(0,0.001,0.002,0.005,0.01,0.02,0.05,0.1,0.2,0.5), maxdepth=1:10)
(regtree.tune<-tune(rpart,LeagueIndex~., data=skillcraft_train, ranges=rpart.ranges))

regtree.tuned <- rpart(LeagueIndex~., data=skillcraft_train, control=rpart.control(minsplit=35, cp=0.002, maxdepth=6))
regtree.tuned_predictions = predict(regtree.tuned, skillcraft_test)
(regtree.tuned_SSE <- compute_SSE(regtree.tuned_predictions, skillcraft_test$LeagueIndex))

plot(regtree.tuned, uniform=TRUE, main="Tuned Regression Tree for Skillcraft Data Set")
text(regtree.tuned, use.n=TRUE, all=TRUE, cex=.8)

par(mar=c(2.5,10,2,2))
barplot(regtree.tuned$variable.importance,horiz=T,las=1,main="Variable Importance in Tuned Regression Tree")

#########################################
# M5
#########################################

library("RWeka")
m5tree<-M5P(LeagueIndex~., data=skillcraft_train)
m5tree_predictions = predict(m5tree, skillcraft_test)
m5tree_SSE <- compute_SSE(m5tree_predictions, skillcraft_test$LeagueIndex)
m5tree_SSE
