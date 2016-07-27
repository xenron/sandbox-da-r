#########################################
# Restaurant Example
#########################################

library(ggplot2)

oliver   <- c(1,1,2,5,7,8,9,7)
thibault <- c(5,9,4,1,1,7,5,9)
maria    <- c(1,4,2,5,8,6,2,8)
pedro    <- c(2,6,7,2,6,1,8,9)
ines     <- c(1,3,2,4,8,9,7,7)
gertrude <- c(1,6,5,7,3,2,5,5)
ratingMatrix <- rbind(oliver, thibault, maria, pedro, ines, gertrude)

colnames(ratingMatrix) <- c("Berny's", "La Traviata", "El Pollo Loco", "Joey's Pizza", "The Old West", "Jake and Jill", "Full Moon", "Acropolis")
ratingMatrix

dist(ratingMatrix, method = 'euclidean')
dist(t(ratingMatrix), method = 'euclidean')

library("proxy")
dist(ratingMatrix, method='cosine')

binaryRatingMatrix <- ratingMatrix > 5
dist(binaryRatingMatrix, method = 'jaccard')

centered_rm <- t(apply(ratingMatrix, 1, function(x) x-mean(x)))
centered_rm

options(digits=2)
(rm_svd <- svd(ratingMatrix))

reconstructed_rm <- rm_svd$u %*% diag(rm_svd$d) %*% t(rm_svd$v)
reconstructed_rm

all.equal(ratingMatrix,reconstructed_rm, tolerance = 0.000001, check.attributes=F)

energy <- rm_svd$d ^ 2
cumsum(energy) / sum(energy)

d92 <- c(rm_svd$d[1:2], rep(0, length(rm_svd$d) - 2))
reconstructed92_rm <- rm_svd$u %*% diag(d92) %*% t(rm_svd$v)
reconstructed92_rm

#########################################
# Real World Data Sets
#########################################

library(data.table)
jester<-fread("jesterfinal151cols.csv", sep=",", header = F)
jester[,V1:=NULL]

jester_m <- as.matrix(jester) 
jester_m <- ifelse(jester_m==99,NA,jester_m)
library(recommenderlab)
jester_rrm <- as(jester_m, "realRatingMatrix")

movies <-fread("ml-1m/ratings.dat", sep=":", header = F)
movies[,c("V2","V4","V6","V7"):=NULL]
head(movies)

userid_factor <- as.factor(movies[,V1])
movieid_factor <- as.factor(movies[,V3])
movies_sm = sparseMatrix(i = as.numeric(userid_factor), j = as.numeric(movieid_factor), x = as.numeric(movies[,V5]))
movies_rrm <- new("realRatingMatrix", data = movies_sm)
colnames(movies_rrm) <-levels(movieid_factor)
rownames(movies_rrm) <- levels(userid_factor)
dim(movies_rrm)

#########################################
# Histograms for ratings
#########################################

jester_ratings <- getRatings(jester_rrm)
jester_normalized_ratings <- getRatings(normalize(jester_rrm, method="Z-score"))

p1 <- ggplot(data.frame(x=getRatings(jester_rrm)), aes(x))
p1 <- p1 + geom_histogram(binwidth=.25,colour="black", fill="white")
p1 <- p1 + ggtitle("Histogram of Raw Ratings\n(Jester)")
p1 <- p1 + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2))
p1 <- p1 + xlab("Rating")  
p1 <- p1 + ylab("Number of Jokes") 
p1

p2 <- ggplot(data.frame(x=getRatings(normalize(jester_rrm, method="Z-score"))), aes(x))
p2 <- p2 + geom_histogram(binwidth=.25,colour="black", fill="white")
p2 <- p2 + ggtitle("Histogram of Normalized Ratings\n(Jester)")
p2 <- p2 + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2))
p2 <- p2 + xlab("Rating")  
p2 <- p2 + ylab("Number of Jokes") 
p2

p3 <- ggplot(data.frame(x=getRatings(movies_rrm)), aes(x))
p3 <- p3 + geom_histogram(binwidth=.25,colour="black", fill="white")
p3 <- p3 + ggtitle("Histogram of Raw Ratings\n(Movielens)")
p3 <- p3 + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2))
p3 <- p3 + xlab("Rating")  
p3 <- p3 + ylab("Number of Movies") 
p3

p4 <- ggplot(data.frame(x=getRatings(normalize(movies_rrm, method="Z-score"))), aes(x))
p4 <- p4 + geom_histogram(binwidth=.25,colour="black", fill="white")
p4 <- p4 + ggtitle("Histogram of Normalized Ratings\n(Movielens)")
p4 <- p4 + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2))
p4 <- p4 + xlab("Rating")  
p4 <- p4 + ylab("Number of Movies") 
p4

#########################################
# Second batch of histograms for ratings
#########################################

jester_items_rated_per_user <- rowCounts(jester_rrm)
jester_average_item_rating_per_item  <- colMeans(jester_rrm)

p1 <- ggplot(data.frame(x=rowCounts(jester_rrm)), aes(x))
p1 <- p1 + geom_histogram(binwidth=1,colour="black", fill="white")
p1 <- p1 + ggtitle("Histogram of Items Rated\n per User (Jester)")
p1 <- p1 + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2))
p1 <- p1 + xlab("Number of Jokes Rated per User")  
p1 <- p1 + ylab("Number of Users") 
p1

p2 <- ggplot(data.frame(x=colMeans(jester_rrm)), aes(x))
p2 <- p2 + geom_histogram(binwidth=.25,colour="black", fill="white")
p2 <- p2 + ggtitle("Histogram of Average Rating\n per Item (Jester)")
p2 <- p2 + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2))
p2 <- p2 + xlab("Average Rating per Joke")  
p2 <- p2 + ylab("Number of Jokes") 
p2

p3 <- ggplot(data.frame(x=rowCounts(movies_rrm)), aes(x))
p3 <- p3 + geom_histogram(binwidth=5,colour="black", fill="white")
p3 <- p3 + ggtitle("Histogram of Items Rated\n per User (Movielens)")
p3 <- p3 + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2))
p3 <- p3 + xlab("Number of Movies Rated per User")  
p3 <- p3 + ylab("Number of Users") 
p3

p4 <- ggplot(data.frame(x=colMeans(movies_rrm)), aes(x))
p4 <- p4 + geom_histogram(binwidth=.25,colour="black", fill="white")
p4 <- p4 + ggtitle("Histogram of Average Rating\n per Item (Movielens)")
p4 <- p4 + theme(plot.title = element_text(lineheight=.8, face="bold", vjust=2))
p4 <- p4 + xlab("Average Rating per Movie")  
p4 <- p4 + ylab("Number of Movies") 
p4

#########################################
# More exploratory analysis
#########################################

(jester_average_items_rated_per_user <- mean(rowCounts(jester_rrm)))
(jester_average_item_rating <- mean(colMeans(jester_rrm), na.rm=T))
(movies_average_items_rated_per_user <- mean(rowCounts(movies_rrm)))
(movies_average_item_rating <- mean(colMeans(movies_rrm)))

#########################################
# Evaluating Binary Top-N Recommendations
#########################################

jester_bn <- binarize(jester_rrm, minRating=5)
jester_bn <- jester_bn[rowCounts(jester_bn)>10]
dim(jester_bn)

algorithms <- list(
     "Random" = list(name="RANDOM", param=NULL),
     "Popular" = list(name="POPULAR", param=NULL),
     "UserBasedCF_COS" = list(name="UBCF", param=list(method="Cosine", nn=50)),
     "UserBasedCF_JAC" = list(name="UBCF", param=list(method="Jaccard", nn=50))
)

jester_split_scheme <- evaluationScheme(jester_bn, method = "split", train = 0.8, given = 10, k=1)
jester_split_eval <- evaluate(jester_split_scheme, algorithms, n = 1:20)

plot(jester_split_eval, annotate=2, legend="topright")
title(main="FPR versus TPR For Binary Jester Data")

plot(jester_split_eval, "prec/rec", annotate=2, legend="bottomright")
title(main="Precision versus Recall For Binary Jester Data")

#########################################
# Movies
#########################################

normalized_algorithms <- list(
     "Random" = list(name="RANDOM", param=list(normalize = "Z-score")),
     "Popular" = list(name="POPULAR", param=list(normalize = "Z-score")),
     "UserBasedCF" = list(name="UBCF", param=list(normalize = "Z-score", method="Cosine", nn=50)),
     "ItemBasedCF" = list(name="IBCF", param=list(normalize = "Z-score")),
     "SVD" = list(name="SVD", param=list(categories=30, normalize="Z-score", treat_na="median"))
 )
movies_cross_scheme <- evaluationScheme(movies_rrm, method = "cross-validation", k = 10, given = 10, goodRating = 4)
movies_cross_eval <- evaluate(movies_cross_scheme, normalized_algorithms, n=1:20)

plot(movies_cross_eval, annotate=4, legend="topright")
title(main="FPR versus TPR For Movielens Data")

plot(movies_cross_eval, "prec/rec", annotate=3, legend="bottomright")
title(main="Precision versus Recall For Movielens Data")
