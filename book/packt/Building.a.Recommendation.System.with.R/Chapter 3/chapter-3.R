## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
library(pander)
set.seed(1)

## ----warning=FALSE, message=FALSE----------------------------------------
if(!"recommenderlab" %in% rownames(installed.packages())){
  install.packages("recommenderlab")
}

## ----warning=FALSE, message=FALSE----------------------------------------
library("recommenderlab")
help(package = "recommenderlab")

## ----results='asis'------------------------------------------------------
data_package <- data(package = "recommenderlab")

## ----eval=FALSE----------------------------------------------------------
## data_package$results[, "Item"]

## ----echo=FALSE----------------------------------------------------------
pander(data_package$results[, "Item"], nrow = 1)

## ------------------------------------------------------------------------
data(MovieLense)
MovieLense

## ------------------------------------------------------------------------
class(MovieLense)

## ----eval=FALSE----------------------------------------------------------
## methods(class = class(MovieLense))

## ----echo=FALSE----------------------------------------------------------
methods_matrix <- methods(class = class(MovieLense))
methods_to_print <- as.character(methods_matrix)
methods_to_print <- methods_to_print[!grepl("coerce", methods_to_print)]
methods_to_print <- gsub(",.*", "", methods_to_print, perl = TRUE)
methods_to_print <- c(methods_to_print, "", "")
pander(matrix(methods_to_print, ncol = 3))

## ------------------------------------------------------------------------
object.size(MovieLense)
object.size(as(MovieLense, "matrix"))

## ------------------------------------------------------------------------
object.size(as(MovieLense, "matrix")) / object.size(MovieLense)

## ------------------------------------------------------------------------
similarity_users <- similarity(MovieLense[1:4, ], method = "cosine", which = "users")

## ------------------------------------------------------------------------
class(similarity_users)

## ----eval=FALSE----------------------------------------------------------
## as.matrix(similarity_users)

## ----echo=FALSE----------------------------------------------------------
pander(as.matrix(similarity_users))

## ------------------------------------------------------------------------
image(as.matrix(similarity_users), main = "User similarity")

## ------------------------------------------------------------------------
similarity_items <- similarity(MovieLense[, 1:4], method = "cosine", which = "items")

## ----eval=FALSE----------------------------------------------------------
## as.matrix(similarity_items)

## ----echo=FALSE----------------------------------------------------------
pander(as.matrix(similarity_items))

## ------------------------------------------------------------------------
image(as.matrix(similarity_items), main = "Item similarity")

## ------------------------------------------------------------------------
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")

## ----eval=FALSE----------------------------------------------------------
## names(recommender_models)

## ----echo=FALSE----------------------------------------------------------
df_models <- data.frame(model = names(recommender_models))
pander(df_models)

## ------------------------------------------------------------------------
lapply(recommender_models, "[[", "description")

## ----eval=FALSE----------------------------------------------------------
## recommender_models$IBCF_realRatingMatrix$parameters

## ----echo=FALSE----------------------------------------------------------
df_parameters <- data.frame(
  parameter = names(recommender_models$IBCF_realRatingMatrix$parameters),
  default = unlist(recommender_models$IBCF_realRatingMatrix$parameters)
  )
rownames(df_parameters) <- NULL
pander(head(df_parameters))

## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
library(pander)
set.seed(1)

## ----warning=FALSE, message=FALSE----------------------------------------
library("recommenderlab")
library("ggplot2")
data(MovieLense)
class(MovieLense)

## ------------------------------------------------------------------------
dim(MovieLense)

## ------------------------------------------------------------------------
slotNames(MovieLense)

## ------------------------------------------------------------------------
class(MovieLense@data)
dim(MovieLense@data)

## ------------------------------------------------------------------------
vector_ratings <- as.vector(MovieLense@data)
unique(vector_ratings)

## ------------------------------------------------------------------------
table_ratings <- table(vector_ratings)

## ----eval=FALSE----------------------------------------------------------
## table_ratings

## ----echo=FALSE----------------------------------------------------------
df_ratings <- data.frame(
  rating = names(table_ratings),
  occurrences = as.vector(table_ratings)
  )
pander(head(df_ratings))

## ------------------------------------------------------------------------
vector_ratings <- vector_ratings[vector_ratings != 0]

## ------------------------------------------------------------------------
vector_ratings <- factor(vector_ratings)
qplot(vector_ratings) + ggtitle("Distribution of the ratings")

## ------------------------------------------------------------------------
views_per_movie <- colCounts(MovieLense)

## ------------------------------------------------------------------------
table_views <- data.frame(
  movie = names(views_per_movie),
  views = views_per_movie
  )
table_views <- table_views[order(table_views$views, decreasing = TRUE), ]

## ------------------------------------------------------------------------
ggplot(table_views[1:6, ], aes(x = movie, y = views)) +
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Number of views of the top movies")

## ------------------------------------------------------------------------
average_ratings <- colMeans(MovieLense)
qplot(average_ratings) +
  stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average movie rating")

## ------------------------------------------------------------------------
average_ratings_relevant <- average_ratings[views_per_movie > 100]
qplot(average_ratings_relevant) +
  stat_bin(binwidth = 0.1) +
  ggtitle(paste("Distribution of the relevant average ratings"))

## ------------------------------------------------------------------------
image(MovieLense, main = "Heatmap of the rating matrix")

## ------------------------------------------------------------------------
image(MovieLense[1:10, 1:15],
      main = "Heatmap of the first rows and columns")

## ------------------------------------------------------------------------
min_n_movies <- quantile(rowCounts(MovieLense), 0.99)
min_n_users <- quantile(colCounts(MovieLense), 0.99)
min_n_movies
min_n_users

## ------------------------------------------------------------------------
image(MovieLense[rowCounts(MovieLense) > min_n_movies,
                 colCounts(MovieLense) > min_n_users],
      main = "Heatmap of the top users and movies")

## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
library(pander)
set.seed(1)

## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
library("recommenderlab")
library("ggplot2")
data(MovieLense)

## ------------------------------------------------------------------------
ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,
                             colCounts(MovieLense) > 100]
ratings_movies

## ------------------------------------------------------------------------

# visualize the top matrix
min_movies <- quantile(rowCounts(ratings_movies), 0.98)
min_users <- quantile(colCounts(ratings_movies), 0.98)
image(ratings_movies[rowCounts(ratings_movies) > min_movies,
                     colCounts(ratings_movies) > min_users],
      main = "Heatmap of the top users and movies")


## ------------------------------------------------------------------------
average_ratings_per_user <- rowMeans(ratings_movies)
qplot(average_ratings_per_user) +
  stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average rating per user")

## ------------------------------------------------------------------------
ratings_movies_norm <- normalize(ratings_movies)

## ------------------------------------------------------------------------
sum(rowMeans(ratings_movies_norm) > 0.00001)

## ------------------------------------------------------------------------

# visualize the normalised matrix
image(ratings_movies_norm[rowCounts(ratings_movies_norm) > min_movies,
                          colCounts(ratings_movies_norm) > min_users],
      main = "Heatmap of the top users and movies")

## ------------------------------------------------------------------------
ratings_movies_watched <- binarize(ratings_movies, minRating = 1)

## ------------------------------------------------------------------------
min_movies_binary <- quantile(rowCounts(ratings_movies), 0.95)
min_users_binary <- quantile(colCounts(ratings_movies), 0.95)
image(ratings_movies_watched[rowCounts(ratings_movies) >
                               min_movies_binary,
                          colCounts(ratings_movies) >
                            min_users_binary],
      main = "Heatmap of the top users and movies")

## ------------------------------------------------------------------------
ratings_movies_good <- binarize(ratings_movies, minRating = 3)
image(ratings_movies_good[rowCounts(ratings_movies) >
                               min_movies_binary,
                          colCounts(ratings_movies) >
                            min_users_binary],
      main = "Heatmap of the top users and movies")

## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
library(pander)
set.seed(1)

## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
library("recommenderlab")
library("ggplot2")
library("knitr")
set.seed(1)
data(MovieLense)
ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,
                             colCounts(MovieLense) > 100]
ratings_movies_norm <- normalize(ratings_movies)

## ------------------------------------------------------------------------
which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(ratings_movies),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
head(which_train)

## ------------------------------------------------------------------------
recc_data_train <- ratings_movies[which_train, ]
recc_data_test <- ratings_movies[!which_train, ]

## ------------------------------------------------------------------------
which_set <- sample(x = 1:5,
                      size = nrow(ratings_movies),
                      replace = TRUE)
for(i_model in 1:5) {
  which_train <- which_set == i_model
  recc_data_train <- ratings_movies[which_train, ]
  recc_data_test <- ratings_movies[!which_train, ]
  # build the recommender
}

## ------------------------------------------------------------------------
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")

## ----eval=FALSE----------------------------------------------------------
## recommender_models$IBCF_realRatingMatrix$parameters

## ----echo=FALSE----------------------------------------------------------
list_parameters <- recommender_models$IBCF_realRatingMatrix$parameters
df_paramters <- data.frame(
  parameter = names(list_parameters),
  default = unlist(list_parameters)
  )
rownames(df_paramters) <- NULL
pander(df_paramters)

## ------------------------------------------------------------------------
recc_model <- Recommender(data = recc_data_train,
                          method = "IBCF",
                          parameter = list(k = 30))
recc_model
class(recc_model)

## ------------------------------------------------------------------------
model_details <- getModel(recc_model)
model_details$description
model_details$k

## ------------------------------------------------------------------------
class(model_details$sim)
dim(model_details$sim)

## ------------------------------------------------------------------------
n_items_top <- 20
image(model_details$sim[1:n_items_top, 1:n_items_top],
      main = "Heatmap of the first rows and columns")

## ------------------------------------------------------------------------
model_details$k
row_sums <- rowSums(model_details$sim > 0)
table(row_sums)

## ------------------------------------------------------------------------
col_sums <- colSums(model_details$sim > 0)
qplot(col_sums) +
  stat_bin(binwidth = 1) +
  ggtitle("Distribution of the column count")

## ------------------------------------------------------------------------
which_max <- order(col_sums, decreasing = TRUE)[1:6]

## ----eval=FALSE----------------------------------------------------------
## rownames(model_details$sim)[which_max]

## ----echo=FALSE----------------------------------------------------------
df_colsums <- data.frame(
  movie = rownames(model_details$sim)[which_max],
  col_sum = col_sums[which_max])
rownames(df_colsums) <- NULL
pander(df_colsums)

## ------------------------------------------------------------------------
n_recommended <- 6

## ------------------------------------------------------------------------
recc_predicted <- predict(object = recc_model, 
                          newdata = recc_data_test,
                          n = n_recommended)
recc_predicted

## ------------------------------------------------------------------------
class(recc_predicted)
slotNames(recc_predicted)

## ------------------------------------------------------------------------
recc_predicted@items[[1]]

## ------------------------------------------------------------------------
recc_user_1 <- recc_predicted@items[[1]]
movies_user_1 <- recc_predicted@itemLabels[recc_user_1]

## ----eval=FALSE----------------------------------------------------------
## movies_user_1

## ----echo=FALSE----------------------------------------------------------
table_user_1 <- data.frame(index = recc_user_1,
                           movie = movies_user_1)
pander(table_user_1)

## ------------------------------------------------------------------------
recc_matrix <- sapply(recc_predicted@items, function(x){
  colnames(ratings_movies)[x]
})
dim(recc_matrix)

## ----eval=FALSE----------------------------------------------------------
## recc_matrix[, 1:4]

## ----echo=FALSE----------------------------------------------------------
pander(recc_matrix[, 1:4])

## ------------------------------------------------------------------------
number_of_items <- factor(table(recc_matrix))
chart_title <- "Distribution of the number of items for IBCF"
qplot(number_of_items) + ggtitle(chart_title)

## ------------------------------------------------------------------------
number_of_items_sorted <- sort(number_of_items,
                               decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(
  names(number_of_items_top),
  number_of_items_top)

## ----eval=FALSE----------------------------------------------------------
## table_top

## ----echo=FALSE----------------------------------------------------------
pander(table_top)

## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
library(pander)
set.seed(1)

## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
library("recommenderlab")
library("ggplot2")
library("knitr")
set.seed(1)
data(MovieLense)
ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,
                             colCounts(MovieLense) > 100]
ratings_movies_norm <- normalize(ratings_movies)

## ----echo=FALSE----------------------------------------------------------
which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(ratings_movies),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
recc_data_train <- ratings_movies[which_train, ]
recc_data_test <- ratings_movies[!which_train, ]

## ------------------------------------------------------------------------
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")

## ----eval=FALSE----------------------------------------------------------
## recommender_models$UBCF_realRatingMatrix$parameters

## ----echo=FALSE----------------------------------------------------------
df_parameters <- data.frame(
  parameter = names(recommender_models$UBCF_realRatingMatrix$parameters),
  default = unlist(recommender_models$UBCF_realRatingMatrix$parameters)
  )
rownames(df_parameters) <- NULL
pander(head(df_parameters))

## ------------------------------------------------------------------------
recc_model <- Recommender(data = recc_data_train,
                          method = "UBCF")
recc_model

## ------------------------------------------------------------------------
model_details <- getModel(recc_model)

## ----eval=FALSE----------------------------------------------------------
## names(model_details)

## ----echo=FALSE----------------------------------------------------------
pander(data.frame(element = names(model_details)))

## ------------------------------------------------------------------------
model_details$data

## ------------------------------------------------------------------------
n_recommended <- 6
recc_predicted <- predict(object = recc_model, 
                          newdata = recc_data_test,
                          n = n_recommended)
recc_predicted

## ------------------------------------------------------------------------
recc_matrix <- sapply(recc_predicted@items, function(x){
  colnames(ratings_movies)[x]
})
dim(recc_matrix)

## ----eval=FALSE----------------------------------------------------------
## recc_matrix[, 1:4]

## ----echo=FALSE----------------------------------------------------------
pander(recc_matrix[, 1:4])

## ------------------------------------------------------------------------
number_of_items <- factor(table(recc_matrix))
chart_title <- "Distribution of the number of items for UBCF"
qplot(number_of_items) + ggtitle(chart_title)

## ------------------------------------------------------------------------
number_of_items_sorted <- sort(number_of_items, decreasing = TRUE)
number_of_items_top <- head(number_of_items_sorted, n = 4)
table_top <- data.frame(
  names(number_of_items_top),
  number_of_items_top)

## ----eval=FALSE----------------------------------------------------------
## table_top

## ----echo=FALSE----------------------------------------------------------
pander(table_top)

## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
library(pander)
set.seed(1)

## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
library("recommenderlab")
library("ggplot2")
library("knitr")
set.seed(1)
data(MovieLense)
ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,
                             colCounts(MovieLense) > 100]

## ------------------------------------------------------------------------
ratings_movies_watched <- binarize(ratings_movies, minRating = 1)

## ------------------------------------------------------------------------
qplot(rowSums(ratings_movies_watched)) +
  stat_bin(binwidth = 10) +
  geom_vline(xintercept = mean(rowSums(ratings_movies_watched)),
             col = "red", linetype = "dashed") +
  ggtitle("Distribution of movies by user")

## ------------------------------------------------------------------------
which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(ratings_movies),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
recc_data_train <- ratings_movies[which_train, ]
recc_data_test <- ratings_movies[!which_train, ]

## ------------------------------------------------------------------------
recc_model <- Recommender(data = recc_data_train,
                          method = "IBCF",
                          parameter = list(method = "Jaccard"))
model_details <- getModel(recc_model)

## ------------------------------------------------------------------------
n_recommended <- 6
recc_predicted <- predict(object = recc_model, 
                          newdata = recc_data_test,
                          n = n_recommended)
recc_matrix <- sapply(recc_predicted@items, function(x){
  colnames(ratings_movies)[x]
})

## ----eval=FALSE----------------------------------------------------------
## recc_matrix[, 1:4]

## ----echo=FALSE----------------------------------------------------------
pander(recc_matrix[, 1:4])

## ------------------------------------------------------------------------
recc_model <- Recommender(data = recc_data_train,
                          method = "UBCF",
                          parameter = list(method = "Jaccard"))

## ------------------------------------------------------------------------
n_recommended <- 6
recc_predicted <- predict(object = recc_model, 
                          newdata = recc_data_test,
                          n = n_recommended)
recc_matrix <- sapply(recc_predicted@items, function(x){
  colnames(ratings_movies)[x]
})
dim(recc_matrix)

## ----eval=FALSE----------------------------------------------------------
## recc_matrix[, 1:4]

## ----echo=FALSE----------------------------------------------------------
pander(recc_matrix[, 1:4])

