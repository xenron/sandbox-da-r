## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
library(pander)
set.seed(1)

## ----warning=FALSE, message=FALSE----------------------------------------
library(recommenderlab)
library(ggplot2)

## ----warning=FALSE, message=FALSE----------------------------------------
data(MovieLense)
ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,
                             colCounts(MovieLense) > 100]
ratings_movies

## ------------------------------------------------------------------------
percentage_training <- 0.8

## ----warning=FALSE, message=FALSE----------------------------------------
min(rowCounts(ratings_movies))

## ----warning=FALSE, message=FALSE----------------------------------------
items_to_keep <- 15

## ----warning=FALSE, message=FALSE----------------------------------------
rating_threshold <- 3

## ----warning=FALSE, message=FALSE----------------------------------------
n_eval <- 1

## ----warning=FALSE, message=FALSE----------------------------------------
eval_sets <- evaluationScheme(data = ratings_movies,
                              method = "split",
                              train = percentage_training,
                              given = items_to_keep,
                              goodRating = rating_threshold,
                              k = n_eval)
eval_sets

## ----warning=FALSE, message=FALSE----------------------------------------
getData(eval_sets, "train")

## ----warning=FALSE, message=FALSE----------------------------------------
nrow(getData(eval_sets, "train")) / nrow(ratings_movies)

## ----warning=FALSE, message=FALSE----------------------------------------
getData(eval_sets, "known")
getData(eval_sets, "unknown")

## ----warning=FALSE, message=FALSE----------------------------------------
nrow(getData(eval_sets, "known")) / nrow(ratings_movies)

## ----warning=FALSE, message=FALSE----------------------------------------
unique(rowCounts(getData(eval_sets, "known")))

## ----warning=FALSE, message=FALSE----------------------------------------
qplot(rowCounts(getData(eval_sets, "unknown"))) +
  geom_histogram(binwidth = 10) +
  ggtitle("unknown items by the users")

## ------------------------------------------------------------------------
percentage_training <- 0.8
items_to_keep <- 15
rating_threshold <- 3
n_eval <- 1

## ----warning=FALSE, message=FALSE----------------------------------------
eval_sets <- evaluationScheme(data = ratings_movies,
                              method = "bootstrap",
                              train = percentage_training,
                              given = items_to_keep,
                              goodRating = rating_threshold,
                              k = n_eval)

## ----warning=FALSE, message=FALSE----------------------------------------
nrow(getData(eval_sets, "train")) / nrow(ratings_movies)

## ----warning=FALSE, message=FALSE----------------------------------------
perc_test <- nrow(getData(eval_sets, "known")) / nrow(ratings_movies)
perc_test

## ----warning=FALSE, message=FALSE----------------------------------------
length(unique(eval_sets@runsTrain[[1]]))

## ----warning=FALSE, message=FALSE----------------------------------------
perc_train <- length(unique(eval_sets@runsTrain[[1]])) / nrow(ratings_movies)
perc_train + perc_test

## ----warning=FALSE, message=FALSE----------------------------------------
table_train <- table(eval_sets@runsTrain[[1]])
n_repetitions <- factor(as.vector(table_train))
qplot(n_repetitions) +
  ggtitle("Number of repetitions in the training set")

## ----warning=FALSE, message=FALSE----------------------------------------
n_fold <- 4
eval_sets <- evaluationScheme(data = ratings_movies,
                              method = "cross-validation",
                              k = n_fold,
                              given = items_to_keep,
                              goodRating = rating_threshold)

## ----warning=FALSE, message=FALSE----------------------------------------
size_sets <- sapply(eval_sets@runsTrain, length)
size_sets

## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
set.seed(1)
library(pander)
library(recommenderlab)
library(ggplot2)
data(MovieLense)
ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,
                             colCounts(MovieLense) > 100]

## ------------------------------------------------------------------------
n_fold <- 4
items_to_keep <- 15
rating_threshold <- 3
eval_sets <- evaluationScheme(data = ratings_movies,
                              method = "cross-validation",
                              k = n_fold,
                              given = items_to_keep,
                              goodRating = rating_threshold)

## ------------------------------------------------------------------------
model_to_evaluate <- "IBCF"
model_parameters <- NULL

## ------------------------------------------------------------------------
eval_recommender <- Recommender(data = getData(eval_sets, "train"),
                                method = model_to_evaluate,
                                parameter = model_parameters)

## ------------------------------------------------------------------------
items_to_recommend <- 10

## ------------------------------------------------------------------------
eval_prediction <- predict(object = eval_recommender,
                           newdata = getData(eval_sets, "known"),
                           n = items_to_recommend,
                           type = "ratings")
class(eval_prediction)

## ----warning=FALSE-------------------------------------------------------
qplot(rowCounts(eval_prediction)) +
  geom_histogram(binwidth = 10) +
  ggtitle("Distribution of movies per user")

## ------------------------------------------------------------------------
eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction,
  data = getData(eval_sets, "unknown"),
  byUser = TRUE)

## ----eval=FALSE----------------------------------------------------------
## head(eval_accuracy)

## ----echo=FALSE----------------------------------------------------------
pander(head(eval_accuracy))

## ------------------------------------------------------------------------
qplot(eval_accuracy[, "RMSE"]) +
  geom_histogram(binwidth = 0.1) +
  ggtitle("Distribution of the RMSE by user")

## ------------------------------------------------------------------------
eval_accuracy <- calcPredictionAccuracy(
  x = eval_prediction,
  data = getData(eval_sets, "unknown"),
  byUser = FALSE)
eval_accuracy

## ----message=FALSE-------------------------------------------------------
results <- evaluate(x = eval_sets,
                    method = model_to_evaluate,
                    n = seq(10, 100, 10))
class(results)

## ----eval=FALSE----------------------------------------------------------
## head(getConfusionMatrix(results)[[1]])

## ----echo=FALSE----------------------------------------------------------
pander(head(getConfusionMatrix(results)[[1]]))

## ------------------------------------------------------------------------
columns_to_sum <- c("TP", "FP", "FN", "TN")
indices_summed <- Reduce("+", getConfusionMatrix(results))[, columns_to_sum]

## ----eval=FALSE----------------------------------------------------------
## head(indices_summed)

## ----echo=FALSE----------------------------------------------------------
pander(head(indices_summed))

## ------------------------------------------------------------------------
plot(results,
     annotate = TRUE,
     main = "ROC curve")

## ------------------------------------------------------------------------
plot(results, "prec/rec",
     annotate = TRUE,
     main = "Precision-recall")

## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
library(pander)
set.seed(1)
library(recommenderlab)
library(ggplot2)
data(MovieLense)
ratings_movies <- MovieLense[rowCounts(MovieLense) > 50,
                             colCounts(MovieLense) > 100]

n_fold <- 4
items_to_keep <- 15
rating_threshold <- 3
eval_sets <- evaluationScheme(data = ratings_movies,
                              method = "cross-validation",
                              k = n_fold,
                              given = items_to_keep,
                              goodRating = rating_threshold)

## ----eval=FALSE----------------------------------------------------------
## list(name = "IBCF", param = list(k = 20))

## ------------------------------------------------------------------------
models_to_evaluate <- list(
  IBCF_cos = list(name = "IBCF", param = list(method = "cosine")),
  IBCF_cor = list(name = "IBCF", param = list(method = "pearson")),
  UBCF_cos = list(name = "UBCF", param = list(method = "cosine")),
  UBCF_cor = list(name = "UBCF", param = list(method = "pearson")),
  random = list(name = "RANDOM", param=NULL)
)

## ------------------------------------------------------------------------
n_recommendations <- c(1, 5, seq(10, 100, 10))

## ----message=FALSE, warning=FALSE----------------------------------------
list_results <- evaluate(x = eval_sets,
                    method = models_to_evaluate,
                    n = n_recommendations)
class(list_results)

## ------------------------------------------------------------------------
class(list_results[[1]])

## ------------------------------------------------------------------------
sapply(list_results, class) == "evaluationResults"

## ------------------------------------------------------------------------
avg_matrices <- lapply(list_results, avg)

## ----eval=FALSE----------------------------------------------------------
## head(avg_matrices$IBCF_cos[, 5:8])

## ----echo=FALSE----------------------------------------------------------
pander(head(avg_matrices$IBCF_cos)[, 5:8])

## ------------------------------------------------------------------------
plot(list_results,
     annotate = 1,
     legend = "topleft")
title("ROC curve")

## ------------------------------------------------------------------------
plot(list_results,
     "prec/rec",
     annotate = 1,
     legend = "bottomright")
title("Precision-recall")

## ------------------------------------------------------------------------
vector_k <- c(5, 10, 20, 30, 40)

## ------------------------------------------------------------------------
models_to_evaluate <- lapply(vector_k, function(k){
  list(name = "IBCF",
       param = list(method = "cosine",
                    k = k))
})
names(models_to_evaluate) <- paste0("IBCF_k_", vector_k)

## ------------------------------------------------------------------------
n_recommendations <- c(1, 5, seq(10, 100, 10))
list_results <- evaluate(x = eval_sets,
                         method = models_to_evaluate,
                         n = n_recommendations)

## ------------------------------------------------------------------------
plot(list_results,
     annotate = 1,
     legend = "topleft")
title("ROC curve")

## ------------------------------------------------------------------------
plot(list_results,
     "prec/rec",
     annotate = 1,
     legend = "bottomright")
title("Precision-recall")

