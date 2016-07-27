## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
library(pander)
set.seed(1)

## ----warning=FALSE, message=FALSE----------------------------------------
library("data.table")
library("ggplot2")
library("recommenderlab")
library("countrycode")

## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
# folder_data <- file.path("data", "microsoft")
folder_data <- file.path("..", "data", "microsoft")
file_train <- file.path(folder_data, "anonymous-msweb.data")
file_test <- file.path(folder_data, "anonymous-msweb.test.txt")
file_in <- file_test

## ----eval=FALSE----------------------------------------------------------
## file_in <- "anonymous-msweb.test.txt"

## ------------------------------------------------------------------------
table_in <- read.csv(file_in, header = FALSE)

## ----eval=FALSE----------------------------------------------------------
## head(table_in)

## ----echo=FALSE----------------------------------------------------------
head_table_in <- head(table_in)
pander(head_table_in)

## ------------------------------------------------------------------------
table_users <- table_in[, 1:2]

## ------------------------------------------------------------------------
table_users <- data.table(table_users)

## ------------------------------------------------------------------------
setnames(table_users, 1:2, c("category", "value"))
table_users <- table_users[category %in% c("C", "V")]

## ----eval=FALSE----------------------------------------------------------
## head(table_users)

## ----echo=FALSE----------------------------------------------------------
pander(head(table_users))

## ----results='hide'------------------------------------------------------
table_users[, chunk_user := cumsum(category == "C")]

## ----eval=FALSE----------------------------------------------------------
## head(table_users)

## ----echo=FALSE----------------------------------------------------------
pander(head(table_users))

## ------------------------------------------------------------------------
table_long <- table_users[, list(user = value[1], item = value[-1]),
                       by = "chunk_user"]

## ----eval=FALSE----------------------------------------------------------
## head(table_long)

## ----echo=FALSE----------------------------------------------------------
pander(head(table_long))

## ----results='hide'------------------------------------------------------
table_long[, value := 1]
table_wide <- reshape(data = table_long,
                      direction = "wide",
                      idvar = "user",
                      timevar = "item",
                      v.names = "value")

## ----eval=FALSE----------------------------------------------------------
## head(table_wide[, 1:5, with = FALSE])

## ----echo=FALSE----------------------------------------------------------
pander(head(table_wide[, 1:5, with = FALSE]))

## ----results='hide'------------------------------------------------------
vector_users <- table_wide[, user]
table_wide[, user := NULL]
table_wide[, chunk_user := NULL]

## ------------------------------------------------------------------------
setnames(x = table_wide,
         old = names(table_wide),
         new = substring(names(table_wide), 7))

## ------------------------------------------------------------------------
matrix_wide <- as.matrix(table_wide)
rownames(matrix_wide) <- vector_users

## ----eval=FALSE----------------------------------------------------------
## head(matrix_wide[, 1:6])

## ----echo=FALSE----------------------------------------------------------
pander(head(matrix_wide[, 1:6]))

## ------------------------------------------------------------------------
matrix_wide[is.na(matrix_wide)] <- 0
ratings_matrix <- as(matrix_wide, "binaryRatingMatrix")
ratings_matrix

## ------------------------------------------------------------------------
image(ratings_matrix[1:50, 1:50],
      main = "Binary rating matrix")

## ----warning=FALSE-------------------------------------------------------
n_users <- colCounts(ratings_matrix)
qplot(n_users) +
  stat_bin(binwidth = 100) +
  ggtitle("Distribution of the number of users")

## ----warning=FALSE-------------------------------------------------------
qplot(n_users[n_users < 100]) +
  stat_bin(binwidth = 10) +
  ggtitle("Distribution of the number of users")

## ------------------------------------------------------------------------
ratings_matrix <- ratings_matrix[, colCounts(ratings_matrix) >= 5]
ratings_matrix

## ------------------------------------------------------------------------
sum(rowCounts(ratings_matrix) == 0)

## ------------------------------------------------------------------------
ratings_matrix <- ratings_matrix[rowCounts(ratings_matrix) >= 5, ]
ratings_matrix

## ------------------------------------------------------------------------
table_in <- data.table(table_in)
table_items <- table_in[V1 == "A"]

## ----eval=FALSE----------------------------------------------------------
## head(table_items)

## ----echo=FALSE----------------------------------------------------------
head_table_items <- head(table_items)
setnames(head_table_items, 2, "__V2__")
head_table_items[[6]] <- NULL
pander(head_table_items)

## ------------------------------------------------------------------------
table_items <- table_items[, c(2, 4, 5), with = FALSE]
setnames(table_items, 1:3, c("id", "description", "url"))
table_items <- table_items[order(id)]

## ----eval=FALSE----------------------------------------------------------
## head(table_items)

## ----echo=FALSE----------------------------------------------------------
head_table_items <- head(table_items)
setnames(head_table_items, 1, "__id__")
setnames(head_table_items, 3, "__url__")
pander(head_table_items)

## ----results='hide'------------------------------------------------------
table_items[, category := "product"]

## ----results='hide'------------------------------------------------------
name_countries <- c(countrycode_data$country.name,
                    "Taiwan", "UK", "Russia", "Venezuela",
                    "Slovenija", "Caribbean", "Netherlands (Holland)",
                    "Europe", "Central America", "MS North Africa")
table_items[description %in% name_countries, category := "region"]

## ----results='hide'------------------------------------------------------
table_items[grepl("Region", description), category := "region"]

## ----eval=FALSE----------------------------------------------------------
## head(table_items)

## ----echo=FALSE----------------------------------------------------------
head_table_items <- head(table_items)
setnames(head_table_items, 1, "__V2__")
pander(head_table_items)

## ----eval=FALSE----------------------------------------------------------
## table_items[, list(n_items = .N), by = category]

## ----echo=FALSE----------------------------------------------------------
pander(table_items[, list(n_items = .N), by = category])

## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
library(pander)
set.seed(1)
library("data.table")
library("ggplot2")
library("recommenderlab")
library("countrycode")
load(file = "prepared.RData")

## ------------------------------------------------------------------------
which_train <- sample(x = c(TRUE, FALSE),
                      size = nrow(ratings_matrix),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
recc_data_train <- ratings_matrix[which_train, ]
recc_data_test <- ratings_matrix[!which_train, ]

## ------------------------------------------------------------------------
recc_model <- Recommender(data = recc_data_train,
                          method = "IBCF",
                          parameter = list(method = "Jaccard"))

## ------------------------------------------------------------------------
class(recc_model@model$sim)
dim(recc_model@model$sim)

## ------------------------------------------------------------------------
image(recc_model@model$sim)

## ------------------------------------------------------------------------
range(recc_model@model$sim)

## ------------------------------------------------------------------------
dist_ratings <- as(recc_model@model$sim, "matrix")

## ------------------------------------------------------------------------
dist_category <- table_items[, 1 - dist(category == "product")]
class(dist_category)

## ------------------------------------------------------------------------
dist_category <- as(dist_category, "matrix")

## ------------------------------------------------------------------------
dim(dist_category)
dim(dist_ratings)

## ------------------------------------------------------------------------
rownames(dist_category) <- table_items[, id]
colnames(dist_category) <- table_items[, id]

## ------------------------------------------------------------------------
vector_items <- rownames(dist_ratings)
dist_category <- dist_category[vector_items, vector_items]

## ------------------------------------------------------------------------
identical(dim(dist_category), dim(dist_ratings))
identical(rownames(dist_category), rownames(dist_ratings))
identical(colnames(dist_category), colnames(dist_ratings))

## ------------------------------------------------------------------------
image(dist_category)

## ------------------------------------------------------------------------
weight_category <- 0.25
dist_tot <- dist_category * weight_category +
  dist_ratings * (1 - weight_category)

## ------------------------------------------------------------------------
image(dist_tot)

## ------------------------------------------------------------------------
recc_model@model$sim <- as(dist_tot, "dgCMatrix")

## ------------------------------------------------------------------------
n_recommended <- 10
recc_predicted <- predict(object = recc_model, 
                          newdata = recc_data_test,
                          n = n_recommended)

## ----eval=FALSE----------------------------------------------------------
## head(recc_predicted@itemLabels)

## ----echo=FALSE----------------------------------------------------------
pander(head(recc_predicted@itemLabels))

## ------------------------------------------------------------------------
table_labels <- data.frame(id = recc_predicted@itemLabels)

## ------------------------------------------------------------------------
table_labels <- merge(table_labels, table_items,
                      by = "id", all.x = TRUE, all.y = FALSE,
                      sort = FALSE)

## ------------------------------------------------------------------------
descriptions <- as(table_labels$description, "character")

## ----eval=FALSE----------------------------------------------------------
## head(table_labels)

## ----echo=FALSE----------------------------------------------------------
head_table_labels <- head(table_labels)
setnames(head_table_labels, "id", "__id__")
pander(head_table_labels)

## ------------------------------------------------------------------------
recc_user_1 <- recc_predicted@items[[1]]
items_user_1 <- descriptions[recc_user_1]

## ----eval=FALSE----------------------------------------------------------
## head(items_user_1)

## ----echo=FALSE----------------------------------------------------------
pander(head(items_user_1))

## ------------------------------------------------------------------------

recc_matrix <- sapply(recc_predicted@items, function(x){
  recommended <- descriptions[x]
  c(recommended, rep("", n_recommended - length(recommended)))
})
dim(recc_matrix)

## ----eval=FALSE----------------------------------------------------------
## head(recc_matrix[, 1:3])

## ----echo=FALSE----------------------------------------------------------
pander(head(recc_matrix[, 1:3]))

## ------------------------------------------------------------------------
table_recomm_per_item <- table(recc_matrix)
recomm_per_item <- as(table_recomm_per_item, "numeric")

## ------------------------------------------------------------------------
bin_recomm_per_item <- cut(recomm_per_item,
                       breaks = c(0, 10, 20, 100,
                                  max(recomm_per_item)))

## ------------------------------------------------------------------------
qplot(bin_recomm_per_item) + ggtitle("Recommendations per item")

## ------------------------------------------------------------------------
recomm_per_item_sorted <- sort(table_recomm_per_item,
                               decreasing = TRUE)
recomm_per_item_top <- head(recomm_per_item_sorted, n = 4)

## ------------------------------------------------------------------------
table_top <- data.frame(
  name = names(recomm_per_item_top),
  n_recomm = recomm_per_item_top)

## ----eval=FALSE----------------------------------------------------------
## table_top

## ----echo=FALSE----------------------------------------------------------
pander(table_top)

## ----echo=FALSE, warning=FALSE, message=FALSE----------------------------
library(pander)
set.seed(1)
library("data.table")
library("ggplot2")
library("recommenderlab")
library("countrycode")
try({load(file = "chapter-5/prepared.RData")}, silent = TRUE)
try({load(file = "prepared.RData")}, silent = TRUE)

## ----eval=FALSE----------------------------------------------------------
## evaluateModel <- function (
##   # data inputs
##   ratings_matrix, # rating matrix
##   table_items, # item description table
##   # K-fold parameters
##   n_fold = 10, # number of folds
##   items_to_keep = 4, # number of items to keep in the test set
##   # model parameters
##   number_neighbors = 30, # number of nearest neighbors
##   weight_description = 0.2, # weight to the item description-based distance
##   items_to_recommend = 10 # number of items to recommend
## ){
##   # build and evaluate the model
## }

## ----eval=FALSE----------------------------------------------------------
## set.seed(1)
## eval_sets <- evaluationScheme(data = ratings_matrix,
##                               method = "cross-validation",
##                               k = n_fold,
##                               given = items_to_keep)

## ----eval=FALSE----------------------------------------------------------
## recc_model <- Recommender(data = getData(eval_sets, "train"),
##                           method = "IBCF",
##                           parameter = list(method = "Jaccard",
##                                            k = number_neighbors))

## ----eval=FALSE----------------------------------------------------------
## dist_ratings <- as(recc_model@model$sim, "matrix")
## vector_items <- rownames(dist_ratings)

## ----eval=FALSE----------------------------------------------------------
## dist_category <- table_items[, 1 - as.matrix(dist(category == "product"))]
## rownames(dist_category) <- table_items[, id]
## colnames(dist_category) <- table_items[, id]
## dist_category <- dist_category[vector_items, vector_items]

## ----eval=FALSE----------------------------------------------------------
## dist_tot <- dist_category * weight_description +
##   dist_ratings * (1 - weight_description)
## recc_model@model$sim <- as(dist_tot, "dgCMatrix")

## ----eval=FALSE----------------------------------------------------------
## eval_prediction <- predict(object = recc_model,
##                            newdata = getData(eval_sets, "known"),
##                            n = items_to_recommend,
##                            type = "topNList")

## ----eval=FALSE----------------------------------------------------------
## eval_accuracy <- calcPredictionAccuracy(
##   x = eval_prediction,
##   data = getData(eval_sets, "unknown"),
##   byUser = FALSE,
##   given = items_to_recommend)

## ----eval=FALSE----------------------------------------------------------
## return(eval_accuracy)

## ------------------------------------------------------------------------
model_evaluation <- evaluateModel(ratings_matrix = ratings_matrix,
                                  table_items = table_items)

## ----eval=FALSE----------------------------------------------------------
## model_evaluation

## ----echo=FALSE----------------------------------------------------------
model_evaluation[1:4] <- round(model_evaluation[1:4])
model_evaluation[5:8] <- paste0(
  round(model_evaluation[5:8] * 100),
  "%")
table_evaluation <- data.frame(
  names(model_evaluation),
  model_evaluation
)
rownames(table_evaluation) <- NULL
colnames(table_evaluation) <- c("__index__", "__value__")
pander(table_evaluation)

## ------------------------------------------------------------------------
nn_to_test <- seq(4, 80, by = 2)

## ------------------------------------------------------------------------
list_performance <- lapply(
  X = nn_to_test,
  FUN = function(nn){
    evaluateModel(ratings_matrix = ratings_matrix,
                  table_items = table_items,
                  number_neighbors = nn,
                  weight_description = 0)
  })

## ----eval=FALSE----------------------------------------------------------
## list_performance[[1]]

## ----echo=FALSE----------------------------------------------------------
table_to_display <- data.frame(
  name = names(list_performance[[1]]),
  value = list_performance[[1]]
)
rownames(table_to_display) <- NULL
pander(table_to_display)

## ----eval=FALSE----------------------------------------------------------
## sapply(list_performance, "[[", "precision")

## ----echo=FALSE----------------------------------------------------------
pander(sapply(list_performance, "[[", "precision"))

## ------------------------------------------------------------------------
table_performance <- data.table(
  nn = nn_to_test,
  precision = sapply(list_performance, "[[", "precision"),
  recall = sapply(list_performance, "[[", "recall")
)

## ----results='hide'------------------------------------------------------
weight_precision <- 0.5
table_performance[
  , performance := precision * weight_precision + recall * (1 - weight_precision)]

## ----eval=FALSE----------------------------------------------------------
## head(table_performance)

## ----echo=FALSE----------------------------------------------------------
pander(head(table_performance))

## ------------------------------------------------------------------------
convertIntoPercent <- function(x){
  paste0(round(x * 100), "%")
}

## ------------------------------------------------------------------------
qplot(table_performance[, nn], table_performance[, precision]) +
  geom_smooth() +
  scale_y_continuous(labels = convertIntoPercent)

## ------------------------------------------------------------------------
qplot(table_performance[, nn], table_performance[, recall]) +
  geom_smooth() +
  scale_y_continuous(labels = convertIntoPercent)

## ------------------------------------------------------------------------
qplot(table_performance[, nn], table_performance[, performance]) +
  geom_smooth() +
  scale_y_continuous(labels = convertIntoPercent)

## ------------------------------------------------------------------------
row_best <- which.max(table_performance$performance)
number_neighbors_opt <- table_performance[row_best, nn]
number_neighbors_opt

## ------------------------------------------------------------------------
wd_to_try <- seq(0, 1, by = 0.05)

## ------------------------------------------------------------------------
list_performance <- lapply(
  X = wd_to_try,
  FUN = function(wd){
    evaluateModel(ratings_matrix = ratings_matrix,
                  table_items = table_items,
                  number_neighbors = number_neighbors_opt,
                  weight_description = wd)
  })

## ----results='hide'------------------------------------------------------
table_performance <- data.table(
  wd = wd_to_try,
  precision = sapply(list_performance, "[[", "precision"),
  recall = sapply(list_performance, "[[", "recall")
)
table_performance[
  , performance := precision * weight_precision + recall * (1 - weight_precision)]

## ------------------------------------------------------------------------
qplot(table_performance[, wd], table_performance[, performance]) +
  geom_smooth() +
  scale_y_continuous(labels = convertIntoPercent)

